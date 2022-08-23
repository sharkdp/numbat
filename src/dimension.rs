use std::collections::HashMap;

use crate::ast::DimensionExpression;
use thiserror::Error;

#[derive(Clone, Error, Debug, PartialEq)]
pub enum DimensionRegistryError {
    #[error("Dimension '{0}' exists already.")]
    DimensionExists(String),

    #[error("Unknown dimension '{0}'.")]
    UnknownDimension(String),
}

type Result<T> = std::result::Result<T, DimensionRegistryError>;

type BaseDimension = String;
type Exponent = i32;
// TODO: this could be represented with a base dimension index in the first tuple component instead of a cloned string
type BaseRepresentation = Vec<(BaseDimension, Exponent)>;

#[derive(Debug, Default)]
pub struct DimensionRegistry {
    base_dimensions: Vec<String>,
    derived_dimensions: HashMap<String, BaseRepresentation>,
}

impl DimensionRegistry {
    pub fn add_base_dimension(&mut self, name: &str) -> Result<()> {
        if self.is_base_dimension(name) {
            return Err(DimensionRegistryError::DimensionExists(name.to_owned()));
        }
        self.base_dimensions.push(name.to_owned());

        Ok(())
    }

    fn is_base_dimension(&self, name: &str) -> bool {
        self.base_dimensions.iter().any(|n| n == name)
    }

    pub fn add_derived_dimension(
        &mut self,
        name: &str,
        expression: &DimensionExpression,
    ) -> Result<()> {
        if self.derived_dimensions.contains_key(name) {
            return Err(DimensionRegistryError::DimensionExists(name.to_owned()));
        }

        let derived_dimension = self.get_base_representation(expression)?;

        self.derived_dimensions
            .insert(name.to_owned(), derived_dimension);

        Ok(())
    }

    pub fn get_base_representation(
        &self,
        expression: &DimensionExpression,
    ) -> Result<BaseRepresentation> {
        match expression {
            DimensionExpression::Dimension(name) => self.get_base_representation_for_name(name),
            DimensionExpression::Multiply(lhs, rhs) => {
                let lhs = self.get_base_representation(lhs)?;
                let rhs = self.get_base_representation(rhs)?;

                Ok(self.merge_base_representations(&lhs, &rhs))
            }
            DimensionExpression::Divide(lhs, rhs) => {
                let lhs = self.get_base_representation(lhs)?;
                let rhs = self
                    .get_base_representation(rhs)?
                    .iter()
                    .map(|(base, exponent)| (base.clone(), -exponent))
                    .collect();

                Ok(self.merge_base_representations(&lhs, &rhs))
            }
            DimensionExpression::Power(expr, outer_exponent) => {
                let base = self.get_base_representation(expr)?;
                Ok(base
                    .iter()
                    .map(|(name, exponent)| (name.clone(), exponent * outer_exponent))
                    .collect())
            }
        }
    }

    fn merge_base_representations(
        &self,
        lhs: &BaseRepresentation,
        rhs: &BaseRepresentation,
    ) -> BaseRepresentation {
        let mut result = lhs.clone();
        for (name_rhs, exponent_rhs) in rhs {
            if let Some((_, ref mut exponent_lhs)) =
                result.iter_mut().find(|(name_lhs, _)| name_lhs == name_rhs)
            {
                *exponent_lhs += *exponent_rhs;
            } else {
                result.push((name_rhs.clone(), *exponent_rhs))
            }
        }
        result.retain(|(_, exponent)| *exponent != 0);
        result.sort();
        result
    }

    fn get_base_representation_for_name(&self, name: &str) -> Result<BaseRepresentation> {
        if self.is_base_dimension(name) {
            Ok(vec![(name.to_owned(), 1)])
        } else {
            self.derived_dimensions
                .get(name)
                .ok_or_else(|| DimensionRegistryError::UnknownDimension(name.to_owned()))
                .cloned()
        }
    }
}

#[cfg(test)]
pub fn parse(input: &str) -> DimensionExpression {
    let tokens = crate::tokenizer::tokenize(input).expect("No tokenizer errors in tests");
    let mut parser = crate::parser::Parser::new(&tokens);
    parser
        .dimension_expression()
        .expect("No parser errors in tests")
}

#[test]
fn basic() {
    let mut registry = DimensionRegistry::default();
    registry.add_base_dimension("length").unwrap();
    registry.add_base_dimension("time").unwrap();
    registry
        .add_derived_dimension("speed", &parse("length / time"))
        .unwrap();
    registry
        .add_derived_dimension("acceleration", &parse("length / time^2"))
        .unwrap();

    registry.add_base_dimension("mass").unwrap();
    registry
        .add_derived_dimension("momentum", &parse("mass * speed"))
        .unwrap();
    registry
        .add_derived_dimension("energy", &parse("momentum^2 / mass"))
        .unwrap();

    assert_eq!(
        registry.get_base_representation(&parse("length")),
        Ok(vec![("length".into(), 1)])
    );
    assert_eq!(
        registry.get_base_representation(&parse("time")),
        Ok(vec![("time".into(), 1)])
    );
    assert_eq!(
        registry.get_base_representation(&parse("mass")),
        Ok(vec![("mass".into(), 1)])
    );
    assert_eq!(
        registry.get_base_representation(&parse("speed")),
        Ok(vec![("length".into(), 1), ("time".into(), -1)])
    );
    assert_eq!(
        registry.get_base_representation(&parse("acceleration")),
        Ok(vec![("length".into(), 1), ("time".into(), -2)])
    );
    assert_eq!(
        registry.get_base_representation(&parse("momentum")),
        Ok(vec![
            ("length".into(), 1),
            ("mass".into(), 1),
            ("time".into(), -1)
        ])
    );
    assert_eq!(
        registry.get_base_representation(&parse("energy")),
        Ok(vec![
            ("length".into(), 2),
            ("mass".into(), 1),
            ("time".into(), -2)
        ])
    );

    registry
        .add_derived_dimension("momentum2", &parse("speed * mass"))
        .unwrap();
    assert_eq!(
        registry.get_base_representation(&parse("momentum2")),
        Ok(vec![
            ("length".into(), 1),
            ("mass".into(), 1),
            ("time".into(), -1)
        ])
    );

    registry
        .add_derived_dimension("energy2", &parse("mass * speed^2"))
        .unwrap();
    assert_eq!(
        registry.get_base_representation(&parse("energy2")),
        Ok(vec![
            ("length".into(), 2),
            ("mass".into(), 1),
            ("time".into(), -2)
        ])
    );

    registry
        .add_derived_dimension("speed2", &parse("momentum / mass"))
        .unwrap();
    assert_eq!(
        registry.get_base_representation(&parse("speed2")),
        Ok(vec![("length".into(), 1), ("time".into(), -1)])
    );
}

#[test]
fn fails_if_same_dimension_is_added_twice() {
    let mut registry = DimensionRegistry::default();
    assert!(registry.add_base_dimension("length").is_ok());
    assert!(registry.add_base_dimension("length").is_err());
}
