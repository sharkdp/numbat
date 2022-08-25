use crate::ast::DimensionExpression;
use crate::registry::{Adapter, BaseRepresentation, Registry, Result};

#[derive(Default)]
pub struct DimensionAdapter;

impl Adapter for DimensionAdapter {
    type Expression = DimensionExpression;

    fn expression_to_base_representation(
        registry: &Registry<Self>,
        expression: &Self::Expression,
    ) -> Result<BaseRepresentation> {
        match expression {
            DimensionExpression::Dimension(name) => registry.get_base_representation_for_name(name),
            DimensionExpression::Multiply(lhs, rhs) => {
                let lhs = registry.get_base_representation(lhs)?;
                let rhs = registry.get_base_representation(rhs)?;

                Ok(registry.merge_base_representations(&lhs, &rhs))
            }
            DimensionExpression::Divide(lhs, rhs) => {
                let lhs = registry.get_base_representation(lhs)?;
                let rhs = registry
                    .get_base_representation(rhs)?
                    .iter()
                    .map(|(base, exponent)| (base.clone(), -exponent))
                    .collect();

                Ok(registry.merge_base_representations(&lhs, &rhs))
            }
            DimensionExpression::Power(expr, outer_exponent) => {
                let base = registry.get_base_representation(expr)?;
                Ok(base
                    .iter()
                    .map(|(name, exponent)| (name.clone(), exponent * outer_exponent))
                    .collect())
            }
        }
    }
}

pub type DimensionRegistry = Registry<DimensionAdapter>;

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
    registry.add_base_entry("length").unwrap();
    registry.add_base_entry("time").unwrap();
    registry
        .add_derived_entry("speed", &parse("length / time"))
        .unwrap();
    registry
        .add_derived_entry("acceleration", &parse("length / time^2"))
        .unwrap();

    registry.add_base_entry("mass").unwrap();
    registry
        .add_derived_entry("momentum", &parse("mass * speed"))
        .unwrap();
    registry
        .add_derived_entry("energy", &parse("momentum^2 / mass"))
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
        .add_derived_entry("momentum2", &parse("speed * mass"))
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
        .add_derived_entry("energy2", &parse("mass * speed^2"))
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
        .add_derived_entry("speed2", &parse("momentum / mass"))
        .unwrap();
    assert_eq!(
        registry.get_base_representation(&parse("speed2")),
        Ok(vec![("length".into(), 1), ("time".into(), -1)])
    );
}

#[test]
fn fails_if_same_dimension_is_added_twice() {
    let mut registry = DimensionRegistry::default();
    assert!(registry.add_base_entry("length").is_ok());
    assert!(registry.add_base_entry("length").is_err());
}
