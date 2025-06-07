use compact_str::{CompactString, ToCompactString};

use crate::arithmetic::{Exponent, Power};
use crate::ast::{TypeExpression, TypeParameterBound};
use crate::registry::{BaseRepresentation, Registry, Result};
use crate::span::Span;
use crate::BaseRepresentationFactor;

#[derive(Default, Clone)]
pub struct DimensionRegistry {
    registry: Registry<()>,
    pub introduced_type_parameters: Vec<(Span, CompactString, Option<TypeParameterBound>)>,
}

impl DimensionRegistry {
    pub fn get_base_representation(
        &self,
        expression: &TypeExpression,
    ) -> Result<BaseRepresentation> {
        match expression {
            TypeExpression::Unity(_) => Ok(BaseRepresentation::unity()),
            TypeExpression::TypeIdentifier(_, name) => {
                if self
                    .introduced_type_parameters
                    .iter()
                    .any(|(_, n, _)| n == name)
                {
                    Ok(BaseRepresentation::from_factor(BaseRepresentationFactor(
                        name.to_compact_string(),
                        Exponent::from_integer(1),
                    )))
                } else {
                    self.registry
                        .get_base_representation_for_name(name)
                        .map(|r| r.0)
                }
            }
            TypeExpression::Multiply(_, lhs, rhs) => {
                let lhs = self.get_base_representation(lhs)?;
                let rhs = self.get_base_representation(rhs)?;

                Ok(lhs * rhs)
            }
            TypeExpression::Divide(_, lhs, rhs) => {
                let lhs = self.get_base_representation(lhs)?;
                let rhs = self.get_base_representation(rhs)?;

                Ok(lhs / rhs)
            }
            TypeExpression::Power(_, expr, _, outer_exponent) => {
                Ok(self.get_base_representation(expr)?.power(*outer_exponent))
            }
        }
    }

    pub fn get_base_representation_for_name(&self, name: &str) -> Result<BaseRepresentation> {
        self.registry
            .get_base_representation_for_name(name)
            .map(|t| t.0)
    }

    pub fn get_derived_entry_names_for(
        &self,
        base_representation: &BaseRepresentation,
    ) -> Vec<CompactString> {
        self.registry
            .get_derived_entry_names_for(base_representation)
    }

    pub fn add_base_dimension(&mut self, name: &str) -> Result<BaseRepresentation> {
        self.registry.add_base_entry(name, ())?;
        Ok(self
            .registry
            .get_base_representation_for_name(name)
            .map(|t| t.0)
            .unwrap())
    }

    pub fn add_derived_dimension(
        &mut self,
        name: &str,
        expression: &TypeExpression,
    ) -> Result<BaseRepresentation> {
        let base_representation = self.get_base_representation(expression)?;
        self.registry
            .add_derived_entry(name, base_representation, ())?;
        Ok(self
            .registry
            .get_base_representation_for_name(name)
            .map(|t| t.0)
            .unwrap())
    }

    pub fn contains(&self, dimension_name: &str) -> bool {
        self.registry.contains(dimension_name)
    }

    pub fn is_base_dimension(&self, dimension_name: &str) -> bool {
        self.registry.is_base_dimension(dimension_name)
    }
}

#[test]
fn basic() {
    use crate::arithmetic::Rational;
    use crate::parser::parse_dexpr;
    use crate::registry::BaseRepresentationFactor;

    let mut registry = DimensionRegistry::default();
    registry.add_base_dimension("Length").unwrap();
    registry.add_base_dimension("Time").unwrap();
    registry
        .add_derived_dimension("Velocity", &parse_dexpr("Length / Time"))
        .unwrap();
    registry
        .add_derived_dimension("Acceleration", &parse_dexpr("Length / Time^2"))
        .unwrap();

    registry.add_base_dimension("Mass").unwrap();
    registry
        .add_derived_dimension("Momentum", &parse_dexpr("Mass * Velocity"))
        .unwrap();
    registry
        .add_derived_dimension("Energy", &parse_dexpr("Momentum^2 / Mass"))
        .unwrap();

    assert_eq!(
        registry.get_base_representation(&parse_dexpr("Length")),
        Ok(BaseRepresentation::from_factor(BaseRepresentationFactor(
            "Length".into(),
            Rational::from_integer(1),
        )))
    );
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("Time")),
        Ok(BaseRepresentation::from_factor(BaseRepresentationFactor(
            "Time".into(),
            Rational::from_integer(1)
        )))
    );
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("Mass")),
        Ok(BaseRepresentation::from_factor(BaseRepresentationFactor(
            "Mass".into(),
            Rational::from_integer(1)
        )))
    );
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("Velocity")),
        Ok(BaseRepresentation::from_factors([
            BaseRepresentationFactor("Length".into(), Rational::from_integer(1)),
            BaseRepresentationFactor("Time".into(), Rational::from_integer(-1))
        ]))
    );
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("Acceleration")),
        Ok(BaseRepresentation::from_factors([
            BaseRepresentationFactor("Length".into(), Rational::from_integer(1)),
            BaseRepresentationFactor("Time".into(), Rational::from_integer(-2))
        ]))
    );
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("Momentum")),
        Ok(BaseRepresentation::from_factors([
            BaseRepresentationFactor("Length".into(), Rational::from_integer(1)),
            BaseRepresentationFactor("Mass".into(), Rational::from_integer(1)),
            BaseRepresentationFactor("Time".into(), Rational::from_integer(-1))
        ]))
    );
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("Energy")),
        Ok(BaseRepresentation::from_factors([
            BaseRepresentationFactor("Length".into(), Rational::from_integer(2)),
            BaseRepresentationFactor("Mass".into(), Rational::from_integer(1)),
            BaseRepresentationFactor("Time".into(), Rational::from_integer(-2))
        ]))
    );

    registry
        .add_derived_dimension("Momentum2", &parse_dexpr("Velocity * Mass"))
        .unwrap();
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("Momentum2")),
        Ok(BaseRepresentation::from_factors([
            BaseRepresentationFactor("Length".into(), Rational::from_integer(1)),
            BaseRepresentationFactor("Mass".into(), Rational::from_integer(1)),
            BaseRepresentationFactor("Time".into(), Rational::from_integer(-1))
        ]))
    );

    registry
        .add_derived_dimension("Energy2", &parse_dexpr("Mass * Velocity^2"))
        .unwrap();
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("Energy2")),
        Ok(BaseRepresentation::from_factors([
            BaseRepresentationFactor("Length".into(), Rational::from_integer(2)),
            BaseRepresentationFactor("Mass".into(), Rational::from_integer(1)),
            BaseRepresentationFactor("Time".into(), Rational::from_integer(-2))
        ]))
    );

    registry
        .add_derived_dimension("Velocity2", &parse_dexpr("Momentum / Mass"))
        .unwrap();
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("Velocity2")),
        Ok(BaseRepresentation::from_factors([
            BaseRepresentationFactor("Length".into(), Rational::from_integer(1)),
            BaseRepresentationFactor("Time".into(), Rational::from_integer(-1))
        ]))
    );
}

#[test]
fn fails_if_same_dimension_is_added_twice() {
    let mut registry = DimensionRegistry::default();
    assert!(registry.add_base_dimension("Length").is_ok());
    assert!(registry.add_base_dimension("Length").is_err());
}
