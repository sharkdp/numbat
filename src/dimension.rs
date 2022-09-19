use crate::arithmetic::Power;
use crate::ast::DimensionExpression;
use crate::registry::{BaseRepresentation, Registry, Result};

#[derive(Default, Clone)]
pub struct DimensionRegistry {
    registry: Registry<()>,
}

impl DimensionRegistry {
    pub fn get_base_representation(
        &self,
        expression: &DimensionExpression,
    ) -> Result<BaseRepresentation> {
        match expression {
            DimensionExpression::Dimension(name) => {
                self.registry.get_base_representation_for_name(name)
            }
            DimensionExpression::Multiply(lhs, rhs) => {
                let lhs = self.get_base_representation(lhs)?;
                let rhs = self.get_base_representation(rhs)?;

                Ok(lhs.multiply(rhs))
            }
            DimensionExpression::Divide(lhs, rhs) => {
                let lhs = self.get_base_representation(lhs)?;
                let rhs = self.get_base_representation(rhs)?;

                Ok(lhs.divide(rhs))
            }
            DimensionExpression::Power(expr, outer_exponent) => {
                Ok(self.get_base_representation(expr)?.power(*outer_exponent))
            }
        }
    }

    pub fn get_base_representation_for_name(&self, name: &str) -> Result<BaseRepresentation> {
        self.registry.get_base_representation_for_name(name)
    }

    pub fn add_base_dimension(&mut self, name: &str) -> Result<BaseRepresentation> {
        self.registry.add_base_entry(name, ())?;
        Ok(self
            .registry
            .get_base_representation_for_name(name)
            .unwrap())
    }

    pub fn add_derived_dimension(
        &mut self,
        name: &str,
        expression: &DimensionExpression,
    ) -> Result<BaseRepresentation> {
        let base_representation = self.get_base_representation(expression)?;
        self.registry.add_derived_entry(name, base_representation)?;
        Ok(self
            .registry
            .get_base_representation_for_name(name)
            .unwrap())
    }
}

#[cfg(test)]
pub fn parse_dexpr(input: &str) -> DimensionExpression {
    let tokens = crate::tokenizer::tokenize(input).expect("No tokenizer errors in tests");
    let mut parser = crate::parser::Parser::new(&tokens);
    let expr = parser
        .dimension_expression()
        .expect("No parser errors in tests");
    assert!(parser.is_at_end());
    expr
}

#[test]
fn basic() {
    use crate::registry::BaseRepresentationFactor;

    let mut registry = DimensionRegistry::default();
    registry.add_base_dimension("Length").unwrap();
    registry.add_base_dimension("Time").unwrap();
    registry
        .add_derived_dimension("Speed", &parse_dexpr("Length / Time"))
        .unwrap();
    registry
        .add_derived_dimension("Acceleration", &parse_dexpr("Length / Time^2"))
        .unwrap();

    registry.add_base_dimension("Mass").unwrap();
    registry
        .add_derived_dimension("Momentum", &parse_dexpr("Mass * Speed"))
        .unwrap();
    registry
        .add_derived_dimension("Energy", &parse_dexpr("Momentum^2 / Mass"))
        .unwrap();

    assert_eq!(
        registry.get_base_representation(&parse_dexpr("Length")),
        Ok(BaseRepresentation::from_factor(BaseRepresentationFactor(
            "Length".into(),
            1
        )))
    );
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("Time")),
        Ok(BaseRepresentation::from_factor(BaseRepresentationFactor(
            "Time".into(),
            1
        )))
    );
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("Mass")),
        Ok(BaseRepresentation::from_factor(BaseRepresentationFactor(
            "Mass".into(),
            1
        )))
    );
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("Speed")),
        Ok(BaseRepresentation::from_factors([
            BaseRepresentationFactor("Length".into(), 1),
            BaseRepresentationFactor("Time".into(), -1)
        ]))
    );
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("Acceleration")),
        Ok(BaseRepresentation::from_factors([
            BaseRepresentationFactor("Length".into(), 1),
            BaseRepresentationFactor("Time".into(), -2)
        ]))
    );
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("Momentum")),
        Ok(BaseRepresentation::from_factors([
            BaseRepresentationFactor("Length".into(), 1),
            BaseRepresentationFactor("Mass".into(), 1),
            BaseRepresentationFactor("Time".into(), -1)
        ]))
    );
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("Energy")),
        Ok(BaseRepresentation::from_factors([
            BaseRepresentationFactor("Length".into(), 2),
            BaseRepresentationFactor("Mass".into(), 1),
            BaseRepresentationFactor("Time".into(), -2)
        ]))
    );

    registry
        .add_derived_dimension("Momentum2", &parse_dexpr("Speed * Mass"))
        .unwrap();
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("Momentum2")),
        Ok(BaseRepresentation::from_factors([
            BaseRepresentationFactor("Length".into(), 1),
            BaseRepresentationFactor("Mass".into(), 1),
            BaseRepresentationFactor("Time".into(), -1)
        ]))
    );

    registry
        .add_derived_dimension("Energy2", &parse_dexpr("Mass * Speed^2"))
        .unwrap();
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("Energy2")),
        Ok(BaseRepresentation::from_factors([
            BaseRepresentationFactor("Length".into(), 2),
            BaseRepresentationFactor("Mass".into(), 1),
            BaseRepresentationFactor("Time".into(), -2)
        ]))
    );

    registry
        .add_derived_dimension("Speed2", &parse_dexpr("Momentum / Mass"))
        .unwrap();
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("Speed2")),
        Ok(BaseRepresentation::from_factors([
            BaseRepresentationFactor("Length".into(), 1),
            BaseRepresentationFactor("Time".into(), -1)
        ]))
    );
}

#[test]
fn fails_if_same_dimension_is_added_twice() {
    let mut registry = DimensionRegistry::default();
    assert!(registry.add_base_dimension("Length").is_ok());
    assert!(registry.add_base_dimension("Length").is_err());
}
