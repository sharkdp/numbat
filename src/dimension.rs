use crate::ast::DimensionExpression;
use crate::registry::{BaseRepresentation, Registry, Result};

#[derive(Default)]
pub struct DimensionRegistry {
    registry: Registry<()>,
}

impl DimensionRegistry {
    pub fn new() -> DimensionRegistry {
        Self {
            registry: Registry::<()>::default(),
        }
    }

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

                Ok(lhs.multiply(&rhs))
            }
            DimensionExpression::Divide(lhs, rhs) => {
                let lhs = self.get_base_representation(lhs)?;
                let rhs = self.get_base_representation(rhs)?;

                Ok(lhs.divide(&rhs))
            }
            DimensionExpression::Power(expr, outer_exponent) => {
                Ok(self.get_base_representation(expr)?.power(*outer_exponent))
            }
        }
    }

    pub fn get_base_representation_for_name(&self, name: &str) -> Result<BaseRepresentation> {
        self.registry.get_base_representation_for_name(name)
    }

    pub fn add_base_dimension(&mut self, name: &str) -> Result<()> {
        self.registry.add_base_entry(name, ())
    }

    pub fn add_derived_dimension(
        &mut self,
        name: &str,
        expression: &DimensionExpression,
    ) -> Result<()> {
        let base_representation = self.get_base_representation(expression)?;
        self.registry.add_derived_entry(name, base_representation)
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
    let mut registry = DimensionRegistry::default();
    registry.add_base_dimension("length").unwrap();
    registry.add_base_dimension("time").unwrap();
    registry
        .add_derived_dimension("speed", &parse_dexpr("length / time"))
        .unwrap();
    registry
        .add_derived_dimension("acceleration", &parse_dexpr("length / time^2"))
        .unwrap();

    registry.add_base_dimension("mass").unwrap();
    registry
        .add_derived_dimension("momentum", &parse_dexpr("mass * speed"))
        .unwrap();
    registry
        .add_derived_dimension("energy", &parse_dexpr("momentum^2 / mass"))
        .unwrap();

    assert_eq!(
        registry.get_base_representation(&parse_dexpr("length")),
        Ok(BaseRepresentation::from_factors([("length".into(), 1)]))
    );
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("time")),
        Ok(BaseRepresentation::from_factors([("time".into(), 1)]))
    );
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("mass")),
        Ok(BaseRepresentation::from_factors([("mass".into(), 1)]))
    );
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("speed")),
        Ok(BaseRepresentation::from_factors([
            ("length".into(), 1),
            ("time".into(), -1)
        ]))
    );
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("acceleration")),
        Ok(BaseRepresentation::from_factors([
            ("length".into(), 1),
            ("time".into(), -2)
        ]))
    );
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("momentum")),
        Ok(BaseRepresentation::from_factors([
            ("length".into(), 1),
            ("mass".into(), 1),
            ("time".into(), -1)
        ]))
    );
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("energy")),
        Ok(BaseRepresentation::from_factors([
            ("length".into(), 2),
            ("mass".into(), 1),
            ("time".into(), -2)
        ]))
    );

    registry
        .add_derived_dimension("momentum2", &parse_dexpr("speed * mass"))
        .unwrap();
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("momentum2")),
        Ok(BaseRepresentation::from_factors([
            ("length".into(), 1),
            ("mass".into(), 1),
            ("time".into(), -1)
        ]))
    );

    registry
        .add_derived_dimension("energy2", &parse_dexpr("mass * speed^2"))
        .unwrap();
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("energy2")),
        Ok(BaseRepresentation::from_factors([
            ("length".into(), 2),
            ("mass".into(), 1),
            ("time".into(), -2)
        ]))
    );

    registry
        .add_derived_dimension("speed2", &parse_dexpr("momentum / mass"))
        .unwrap();
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("speed2")),
        Ok(BaseRepresentation::from_factors([
            ("length".into(), 1),
            ("time".into(), -1)
        ]))
    );
}

#[test]
fn fails_if_same_dimension_is_added_twice() {
    let mut registry = DimensionRegistry::default();
    assert!(registry.add_base_dimension("length").is_ok());
    assert!(registry.add_base_dimension("length").is_err());
}
