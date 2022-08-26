use crate::ast::DimensionExpression;
use crate::registry::{BaseRepresentation, Registry, RegistryAdapter, Result};

#[derive(Default)]
pub struct DimensionAdapter;

impl RegistryAdapter for DimensionAdapter {
    type DerivedExpression = DimensionExpression;
    type Metadata = ();

    fn expression_to_base_representation(
        registry: &Registry<Self>,
        expression: &Self::DerivedExpression,
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
                let rhs = registry.get_base_representation(rhs)?.invert();

                Ok(registry.merge_base_representations(&lhs, &rhs))
            }
            DimensionExpression::Power(expr, outer_exponent) => {
                let base = registry.get_base_representation(expr)?;
                Ok(BaseRepresentation::from_components(
                    &base
                        .components
                        .iter()
                        .map(|(name, exponent)| (name.clone(), exponent * outer_exponent))
                        .collect::<Vec<_>>(),
                ))
            }
        }
    }
}

pub type DimensionRegistry = Registry<DimensionAdapter>;

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
    registry.add_base_entry("length", ()).unwrap();
    registry.add_base_entry("time", ()).unwrap();
    registry
        .add_derived_entry("speed", &parse_dexpr("length / time"), ())
        .unwrap();
    registry
        .add_derived_entry("acceleration", &parse_dexpr("length / time^2"), ())
        .unwrap();

    registry.add_base_entry("mass", ()).unwrap();
    registry
        .add_derived_entry("momentum", &parse_dexpr("mass * speed"), ())
        .unwrap();
    registry
        .add_derived_entry("energy", &parse_dexpr("momentum^2 / mass"), ())
        .unwrap();

    assert_eq!(
        registry.get_base_representation(&parse_dexpr("length")),
        Ok(BaseRepresentation::from_components(&[("length".into(), 1)]))
    );
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("time")),
        Ok(BaseRepresentation::from_components(&[("time".into(), 1)]))
    );
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("mass")),
        Ok(BaseRepresentation::from_components(&[("mass".into(), 1)]))
    );
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("speed")),
        Ok(BaseRepresentation::from_components(&[
            ("length".into(), 1),
            ("time".into(), -1)
        ]))
    );
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("acceleration")),
        Ok(BaseRepresentation::from_components(&[
            ("length".into(), 1),
            ("time".into(), -2)
        ]))
    );
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("momentum")),
        Ok(BaseRepresentation::from_components(&[
            ("length".into(), 1),
            ("mass".into(), 1),
            ("time".into(), -1)
        ]))
    );
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("energy")),
        Ok(BaseRepresentation::from_components(&[
            ("length".into(), 2),
            ("mass".into(), 1),
            ("time".into(), -2)
        ]))
    );

    registry
        .add_derived_entry("momentum2", &parse_dexpr("speed * mass"), ())
        .unwrap();
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("momentum2")),
        Ok(BaseRepresentation::from_components(&[
            ("length".into(), 1),
            ("mass".into(), 1),
            ("time".into(), -1)
        ]))
    );

    registry
        .add_derived_entry("energy2", &parse_dexpr("mass * speed^2"), ())
        .unwrap();
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("energy2")),
        Ok(BaseRepresentation::from_components(&[
            ("length".into(), 2),
            ("mass".into(), 1),
            ("time".into(), -2)
        ]))
    );

    registry
        .add_derived_entry("speed2", &parse_dexpr("momentum / mass"), ())
        .unwrap();
    assert_eq!(
        registry.get_base_representation(&parse_dexpr("speed2")),
        Ok(BaseRepresentation::from_components(&[
            ("length".into(), 1),
            ("time".into(), -1)
        ]))
    );
}

#[test]
fn fails_if_same_dimension_is_added_twice() {
    let mut registry = DimensionRegistry::default();
    assert!(registry.add_base_entry("length", ()).is_ok());
    assert!(registry.add_base_entry("length", ()).is_err());
}
