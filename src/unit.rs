use crate::ast::{BinaryOperator, Expression};
use crate::registry::{
    negate_base_representation, BaseRepresentation, Registry, RegistryAdapter, Result,
};

#[derive(Default)]
pub struct UnitAdapter;

impl RegistryAdapter for UnitAdapter {
    type DerivedExpression = Expression;

    fn expression_to_base_representation(
        registry: &Registry<Self>,
        expression: &Expression,
    ) -> Result<BaseRepresentation> {
        match expression {
            Expression::Scalar(_) => Ok(vec![]),
            Expression::Identifier(name) => registry.get_base_representation_for_name(name),
            Expression::Negate(expr) => Self::expression_to_base_representation(registry, expr),
            Expression::BinaryOperator(BinaryOperator::Add | BinaryOperator::Sub, lhs, _) => {
                Self::expression_to_base_representation(registry, lhs)
            }
            Expression::BinaryOperator(BinaryOperator::Mul, lhs, rhs) => Ok(registry
                .merge_base_representations(
                    &Self::expression_to_base_representation(registry, lhs)?,
                    &Self::expression_to_base_representation(registry, rhs)?,
                )),
            Expression::BinaryOperator(BinaryOperator::Div, lhs, rhs) => Ok(registry
                .merge_base_representations(
                    &Self::expression_to_base_representation(registry, lhs)?,
                    &negate_base_representation(&Self::expression_to_base_representation(
                        registry, rhs,
                    )?),
                )),
            Expression::BinaryOperator(BinaryOperator::ConvertTo, _, _) => todo!(),
        }
    }
}

pub type UnitRegistry = Registry<UnitAdapter>;

#[cfg(test)]
pub fn parse(input: &str) -> Expression {
    let tokens = crate::tokenizer::tokenize(input).expect("No tokenizer errors in tests");
    let mut parser = crate::parser::Parser::new(&tokens);
    let expr = parser.expression().expect("No parser errors in tests");
    assert!(parser.is_at_end());
    expr
}

#[test]
fn basic() {
    let mut registry = UnitRegistry::default();
    registry.add_base_entry("meter").unwrap();
    registry.add_base_entry("second").unwrap();

    registry.add_base_entry("kilogram").unwrap();
    registry
        .add_derived_entry("newton", &parse("kilogram * meter / (second * second)"))
        .unwrap();
    registry
        .add_derived_entry("joule", &parse("newton * meter"))
        .unwrap();

    assert_eq!(
        registry.get_base_representation(&parse("meter")),
        Ok(vec![("meter".into(), 1)])
    );
    assert_eq!(
        registry.get_base_representation(&parse("second")),
        Ok(vec![("second".into(), 1)])
    );
    assert_eq!(
        registry.get_base_representation(&parse("kilogram")),
        Ok(vec![("kilogram".into(), 1)])
    );
    assert_eq!(
        registry.get_base_representation(&parse("newton")),
        Ok(vec![
            ("kilogram".into(), 1),
            ("meter".into(), 1),
            ("second".into(), -2)
        ])
    );
    assert_eq!(
        registry.get_base_representation(&parse("joule")),
        Ok(vec![
            ("kilogram".into(), 1),
            ("meter".into(), 2),
            ("second".into(), -2)
        ])
    );
}
