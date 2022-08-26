use crate::ast::{BinaryOperator, DimensionExpression, Expression};
use crate::dimension;
use crate::parser::parse;
use crate::registry::{BaseRepresentation, Registry, RegistryAdapter, Result};

#[derive(Default)]
pub struct UnitAdapter;

impl RegistryAdapter for UnitAdapter {
    type DerivedExpression = Expression;

    // TODO: Optimization: do not store the unevaluated DimensionExpression here, but rather a direct link to the corresponding dimension (does that always exist?!)
    type Metadata = DimensionExpression;

    fn expression_to_base_representation(
        registry: &Registry<Self>,
        expression: &Expression,
    ) -> Result<BaseRepresentation> {
        match expression {
            Expression::Scalar(_) => Ok(BaseRepresentation::scalar()),
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
                    &Self::expression_to_base_representation(registry, rhs)?.invert(),
                )),
            Expression::BinaryOperator(BinaryOperator::ConvertTo, _, _) => todo!(),
        }
    }
}

pub type UnitRegistry = Registry<UnitAdapter>;

#[cfg(test)]
pub fn parse_expr(input: &str) -> Expression {
    let tokens = crate::tokenizer::tokenize(input).expect("No tokenizer errors in tests");
    let mut parser = crate::parser::Parser::new(&tokens);
    let expr = parser.expression().expect("No parser errors in tests");
    assert!(parser.is_at_end());
    expr
}

#[test]
fn basic() {
    use crate::dimension::{parse_dexpr, DimensionRegistry};

    let mut dimension_registry = DimensionRegistry::default();
    dimension_registry.add_base_entry("length", ());
    dimension_registry.add_base_entry("time", ());
    dimension_registry.add_base_entry("mass", ());
    dimension_registry
        .add_derived_entry("force", &parse_dexpr("mass * length / time^2"), ())
        .unwrap();
    dimension_registry
        .add_derived_entry("energy", &parse_dexpr("force * length"), ())
        .unwrap();

    let mut registry = UnitRegistry::default();
    registry
        .add_base_entry("meter", parse_dexpr("length"))
        .unwrap();
    registry
        .add_base_entry("second", parse_dexpr("time"))
        .unwrap();
    registry
        .add_base_entry("kilogram", parse_dexpr("mass"))
        .unwrap();

    registry
        .add_derived_entry(
            "newton",
            &parse_expr("kilogram * meter / (second * second)"),
            parse_dexpr("force"),
        )
        .unwrap();
    registry
        .add_derived_entry(
            "joule",
            &parse_expr("newton * meter"),
            parse_dexpr("energy"),
        )
        .unwrap();

    assert_eq!(
        registry.get_base_representation(&parse_expr("meter")),
        Ok(BaseRepresentation::from_components(&[("meter".into(), 1)]))
    );
    assert_eq!(
        registry.get_base_representation(&parse_expr("second")),
        Ok(BaseRepresentation::from_components(&[("second".into(), 1)]))
    );
    assert_eq!(
        registry.get_base_representation(&parse_expr("kilogram")),
        Ok(BaseRepresentation::from_components(&[(
            "kilogram".into(),
            1
        )]))
    );
    assert_eq!(
        registry.get_base_representation(&parse_expr("newton")),
        Ok(BaseRepresentation::from_components(&[
            ("kilogram".into(), 1),
            ("meter".into(), 1),
            ("second".into(), -2)
        ]))
    );
    assert_eq!(
        registry.get_base_representation(&parse_expr("joule")),
        Ok(BaseRepresentation::from_components(&[
            ("kilogram".into(), 1),
            ("meter".into(), 2),
            ("second".into(), -2)
        ]))
    );
}
