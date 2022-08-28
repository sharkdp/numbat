use crate::ast::{BinaryOperator, DimensionExpression, Expression};
use crate::dimension::DimensionRegistry;
use crate::registry::{BaseRepresentation, Exponent, Registry, Result};

pub struct UnitRegistry {
    // TODO: Optimization: do not store the unevaluated DimensionExpression here, but rather a direct link to the corresponding dimension (does that always exist?!)
    registry: Registry<DimensionExpression>,
}

impl UnitRegistry {
    pub fn new() -> Self {
        Self {
            registry: Registry::<DimensionExpression>::default(),
        }
    }

    pub fn get_base_representation(&self, expression: &Expression) -> Result<BaseRepresentation> {
        match expression {
            Expression::Scalar(_) => Ok(BaseRepresentation::scalar()),
            Expression::Identifier(name) => self.get_base_representation_for_name(name),
            Expression::Negate(expr) => self.get_base_representation(expr),
            Expression::BinaryOperator(BinaryOperator::Add | BinaryOperator::Sub, lhs, _) => {
                self.get_base_representation(lhs)
            }
            Expression::BinaryOperator(BinaryOperator::Mul, lhs, rhs) => Ok(self
                .get_base_representation(lhs)?
                .multiply(&self.get_base_representation(rhs)?)),
            Expression::BinaryOperator(BinaryOperator::Div, lhs, rhs) => Ok(self
                .get_base_representation(lhs)?
                .divide(&self.get_base_representation(rhs)?)),
            Expression::BinaryOperator(BinaryOperator::ConvertTo, _, _) => todo!(),
        }
    }

    pub fn get_base_representation_for_name(&self, name: &str) -> Result<BaseRepresentation> {
        self.registry.get_base_representation_for_name(name)
    }

    pub fn add_base_unit(&mut self, name: &str, dexpr: DimensionExpression) -> Result<()> {
        self.registry.add_base_entry(name, dexpr)
    }

    pub fn add_derived_unit(
        &mut self,
        name: &str,
        expression: &Expression,
        dimension_registry: &DimensionRegistry,
        dexpr: &DimensionExpression,
    ) -> Result<()> {
        let base_representation = self.get_base_representation(&expression)?;

        // let components: Vec<(, i32)> = base_representation
        //     .components
        //     .iter()
        //     .flat_map(|(base_name, exp)| dimension_registry.get_base_representation_for_name(base_name).unwrap().power(*exp).components)
        //     .collect();
        // let dimension_base_representation_expected = BaseRepresentation::from_components(components);

        // let dimension_base_representation_actual = dimension_registry.get_base_representation(dexpr)?;

        // if dimension_base_representation_actual != dimension_base_representation_expected {
        //     assert!(false);
        // }

        self.registry.add_derived_entry(name, base_representation)?;

        Ok(())
    }
}

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

    let mut dimension_registry = DimensionRegistry::new();
    dimension_registry.add_base_dimension("length").unwrap();
    dimension_registry.add_base_dimension("time").unwrap();
    dimension_registry.add_base_dimension("mass").unwrap();
    dimension_registry
        .add_derived_dimension("force", &parse_dexpr("mass * length / time^2"))
        .unwrap();
    dimension_registry
        .add_derived_dimension("energy", &parse_dexpr("force * length"))
        .unwrap();

    let mut registry = UnitRegistry::new();
    registry
        .add_base_unit("meter", parse_dexpr("length"))
        .unwrap();
    registry
        .add_base_unit("second", parse_dexpr("time"))
        .unwrap();
    registry
        .add_base_unit("kilogram", parse_dexpr("mass"))
        .unwrap();

    registry
        .add_derived_unit(
            "newton",
            &parse_expr("kilogram * meter / (second * second)"),
            &dimension_registry,
            &parse_dexpr("force"),
        )
        .unwrap();
    registry
        .add_derived_unit(
            "joule",
            &parse_expr("newton * meter"),
            &dimension_registry,
            &parse_dexpr("energy"),
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
