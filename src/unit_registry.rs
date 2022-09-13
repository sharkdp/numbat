use crate::arithmetic::Power;
use crate::ast::{BinaryOperator, DimensionExpression, Expression};
use crate::dimension::DimensionRegistry;
use crate::registry::{BaseRepresentation, BaseRepresentationFactor, Registry, RegistryError};

use thiserror::Error;

#[derive(Clone, Error, Debug, PartialEq, Eq)]
pub enum UnitRegistryError {
    #[error("{0}")]
    RegistryError(RegistryError),

    #[error(
        "Unexpected dimension in definition of unit '{0}'. Specified: '{1}', computed: '{2}'."
    )]
    IncompatibleDimension(String, BaseRepresentation, BaseRepresentation),
}

pub type Result<T> = std::result::Result<T, UnitRegistryError>;

pub struct UnitRegistry {
    // TODO(minor): Optimization: do not store the unevaluated DimensionExpression here, but rather a direct link to the corresponding dimension (does that always exist?!)
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
            Expression::Scalar(_) => Ok(BaseRepresentation::unity()),
            Expression::Identifier(name) => self.get_base_representation_for_name(name),
            Expression::Negate(expr) => self.get_base_representation(expr),
            Expression::BinaryOperator(BinaryOperator::Add | BinaryOperator::Sub, _lhs, _rhs) => {
                todo!()
                // TODO(minor): add and sub should probably not be allowed in unit definitions,
                // unless we want to allow for something like `unit year = 12 months + …`.
            }
            Expression::BinaryOperator(BinaryOperator::Mul, lhs, rhs) => Ok(self
                .get_base_representation(lhs)?
                .multiply(self.get_base_representation(rhs)?)),
            Expression::BinaryOperator(BinaryOperator::Div, lhs, rhs) => Ok(self
                .get_base_representation(lhs)?
                .divide(self.get_base_representation(rhs)?)),
            Expression::BinaryOperator(BinaryOperator::Power, lhs, rhs) => match rhs.as_ref() {
                Expression::Scalar(n) => {
                    Ok(self.get_base_representation(lhs)?.power(n.to_f64() as i32))
                }
                _ => todo!("Return some error"),
            },
            Expression::BinaryOperator(BinaryOperator::ConvertTo, _, _) => todo!(),
        }
    }

    pub fn get_base_representation_for_name(&self, name: &str) -> Result<BaseRepresentation> {
        self.registry
            .get_base_representation_for_name(name)
            .map_err(UnitRegistryError::RegistryError)
    }

    pub fn add_base_unit(&mut self, name: &str, dexpr: DimensionExpression) -> Result<()> {
        self.registry
            .add_base_entry(name, dexpr)
            .map_err(UnitRegistryError::RegistryError)
    }

    pub fn add_derived_unit(
        &mut self,
        name: &str,
        expression: &Expression,
        dimension_registry: &DimensionRegistry,
        dexpr: Option<&DimensionExpression>,
    ) -> Result<()> {
        let base_representation = self.get_base_representation(expression)?;

        if let Some(dexpr) = dexpr {
            let components =
                base_representation
                    .iter()
                    .flat_map(|BaseRepresentationFactor(base_name, exp)| {
                        let dimension = self.registry.base_entry_metadata(base_name).unwrap(); // TODO(minor): remove unwrap

                        dimension_registry
                            .get_base_representation(dimension)
                            .unwrap()
                            .power(*exp)
                    });
            let dimension_base_representation_computed =
                BaseRepresentation::from_factors(components);

            let dimension_base_representation_specified = dimension_registry
                .get_base_representation(dexpr)
                .map_err(UnitRegistryError::RegistryError)?;

            if dimension_base_representation_specified != dimension_base_representation_computed {
                return Err(UnitRegistryError::IncompatibleDimension(
                    name.to_owned(),
                    dimension_base_representation_specified,
                    dimension_base_representation_computed,
                ));
            }
        }

        self.registry
            .add_derived_entry(name, base_representation)
            .map_err(UnitRegistryError::RegistryError)?;

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
            Some(&parse_dexpr("force")),
        )
        .unwrap();
    registry
        .add_derived_unit(
            "joule",
            &parse_expr("newton * meter"),
            &dimension_registry,
            Some(&parse_dexpr("energy")),
        )
        .unwrap();

    assert_eq!(
        registry.get_base_representation(&parse_expr("meter")),
        Ok(BaseRepresentation::from_factor(BaseRepresentationFactor(
            "meter".into(),
            1
        )))
    );
    assert_eq!(
        registry.get_base_representation(&parse_expr("second")),
        Ok(BaseRepresentation::from_factor(BaseRepresentationFactor(
            "second".into(),
            1
        )))
    );
    assert_eq!(
        registry.get_base_representation(&parse_expr("kilogram")),
        Ok(BaseRepresentation::from_factor(BaseRepresentationFactor(
            "kilogram".into(),
            1
        )))
    );
    assert_eq!(
        registry.get_base_representation(&parse_expr("newton")),
        Ok(BaseRepresentation::from_factors([
            BaseRepresentationFactor("kilogram".into(), 1),
            BaseRepresentationFactor("meter".into(), 1),
            BaseRepresentationFactor("second".into(), -2)
        ]))
    );
    assert_eq!(
        registry.get_base_representation(&parse_expr("joule")),
        Ok(BaseRepresentation::from_factors([
            BaseRepresentationFactor("kilogram".into(), 1),
            BaseRepresentationFactor("meter".into(), 2),
            BaseRepresentationFactor("second".into(), -2)
        ]))
    );

    assert!(registry
        .add_derived_unit(
            "joule2",
            &parse_expr("newton * meter"),
            &dimension_registry,
            Some(&parse_dexpr("force")),
        )
        .is_err())
}
