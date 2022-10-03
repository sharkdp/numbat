use crate::arithmetic::{Power, Rational};
use crate::registry::{BaseRepresentation, Registry, RegistryError};
use crate::typed_ast::{BinaryOperator, Expression, Type};

use num_traits::FromPrimitive;
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
    registry: Registry<Type>,
}

impl UnitRegistry {
    pub fn new() -> Self {
        Self {
            registry: Registry::<Type>::default(),
        }
    }

    pub fn get_base_representation(&self, expression: &Expression) -> Result<BaseRepresentation> {
        match expression {
            Expression::Scalar(_) => Ok(BaseRepresentation::unity()),
            Expression::Identifier(name, _type) => self.get_base_representation_for_name(name),
            Expression::Negate(expr, _type) => self.get_base_representation(expr),
            Expression::BinaryOperator(
                BinaryOperator::Add | BinaryOperator::Sub,
                _lhs,
                _rhs,
                _type,
            ) => {
                todo!()
                // TODO(minor): add and sub should probably not be allowed in unit definitions,
                // unless we want to allow for something like `unit year = 12 months + …`.
            }
            Expression::BinaryOperator(BinaryOperator::Mul, lhs, rhs, _type) => Ok(self
                .get_base_representation(lhs)?
                .multiply(self.get_base_representation(rhs)?)),
            Expression::BinaryOperator(BinaryOperator::Div, lhs, rhs, _type) => Ok(self
                .get_base_representation(lhs)?
                .divide(self.get_base_representation(rhs)?)),
            Expression::BinaryOperator(BinaryOperator::Power, lhs, rhs, _type) => {
                match rhs.as_ref() {
                    Expression::Scalar(n) => {
                        Ok(self
                            .get_base_representation(lhs)?
                            .power(Rational::from_f64(n.to_f64()).unwrap())) // TODO
                    }
                    _ => todo!("Return some error"),
                }
            }
            Expression::BinaryOperator(BinaryOperator::ConvertTo, _, _, _type) => todo!(),
            Expression::FunctionCall(_, _, _) => todo!(), // Not allowed here?
        }
    }

    pub fn get_base_representation_for_name(&self, name: &str) -> Result<BaseRepresentation> {
        self.registry
            .get_base_representation_for_name(name)
            .map_err(UnitRegistryError::RegistryError)
    }

    pub fn add_base_unit(&mut self, name: &str, type_: Type) -> Result<()> {
        self.registry
            .add_base_entry(name, type_)
            .map_err(UnitRegistryError::RegistryError)
    }

    pub fn add_derived_unit(&mut self, name: &str, expression: &Expression) -> Result<()> {
        let base_representation = self.get_base_representation(expression)?;

        self.registry
            .add_derived_entry(name, base_representation)
            .map_err(UnitRegistryError::RegistryError)?;

        Ok(())
    }
}
