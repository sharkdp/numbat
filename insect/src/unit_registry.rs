use crate::registry::{BaseRepresentation, Registry, RegistryError};
use crate::typed_ast::{Expression, Type};

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

    pub fn add_base_unit(&mut self, name: &str, type_: Type) -> Result<()> {
        self.registry
            .add_base_entry(name, type_)
            .map_err(UnitRegistryError::RegistryError)
    }

    pub fn add_derived_unit(&mut self, name: &str, expression: &Expression) -> Result<()> {
        self.registry
            .add_derived_entry(name, expression.get_type())
            .map_err(UnitRegistryError::RegistryError)?;

        Ok(())
    }
}
