use crate::markup::Markup;
use crate::prefix_parser::AcceptsPrefix;
use crate::registry::{BaseRepresentation, BaseRepresentationFactor, Registry, RegistryError};
use crate::typed_ast::Type;
use crate::unit::{CanonicalName, Unit};

use compact_str::CompactString;
use thiserror::Error;

#[derive(Clone, Error, Debug, PartialEq, Eq)]
pub enum UnitRegistryError {
    #[error("{0}")]
    RegistryError(RegistryError),
}

pub type Result<T> = std::result::Result<T, UnitRegistryError>;

#[derive(Debug, Clone)]
pub struct UnitMetadata {
    pub type_: Type,
    pub readable_type: Markup,
    pub aliases: Vec<(CompactString, AcceptsPrefix)>,
    pub name: Option<CompactString>,
    pub canonical_name: CanonicalName,
    pub url: Option<CompactString>,
    pub description: Option<CompactString>,
    pub binary_prefixes: bool,
    pub metric_prefixes: bool,
}

#[derive(Clone)]
pub struct UnitRegistry {
    pub inner: Registry<UnitMetadata>,
}

impl UnitRegistry {
    pub fn new() -> Self {
        Self {
            inner: Registry::<UnitMetadata>::default(),
        }
    }

    pub fn add_base_unit(&mut self, name: &str, metadata: UnitMetadata) -> Result<()> {
        self.inner
            .add_base_entry(name, metadata)
            .map_err(UnitRegistryError::RegistryError)
    }

    pub fn add_derived_unit(
        &mut self,
        name: &str,
        base_representation: &Unit,
        metadata: UnitMetadata,
    ) -> Result<()> {
        let base_representation_factors = base_representation
            .iter()
            .map(|factor| BaseRepresentationFactor(factor.unit_id.name.clone(), factor.exponent));
        let base_representation = BaseRepresentation::from_factors(base_representation_factors);
        self.inner
            .add_derived_entry(name, base_representation, metadata)
            .map_err(UnitRegistryError::RegistryError)?;

        Ok(())
    }
}
