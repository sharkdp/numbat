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
    pub is_abbreviation: bool,
    pub code_source_id: usize,
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

    /// Given a unit, find all registered unit names with the same physical dimension.
    /// This is used for simplification - finding simpler units to convert to.
    /// Base units are returned first (preferred), then derived units.
    /// Abbreviation units (marked with @abbreviation) are excluded.
    pub fn get_matching_unit_names(&self, unit: &Unit) -> impl Iterator<Item = CompactString> + '_ {
        // Get base representation of the input unit
        let (base_unit, _) = unit.to_base_unit_representation();

        // Convert to registry's BaseRepresentation format
        let base_repr = BaseRepresentation::from_factors(
            base_unit
                .iter()
                .map(|f| BaseRepresentationFactor(f.unit_id.name.clone(), f.exponent)),
        );

        // Check if the base representation matches a single base unit
        // (e.g., dpi·in has base repr "dot", which matches base unit "dot")
        // Base units are preferred, so yield them first.
        let base_unit_match = (base_repr.iter().count() == 1)
            .then(|| base_repr.iter().next())
            .flatten()
            .filter(|factor| factor.1 == num_rational::Ratio::from_integer(1))
            .filter(|factor| self.inner.is_base_unit(&factor.0))
            .map(|factor| factor.0.clone());

        // Then yield all derived units with the same base representation,
        // excluding abbreviation units which should not be used as simplification targets
        let derived_units = self
            .inner
            .get_derived_entry_names_for_filtered(&base_repr, |metadata| !metadata.is_abbreviation);

        base_unit_match.into_iter().chain(derived_units)
    }
}
