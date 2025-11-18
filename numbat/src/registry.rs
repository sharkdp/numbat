use std::{collections::HashMap, fmt::Display};

use compact_str::{CompactString, ToCompactString};
use itertools::Itertools;
use num_traits::Zero;
use thiserror::Error;

use crate::{
    arithmetic::{Exponent, Power, Rational, pretty_exponent},
    pretty_print::PrettyPrint,
    product::{Canonicalize, Product},
    suggestion,
};

#[derive(Clone, Error, Debug, PartialEq, Eq)]
pub enum RegistryError {
    #[error("Entry '{0}' exists already.")]
    EntryExists(String),

    #[error("Unknown entry '{0}'.")]
    UnknownEntry(String, Option<String>),
}

pub type Result<T> = std::result::Result<T, RegistryError>;

pub type BaseEntry = CompactString;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct BaseRepresentationFactor(pub BaseEntry, pub Exponent);

impl Display for BaseRepresentationFactor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.0, pretty_exponent(&self.1))
    }
}

impl Canonicalize for BaseRepresentationFactor {
    type MergeKey = BaseEntry;

    fn merge_key(&self) -> Self::MergeKey {
        self.0.clone() // TODO(minor): can cloning be prevented here?
    }

    fn merge(self, other: Self) -> Self {
        BaseRepresentationFactor(self.0, self.1 + other.1)
    }

    fn is_trivial(&self) -> bool {
        self.1 == Rational::zero()
    }
}

impl Power for BaseRepresentationFactor {
    fn power(self, e: Exponent) -> Self {
        let BaseRepresentationFactor(entry, exp) = self;
        BaseRepresentationFactor(entry, exp * e)
    }
}

// TODO(minor): this could be represented with a base index in the first tuple component instead of a cloned string
pub type BaseRepresentation = Product<BaseRepresentationFactor, true>;

impl Display for BaseRepresentation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.iter().count() == 0 {
            f.write_str("Scalar")
        } else {
            f.write_str(&self.as_string(|f| f.1, '×', '/', true))
        }
    }
}

impl PrettyPrint for BaseRepresentation {
    fn pretty_print(&self) -> crate::markup::Markup {
        if self.iter().count() == 0 {
            crate::markup::type_identifier("Scalar")
        } else {
            self.pretty_print_with(|f| f.1, '×', '/', true, None)
        }
    }
}

#[derive(Debug, Clone)]
pub struct Registry<Metadata> {
    base_entries: Vec<(CompactString, Metadata)>,
    derived_entries: HashMap<CompactString, (BaseRepresentation, Metadata)>,
}

impl<T> Default for Registry<T> {
    fn default() -> Self {
        Self {
            base_entries: vec![],
            derived_entries: HashMap::default(),
        }
    }
}

impl<Metadata: Clone> Registry<Metadata> {
    pub fn add_base_entry(&mut self, name: &str, metadata: Metadata) -> Result<()> {
        if self.contains(name) {
            return Err(RegistryError::EntryExists(name.to_owned()));
        }
        self.base_entries.push((name.to_compact_string(), metadata));

        Ok(())
    }

    pub fn get_derived_entry_names_for(
        &self,
        base_representation: &BaseRepresentation,
    ) -> Vec<CompactString> {
        self.derived_entries
            .iter()
            .filter(|(_, (br, _))| br == base_representation)
            .map(|(name, _)| name.to_compact_string())
            .sorted_unstable()
            .collect()
    }

    pub fn add_derived_entry(
        &mut self,
        name: &str,
        base_representation: BaseRepresentation,
        metadata: Metadata,
    ) -> Result<()> {
        if self.contains(name) {
            return Err(RegistryError::EntryExists(name.to_owned()));
        }

        self.derived_entries
            .insert(name.to_compact_string(), (base_representation, metadata));

        Ok(())
    }

    pub fn contains(&self, name: &str) -> bool {
        self.base_entries.iter().any(|(n, _)| n == name) || self.derived_entries.contains_key(name)
    }

    pub fn get_base_representation_for_name(
        &self,
        name: &str,
    ) -> Result<(BaseRepresentation, Metadata)> {
        if let Some(metadata) = self
            .base_entries
            .iter()
            .find(|(n, _)| n == name)
            .map(|(_, m)| m)
        {
            Ok((
                BaseRepresentation::from_factor(BaseRepresentationFactor(
                    name.to_compact_string(),
                    Rational::from_integer(1),
                )),
                metadata.clone(),
            ))
        } else {
            self.derived_entries
                .get(name)
                .ok_or_else(|| {
                    let suggestion = suggestion::did_you_mean(
                        self.base_entries
                            .iter()
                            .map(|(id, _)| id)
                            .chain(self.derived_entries.keys()),
                        name,
                    );
                    RegistryError::UnknownEntry(name.to_owned(), suggestion)
                })
                .cloned()
        }
    }

    pub fn iter_base_entries(&self) -> impl Iterator<Item = CompactString> + '_ {
        self.base_entries.iter().map(|(name, _)| name.clone())
    }

    pub fn iter_derived_entries(&self) -> impl Iterator<Item = CompactString> + '_ {
        self.derived_entries.keys().cloned()
    }

    pub fn is_base_dimension(&self, dimension_name: &str) -> bool {
        self.base_entries
            .iter()
            .any(|(name, _)| name == dimension_name)
    }
}
