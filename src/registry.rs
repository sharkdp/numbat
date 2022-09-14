use std::{collections::HashMap, fmt::Display};

use thiserror::Error;

use crate::{
    arithmetic::{Exponent, Power},
    product::{Canonicalize, Product},
};

#[derive(Clone, Error, Debug, PartialEq, Eq)]
pub enum RegistryError {
    #[error("Entry '{0}' exists already.")]
    EntryExists(String),

    #[error("Unknown entry '{0}'.")]
    UnknownEntry(String),
}

pub type Result<T> = std::result::Result<T, RegistryError>;

pub type BaseEntry = String;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BaseIndex(isize);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct BaseRepresentationFactor(pub BaseEntry, pub Exponent);

impl Canonicalize for BaseRepresentationFactor {
    type MergeKey = BaseEntry;

    fn merge_key(&self) -> Self::MergeKey {
        self.0.clone() // TODO(minor): can cloning be prevented here?
    }

    fn merge(self, other: Self) -> Self {
        BaseRepresentationFactor(self.0, self.1 + other.1)
    }

    fn is_trivial(&self) -> bool {
        self.1 == 0
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
        for BaseRepresentationFactor(name, exp) in self.iter() {
            write!(f, "{}^({}) ", name, exp)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Registry<Metadata> {
    base_entries: Vec<(String, Metadata)>,
    derived_entries: HashMap<String, BaseRepresentation>,
}

impl<T> Default for Registry<T> {
    fn default() -> Self {
        Self {
            base_entries: vec![],
            derived_entries: HashMap::default(),
        }
    }
}

impl<Metadata> Registry<Metadata> {
    pub fn add_base_entry(&mut self, name: &str, metadata: Metadata) -> Result<()> {
        if self.is_base_entry(name) {
            return Err(RegistryError::EntryExists(name.to_owned()));
        }
        self.base_entries.push((name.to_owned(), metadata));

        Ok(())
    }

    pub fn is_base_entry(&self, name: &str) -> bool {
        self.base_entries.iter().any(|(n, _)| n == name)
    }

    pub fn add_derived_entry(
        &mut self,
        name: &str,
        base_representation: BaseRepresentation,
    ) -> Result<()> {
        if self.derived_entries.contains_key(name) {
            return Err(RegistryError::EntryExists(name.to_owned()));
        }

        self.derived_entries
            .insert(name.to_owned(), base_representation);

        Ok(())
    }

    pub fn get_base_representation_for_name(&self, name: &str) -> Result<BaseRepresentation> {
        if self.is_base_entry(name) {
            Ok(BaseRepresentation::from_factor(BaseRepresentationFactor(
                name.to_owned(),
                1,
            )))
        } else {
            self.derived_entries
                .get(name)
                .ok_or_else(|| RegistryError::UnknownEntry(name.to_owned()))
                .cloned()
        }
    }
}
