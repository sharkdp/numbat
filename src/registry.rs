use std::{collections::HashMap, fmt::Display};

use thiserror::Error;

use crate::arithmetic::Exponent;

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BaseRepresentation {
    // TODO: this could be represented with a base index in the first tuple component instead of a cloned string
    components: Vec<(BaseEntry, Exponent)>,
}

impl BaseRepresentation {
    pub fn from_factors(components: impl IntoIterator<Item = (BaseEntry, Exponent)>) -> Self {
        let mut components: Vec<_> = components.into_iter().collect();
        components.sort();
        Self { components }
    }

    pub fn unity() -> BaseRepresentation {
        Self { components: vec![] }
    }

    pub fn invert(&self) -> BaseRepresentation {
        BaseRepresentation::from_factors(
            self.components
                .iter()
                .map(|(base, exponent)| (base.clone(), -exponent)),
        )
    }

    pub fn multiply(&self, rhs: &BaseRepresentation) -> BaseRepresentation {
        let mut result = self.clone();
        for (name_rhs, exponent_rhs) in &rhs.components {
            if let Some((_, ref mut exponent_lhs)) = result
                .components
                .iter_mut()
                .find(|(name_lhs, _)| name_lhs == name_rhs)
            {
                *exponent_lhs += exponent_rhs;
            } else {
                result.components.push((name_rhs.clone(), *exponent_rhs))
            }
        }
        result.components.retain(|(_, exponent)| *exponent != 0);
        result.components.sort();
        result
    }

    pub fn divide(&self, rhs: &BaseRepresentation) -> BaseRepresentation {
        self.multiply(&rhs.invert())
    }

    pub fn power(&self, exponent: Exponent) -> BaseRepresentation {
        BaseRepresentation::from_factors(
            self.components
                .iter()
                .map(|(name, inner_exponent)| (name.clone(), inner_exponent * exponent)),
        )
    }
}

impl Display for BaseRepresentation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (name, exp) in &self.components {
            write!(f, "{}^({}) ", name, exp)?;
        }
        Ok(())
    }
}

// TODO: Implement Iterator instead of IntoIterator. This allow us to remove some .clone() calls
impl IntoIterator for BaseRepresentation {
    type Item = (BaseEntry, Exponent);
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.components.into_iter()
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

    pub fn base_entry_metadata(&self, name: &str) -> Result<&Metadata> {
        self.base_entries
            .iter()
            .find(|(n, _)| n == name)
            .map(|(_, ref m)| m)
            .ok_or_else(|| RegistryError::UnknownEntry(name.to_owned()))
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
            Ok(BaseRepresentation::from_factors([(name.to_owned(), 1)]))
        } else {
            self.derived_entries
                .get(name)
                .ok_or_else(|| RegistryError::UnknownEntry(name.to_owned()))
                .cloned()
        }
    }
}
