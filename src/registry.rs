use std::{collections::HashMap, marker::PhantomData};

use thiserror::Error;

#[derive(Clone, Error, Debug, PartialEq)]
pub enum RegistryError {
    #[error("Entry '{0}' exists already.")]
    EntryExists(String),

    #[error("Unknown entry '{0}'.")]
    UnknownEntry(String),
}

pub type Result<T> = std::result::Result<T, RegistryError>;

type BaseEntry = String;
type Exponent = i32;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BaseIndex(isize);

#[derive(Debug, Clone, PartialEq)]
pub struct BaseRepresentation {
    // TODO: this could be represented with a base index in the first tuple component instead of a cloned string
    // TODO: make this non-public, provide more convenience methods
    pub components: Vec<(BaseEntry, Exponent)>,
}

impl BaseRepresentation {
    // TODO: provide an IntoIter interface
    pub fn from_components<'a>(components: &[(BaseEntry, Exponent)]) -> Self {
        Self {
            components: components.into_iter().cloned().collect(),
        }
    }

    pub fn scalar() -> BaseRepresentation {
        Self { components: vec![] }
    }

    pub fn invert(&self) -> BaseRepresentation {
        let components: Vec<_> = self
            .components
            .iter()
            .map(|(base, exponent)| (base.clone(), -exponent))
            .collect();
        BaseRepresentation::from_components(&components)
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
        BaseRepresentation::from_components(
            &self
                .components
                .iter()
                .map(|(name, inner_exponent)| (name.clone(), inner_exponent * exponent))
                .collect::<Vec<_>>(),
        )
    }
}

pub trait RegistryAdapter: Sized + Default {
    type DerivedExpression;
    type Metadata;
    type Parent;

    fn expression_to_base_representation(
        registry: &Registry<Self>,
        expr: &Self::DerivedExpression,
    ) -> Result<BaseRepresentation>;
}

#[derive(Debug)]
pub struct Registry<A: RegistryAdapter> {
    base_entries: Vec<(String, A::Metadata)>,
    derived_entries: HashMap<String, BaseRepresentation>,
    adapter: PhantomData<A>,
}

impl<A: RegistryAdapter> Default for Registry<A> {
    fn default() -> Self {
        Self {
            base_entries: vec![],
            derived_entries: HashMap::default(),
            adapter: PhantomData::<A>::default(),
        }
    }
}

impl<A: RegistryAdapter> Registry<A> {
    pub fn add_base_entry(&mut self, name: &str, metadata: A::Metadata) -> Result<()> {
        if self.is_base_entry(name) {
            return Err(RegistryError::EntryExists(name.to_owned()));
        }
        self.base_entries.push((name.to_owned(), metadata));

        Ok(())
    }

    pub fn is_base_entry(&self, name: &str) -> bool {
        self.base_entries.iter().any(|(n, _)| n == name)
    }

    pub fn base_entry_metadata(&self, name: &str) -> Result<&A::Metadata> {
        self.base_entries
            .iter()
            .find(|(n, _)| n == name)
            .map(|(_, ref m)| m)
            .ok_or_else(|| RegistryError::UnknownEntry(name.to_owned()))
    }

    pub fn add_derived_entry(
        &mut self,
        name: &str,
        expression: &A::DerivedExpression,
        parent: &A::Parent,
        metadata: A::Metadata,
    ) -> Result<()> {
        if self.derived_entries.contains_key(name) {
            return Err(RegistryError::EntryExists(name.to_owned()));
        }

        let base_representation = self.get_base_representation(&expression)?;

        let parent_base_representation: Vec<_> = base_representation
            .components
            .iter()
            .map(|(base_name, exp)| self.base_entry_metadata(base_name).unwrap())
            .collect();

        // TODO: verify that base_representation is compatible with metadata
        drop(parent);
        drop(metadata);

        self.derived_entries
            .insert(name.to_owned(), base_representation);

        Ok(())
    }

    pub fn get_base_representation(
        &self,
        expression: &A::DerivedExpression,
    ) -> Result<BaseRepresentation> {
        A::expression_to_base_representation(self, expression)
    }

    pub fn get_base_representation_for_name(&self, name: &str) -> Result<BaseRepresentation> {
        if self.is_base_entry(name) {
            Ok(BaseRepresentation::from_components(&[(name.to_owned(), 1)]))
        } else {
            self.derived_entries
                .get(name)
                .ok_or_else(|| RegistryError::UnknownEntry(name.to_owned()))
                .cloned()
        }
    }
}
