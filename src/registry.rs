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
// TODO: this could be represented with a base index in the first tuple component instead of a cloned string
pub type BaseRepresentation = Vec<(BaseEntry, Exponent)>;

// TODO: turn BaseRepresentation into a proper type (strong type or proper struct) and add this as a method
pub fn negate_base_representation(rep: &BaseRepresentation) -> BaseRepresentation {
    rep.iter()
        .map(|(base, exponent)| (base.clone(), -exponent))
        .collect()
}

pub trait RegistryAdapter: Sized + Default {
    type DerivedExpression;

    fn expression_to_base_representation(
        registry: &Registry<Self>,
        expr: &Self::DerivedExpression,
    ) -> Result<BaseRepresentation>;
}

#[derive(Debug, Default)]
pub struct Registry<A: RegistryAdapter> {
    base_entries: Vec<String>,
    derived_entries: HashMap<String, BaseRepresentation>,
    adapter: PhantomData<A>,
}

impl<A: RegistryAdapter> Registry<A> {
    pub fn add_base_entry(&mut self, name: &str) -> Result<()> {
        if self.is_base_entry(name) {
            return Err(RegistryError::EntryExists(name.to_owned()));
        }
        self.base_entries.push(name.to_owned());

        Ok(())
    }

    pub fn is_base_entry(&self, name: &str) -> bool {
        self.base_entries.iter().any(|n| n == name)
    }

    pub fn add_derived_entry(
        &mut self,
        name: &str,
        expression: &A::DerivedExpression,
    ) -> Result<()> {
        if self.derived_entries.contains_key(name) {
            return Err(RegistryError::EntryExists(name.to_owned()));
        }

        self.derived_entries
            .insert(name.to_owned(), self.get_base_representation(&expression)?);

        Ok(())
    }

    pub fn get_base_representation(
        &self,
        expression: &A::DerivedExpression,
    ) -> Result<BaseRepresentation> {
        A::expression_to_base_representation(self, expression)
    }

    pub fn merge_base_representations(
        &self,
        lhs: &BaseRepresentation,
        rhs: &BaseRepresentation,
    ) -> BaseRepresentation {
        let mut result = lhs.clone();
        for (name_rhs, exponent_rhs) in rhs {
            if let Some((_, ref mut exponent_lhs)) =
                result.iter_mut().find(|(name_lhs, _)| name_lhs == name_rhs)
            {
                *exponent_lhs += *exponent_rhs;
            } else {
                result.push((name_rhs.clone(), *exponent_rhs))
            }
        }
        result.retain(|(_, exponent)| *exponent != 0);
        result.sort();
        result
    }

    pub fn get_base_representation_for_name(&self, name: &str) -> Result<BaseRepresentation> {
        if self.is_base_entry(name) {
            Ok(vec![(name.to_owned(), 1)])
        } else {
            self.derived_entries
                .get(name)
                .ok_or_else(|| RegistryError::UnknownEntry(name.to_owned()))
                .cloned()
        }
    }
}
