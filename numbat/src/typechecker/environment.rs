use crate::span::Span;
use crate::Type;

use super::substitutions::{ApplySubstitution, Substitution, SubstitutionError};
use super::type_scheme::TypeScheme;

use std::collections::HashMap;

type Identifier = String; // TODO ?

#[derive(Clone, Debug)]
pub struct FunctionSignature {
    pub definition_span: Span,
    pub type_parameters: Vec<(Span, String)>,
    pub parameters: Vec<(Span, String)>,
    pub fn_type: Type,
}

#[derive(Clone, Debug)]
pub struct FunctionMetadata {
    pub name: Option<String>,
    pub url: Option<String>,
    pub description: Option<String>,
}

#[derive(Clone, Debug)]
pub enum IdentifierKind {
    /// A normal identifier (variable, unit) with the place where it has been defined
    Normal(Type, Span),
    /// A function
    Function(FunctionSignature, FunctionMetadata),
    /// Identifiers that are defined by the language: `_` and `ans` (see LAST_RESULT_IDENTIFIERS)
    Predefined(Type),
}

impl IdentifierKind {
    fn get_type(&self) -> Type {
        match self {
            IdentifierKind::Predefined(t) => t.clone(),
            IdentifierKind::Normal(t, _) => t.clone(),
            IdentifierKind::Function(s, _) => s.fn_type.clone(),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Environment {
    identifiers: HashMap<Identifier, IdentifierKind>,
}

impl Environment {
    pub fn add(&mut self, i: Identifier, type_: Type, span: Span) {
        self.identifiers
            .insert(i, IdentifierKind::Normal(type_, span));
    }

    pub(crate) fn add_function(
        &mut self,
        v: String,
        signature: FunctionSignature,
        metadata: FunctionMetadata,
    ) {
        self.identifiers
            .insert(v, IdentifierKind::Function(signature, metadata));
    }

    pub fn add_predefined(&mut self, v: Identifier, type_: Type) {
        self.identifiers
            .insert(v, IdentifierKind::Predefined(type_));
    }

    pub(crate) fn get_identifier_type(&self, v: &str) -> Option<Type> {
        self.identifiers.get(v).map(|k| k.get_type())
    }

    pub(crate) fn iter_identifiers(&self) -> impl Iterator<Item = &Identifier> {
        self.identifiers.keys()
    }

    pub(crate) fn get_function_info(
        &self,
        name: &str,
    ) -> Option<(&FunctionSignature, &FunctionMetadata)> {
        match self.identifiers.get(name) {
            Some(IdentifierKind::Function(signature, metadata)) => Some((signature, &metadata)),
            _ => None,
        }
    }
}

impl ApplySubstitution for Environment {
    fn apply(&mut self, substitution: &Substitution) -> Result<(), SubstitutionError> {
        for (_, kind) in self.identifiers.iter_mut() {
            match kind {
                IdentifierKind::Normal(t, _) => {
                    t.apply(substitution)?;
                }
                IdentifierKind::Function(signature, _) => {
                    signature.fn_type.apply(substitution)?;
                }
                IdentifierKind::Predefined(t) => {
                    t.apply(substitution)?;
                }
            }
        }
        Ok(())
    }
}
