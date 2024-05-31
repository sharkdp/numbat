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
    pub parameter_types: Vec<(Span, String, Type)>,
    pub return_type: Type,
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
            IdentifierKind::Function(f, _) => Type::Fn(
                f.parameter_types
                    .iter()
                    .map(|(_, _, t)| t.clone())
                    .collect(),
                Box::new(f.return_type.clone()),
            ),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Environment {
    identifiers: HashMap<Identifier, IdentifierKind>,
}

impl Environment {
    pub fn add(&mut self, v: Identifier, type_: Type, span: Span) {
        self.identifiers
            .insert(v, IdentifierKind::Normal(type_, span));
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

// impl ApplySubstitution for Environment {
//     fn apply(&mut self, substitution: &Substitution) -> Result<(), SubstitutionError> {
//         for (_, t) in &mut self.0 {
//             t.apply(substitution)?;
//         }
//         Ok(())
//     }
// }
