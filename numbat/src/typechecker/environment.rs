use crate::ast::TypeParameterBound;
use crate::span::Span;
use crate::type_variable::TypeVariable;
use crate::Type;

use super::substitutions::{ApplySubstitution, Substitution, SubstitutionError};
use super::type_scheme::TypeScheme;

use std::collections::HashMap;

type Identifier = String; // TODO ?

#[derive(Clone, Debug)]
pub struct FunctionSignature {
    pub definition_span: Span,
    pub type_parameters: Vec<(Span, String, Option<TypeParameterBound>)>,
    pub parameters: Vec<(Span, String)>,
    pub fn_type: TypeScheme,
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
    Normal(TypeScheme, Span),
    /// A function
    Function(FunctionSignature, FunctionMetadata),
    /// Identifiers that are defined by the language: `_` and `ans` (see LAST_RESULT_IDENTIFIERS)
    Predefined(TypeScheme),
}

impl IdentifierKind {
    fn get_type(&self) -> TypeScheme {
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
            .insert(i, IdentifierKind::Normal(TypeScheme::Concrete(type_), span));
    }

    pub fn add_scheme(&mut self, i: Identifier, scheme: TypeScheme, span: Span) {
        self.identifiers
            .insert(i, IdentifierKind::Normal(scheme, span));
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

    pub fn add_predefined(&mut self, v: Identifier, type_: TypeScheme) {
        self.identifiers
            .insert(v, IdentifierKind::Predefined(type_));
    }

    pub(crate) fn get_identifier_type(&self, v: &str) -> Option<TypeScheme> {
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

    pub(crate) fn generalize_types(&mut self, dtype_variables: &[TypeVariable]) {
        for (_, kind) in self.identifiers.iter_mut() {
            match kind {
                IdentifierKind::Normal(t, _) => {
                    t.generalize(dtype_variables);
                }
                IdentifierKind::Function(signature, _) => {
                    signature.fn_type.generalize(dtype_variables);
                }
                IdentifierKind::Predefined(t) => {
                    t.generalize(dtype_variables);
                }
            }
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
