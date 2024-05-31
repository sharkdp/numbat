use crate::span::Span;
use crate::Type;

use super::substitutions::{ApplySubstitution, Substitution, SubstitutionError};
use super::type_scheme::TypeScheme;

use std::collections::HashMap;

type Identifier = String; // TODO ?

#[derive(Clone)]
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

#[derive(Clone)]
pub struct Environment(HashMap<Identifier, TypeScheme>);

impl Environment {
    // pub fn new() -> Environment {
    //     Environment(HashMap::new())
    // }

    // pub fn add(&mut self, v: Identifier, t: TypeScheme) {
    //     self.0.insert(v, t);
    // }

    // pub(crate) fn lookup(&self, v: &Identifier) -> Option<&TypeScheme> {
    //     self.0.get(v)
    // }
}

impl ApplySubstitution for Environment {
    fn apply(&mut self, substitution: &Substitution) -> Result<(), SubstitutionError> {
        for (_, t) in &mut self.0 {
            t.apply(substitution)?;
        }
        Ok(())
    }
}
