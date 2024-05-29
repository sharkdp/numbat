use super::substitutions::{ApplySubstitution, Substitution, SubstitutionError};
use super::type_scheme::TypeScheme;

use std::collections::HashMap;

type Identifier = String; // TODO ?

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
