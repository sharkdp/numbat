use thiserror::Error;

use crate::type_variable::TypeVariable;
use crate::typed_ast::{DType, Type};

#[derive(Debug, Clone)]
pub struct Substitution(pub Vec<(TypeVariable, Type)>);

impl Substitution {
    pub fn empty() -> Substitution {
        Substitution(vec![])
    }

    pub fn single(v: TypeVariable, t: Type) -> Substitution {
        Substitution(vec![(v, t)])
    }

    pub fn lookup(&self, v: &TypeVariable) -> Option<&Type> {
        self.0.iter().find(|(var, _)| var == v).map(|(_, t)| t)
    }

    pub fn pretty_print(&self) -> String {
        self.0
            .iter()
            .map(|(v, t)| format!("  {} := {}", v.name(), t))
            .collect::<Vec<String>>()
            .join("\n")
    }

    pub fn extend(&mut self, other: Substitution) {
        for (_, t) in &mut self.0 {
            t.apply(&other).unwrap(); // TODO: is the unwrap okay here?
        }
        self.0.extend(other.0);
    }
}

#[derive(Debug, Clone, Error, PartialEq, Eq)]
pub enum SubstitutionError {
    #[error("Used non-dimension type in a dimension expression: {0}")]
    SubstitutedNonDTypeWithinDType(Type),
}

pub trait ApplySubstitution {
    fn apply(&mut self, substitution: &Substitution) -> Result<(), SubstitutionError>;
}

impl ApplySubstitution for Type {
    fn apply(&mut self, _substitution: &Substitution) -> Result<(), SubstitutionError> {
        Ok(()) // TODO
    }
}

impl ApplySubstitution for DType {
    fn apply(&mut self, _substitution: &Substitution) -> Result<(), SubstitutionError> {
        Ok(()) // TODO
    }
}
