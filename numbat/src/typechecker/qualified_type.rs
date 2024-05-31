use crate::{type_variable::TypeVariable, Type};

use super::{
    substitutions::{ApplySubstitution, Substitution, SubstitutionError},
    type_scheme::TypeScheme,
};

/// A predicate on type variables.
#[derive(Clone, Debug, PartialEq)]
pub enum Bound {
    // TODO: At the moment, this can only be IsDim(Type::TVar(_)). Maybe we can find a better representation.
    IsDim(Type),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Bounds(Vec<Bound>);

impl Bounds {
    pub fn none() -> Bounds {
        Bounds(vec![])
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn iter(&self) -> std::slice::Iter<Bound> {
        self.0.iter()
    }

    pub fn iter_mut(&mut self) -> std::slice::IterMut<Bound> {
        self.0.iter_mut()
    }

    pub fn union(self, other: Bounds) -> Bounds {
        let mut bounds = self;
        bounds.0.extend(other.0);
        bounds
    }
}

impl FromIterator<Bound> for Bounds {
    fn from_iter<T: IntoIterator<Item = Bound>>(iter: T) -> Self {
        Bounds(iter.into_iter().collect())
    }
}

/// A qualified type is a type with a (potentially empty) set of bounds
/// on the type variables.
///
/// For example, the type of the square-function (D -> D^2), needs an
/// additional `D: Dim` bound, as arbitrary types (like Bool) can not
/// be squared.
#[derive(Clone, Debug, PartialEq)]
pub struct QualifiedType {
    pub inner: Type,
    pub bounds: Bounds,
}

impl QualifiedType {
    pub fn new(inner: Type, bounds: Bounds) -> QualifiedType {
        QualifiedType { inner, bounds }
    }

    pub fn pretty_print(&self) -> String {
        let bounds = self
            .bounds
            .iter()
            .map(|b| match b {
                Bound::IsDim(t) => format!("Dim({t})", t = t),
            })
            .collect::<Vec<String>>()
            .join(", ");
        if bounds.is_empty() {
            self.inner.to_string()
        } else {
            format!("{} where {}", self.inner.to_string(), bounds)
        }
    }

    pub fn quantify(&self, variables: &[TypeVariable]) -> TypeScheme {
        let mut qt = self.clone();
        for (i, v) in variables.iter().enumerate() {
            qt.apply(&Substitution::single(
                v.clone(),
                Type::TVar(TypeVariable::Quantified(i)),
            ))
            .unwrap();
        }

        TypeScheme::quantified(variables.len(), qt)
    }

    pub fn type_variables(&self) -> Vec<TypeVariable> {
        self.inner.type_variables()
    }

    pub(crate) fn instantiate(&self, type_variables: &[TypeVariable]) -> QualifiedType {
        QualifiedType {
            inner: self.inner.instantiate(type_variables),
            bounds: self
                .bounds
                .iter()
                .map(|b| match b {
                    Bound::IsDim(t) => Bound::IsDim(t.instantiate(type_variables)),
                })
                .collect(),
        }
    }
}

impl ApplySubstitution for QualifiedType {
    fn apply(&mut self, substitution: &Substitution) -> Result<(), SubstitutionError> {
        self.inner.apply(substitution)?;

        for Bound::IsDim(v) in self.bounds.iter_mut() {
            v.apply(substitution)?;
        }
        Ok(())
    }
}
