use crate::{Type, type_variable::TypeVariable};

use super::{
    substitutions::{ApplySubstitution, Substitution, SubstitutionError},
    type_scheme::TypeScheme,
};

/// A predicate on type variables.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Bound {
    IsDim(Type),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Bounds(Vec<Bound>);

impl Bounds {
    pub fn none() -> Bounds {
        Bounds(vec![])
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Bound> {
        self.0.iter()
    }

    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, Bound> {
        self.0.iter_mut()
    }

    pub fn union(self, other: Bounds) -> Bounds {
        let mut bounds = self;
        bounds.0.extend(other.0);
        bounds
    }

    pub fn is_dtype_bound(&self, tv: &TypeVariable) -> bool {
        self.0.iter().any(|b| match b {
            Bound::IsDim(Type::TVar(v)) => v == tv,
            _ => false,
        })
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct QualifiedType {
    pub inner: Type,
    pub bounds: Bounds,
}

impl QualifiedType {
    pub fn new(inner: Type, bounds: Bounds) -> QualifiedType {
        QualifiedType { inner, bounds }
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

    pub fn type_variables(&self, including_type_parameters: bool) -> Vec<TypeVariable> {
        self.inner.type_variables(including_type_parameters)
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
