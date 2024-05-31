use super::name_generator::NameGenerator;
use super::qualified_type::{Bounds, QualifiedType};
use super::substitutions::{ApplySubstitution, Substitution, SubstitutionError};
use crate::typed_ast::Type;

/// A type *scheme* is a type with a set of universally quantified
/// type variables.
///
/// The way this is represented is by using type variables of kind
/// `Quantified(i)` in the type tree, where `i` is a unique index
/// for each universally quantified type variable.
///
/// Type schemes can be created by calling .quantify() on a qualified
/// type.
#[derive(Clone, Debug, PartialEq)]
pub enum TypeScheme {
    Concrete(Type),
    Quantified(usize, QualifiedType),
}

impl TypeScheme {
    pub fn new_quantified(num_quantified: usize, qt: QualifiedType) -> TypeScheme {
        TypeScheme::Quantified(num_quantified, qt)
    }

    pub fn instantiate(&self, name_generator: &mut NameGenerator) -> QualifiedType {
        if let TypeScheme::Quantified(n_gen, qt) = &self {
            // Replace $gen_i by fresh type variables

            let new_type_variables = (0..*n_gen)
                .map(|_| name_generator.fresh_type_variable())
                .collect::<Vec<_>>();

            qt.instantiate(&new_type_variables)
        } else {
            unreachable!("Tried to instantiate concrete type")
        }
    }

    pub fn make_quantified(type_: Type) -> TypeScheme {
        TypeScheme::Quantified(0, QualifiedType::new(type_, Bounds::none()))
    }
}

impl ApplySubstitution for TypeScheme {
    fn apply(&mut self, substitution: &Substitution) -> Result<(), SubstitutionError> {
        match self {
            TypeScheme::Concrete(t) => t.apply(substitution),
            TypeScheme::Quantified(_, qt) => qt.apply(substitution),
        }
    }
}
