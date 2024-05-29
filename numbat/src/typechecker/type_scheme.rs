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
pub struct TypeScheme(usize, QualifiedType);

impl TypeScheme {
    pub fn new(num_quantified: usize, qt: QualifiedType) -> TypeScheme {
        TypeScheme(num_quantified, qt)
    }

    pub fn instantiate(&self, name_generator: &mut NameGenerator) -> QualifiedType {
        // Replace $gen_i by fresh type variables

        let new_type_variables = (0..self.0)
            .map(|_| name_generator.fresh_type_variable())
            .collect::<Vec<_>>();

        self.1.instantiate(&new_type_variables)
    }

    pub fn from_type(type_: Type) -> TypeScheme {
        TypeScheme(0, QualifiedType::new(type_, Bounds::none()))
    }
}

impl ApplySubstitution for TypeScheme {
    fn apply(&mut self, substitution: &Substitution) -> Result<(), SubstitutionError> {
        self.1.apply(substitution)
    }
}
