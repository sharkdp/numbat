use log::debug;

use super::name_generator::NameGenerator;
use super::qualified_type::{Bound, Bounds, QualifiedType};
use super::substitutions::{ApplySubstitution, Substitution, SubstitutionError};
use crate::pretty_print::PrettyPrint;
use crate::type_variable::TypeVariable;
use crate::typed_ast::Type;

/// A type *scheme* (the `Quantified` part below) is a type with a
/// set of universally quantified type variables.
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
    pub fn concrete(type_: Type) -> TypeScheme {
        TypeScheme::Concrete(type_)
    }

    pub fn quantified(num_quantified: usize, qt: QualifiedType) -> TypeScheme {
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
            unreachable!("Tried to instantiate concrete type: {:#?}", self);
        }
    }

    pub fn make_quantified(type_: Type) -> TypeScheme {
        TypeScheme::Quantified(0, QualifiedType::new(type_, Bounds::none()))
    }

    pub(crate) fn is_dtype(&self) -> bool {
        match self {
            TypeScheme::Concrete(t) => t.is_dtype(),
            TypeScheme::Quantified(_, qt) => qt.inner.is_dtype(),
        }
    }

    pub(crate) fn is_scalar(&self) -> bool {
        match self {
            TypeScheme::Concrete(t) => t.is_scalar(),
            TypeScheme::Quantified(_, qt) => qt.inner.is_scalar(),
        }
    }

    pub(crate) fn to_readable_type(
        &self,
        registry: &crate::dimension::DimensionRegistry,
    ) -> crate::markup::Markup {
        todo!()
    }

    #[track_caller]
    pub(crate) fn unsafe_as_concrete(&self) -> Type {
        match self {
            TypeScheme::Concrete(t) => t.clone(),
            _ => unreachable!("Expected concrete type: {:#?}", self),
        }
    }

    pub(crate) fn generalize(&mut self, dtype_variables: &[TypeVariable]) {
        debug!("Generalizing type: {:#?}", self);

        let free_variables = self.type_variables();

        let TypeScheme::Concrete(type_) = self else {
            // TODO: we currently don't report and error here because we run generalization
            // again and again on the environment, so it has a few concrete types in it. But
            // maybe it would be better to model this explicitly and report an error here.
            return;
        };

        // Generate qualified type
        let bounds = dtype_variables
            .iter()
            .map(|v| Bound::IsDim(Type::TVar(v.clone())))
            .collect();
        let qualified_type = QualifiedType::new(type_.clone(), bounds);

        // Generalization: quantify over all free type variables
        let type_scheme = qualified_type.quantify(&free_variables);

        debug!("… to type scheme: {:#?}", type_scheme);

        *self = type_scheme;
    }

    fn type_variables(&self) -> Vec<TypeVariable> {
        match self {
            TypeScheme::Concrete(t) => t.type_variables(),
            TypeScheme::Quantified(_, qt) => qt.type_variables(),
        }
    }

    pub(crate) fn to_concrete_type(&self) -> Type {
        // TODO: make sure there are no type variables / $tget nodes left over
        match self {
            TypeScheme::Concrete(t) => t.clone(),
            TypeScheme::Quantified(_, qt) => qt.inner.clone(),
        }
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

impl PrettyPrint for TypeScheme {
    fn pretty_print(&self) -> crate::markup::Markup {
        // TODO
        match self {
            TypeScheme::Concrete(t) => t.pretty_print(),
            TypeScheme::Quantified(_, qt) => qt.inner.pretty_print(),
        }
    }
}
