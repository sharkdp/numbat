use super::name_generator::NameGenerator;
use super::qualified_type::{Bound, Bounds, QualifiedType};
use super::substitutions::{ApplySubstitution, Substitution, SubstitutionError};
use crate::markup as m;
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

    fn instantiate_with(&self, new_type_variables: &[TypeVariable]) -> QualifiedType {
        if let TypeScheme::Quantified(n_gen, qt) = &self {
            assert!(n_gen == &new_type_variables.len());

            qt.instantiate(&new_type_variables)
        } else {
            unreachable!("Tried to instantiate concrete type: {:#?}", self);
        }
    }

    pub fn instantiate_for_printing(&self) -> (QualifiedType, Vec<TypeVariable>) {
        match self {
            TypeScheme::Concrete(t) => {
                // TODO: we shouldn't print concrete types, but we do it for now in the logging
                (QualifiedType::new(t.clone(), Bounds::none()), vec![])
            }
            TypeScheme::Quantified(n_gen, _) => {
                // TODO: is this a good idea? we don't take care of name clashes here
                let type_parameters = if *n_gen <= 26 {
                    (0..*n_gen)
                        .map(|n| TypeVariable::new(format!("{}", (b'A' + n as u8) as char)))
                        .collect::<Vec<_>>()
                } else {
                    (0..*n_gen)
                        .map(|n| TypeVariable::new(format!("T{n}")))
                        .collect::<Vec<_>>()
                };

                (self.instantiate_with(&type_parameters), type_parameters)
            }
        }
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
        _registry: &crate::dimension::DimensionRegistry,
    ) -> crate::markup::Markup {
        // TODO: Bring back "readable type" functionality, to turn e.g. `Length / Time` into `Velocity`
        self.pretty_print()
    }

    #[track_caller]
    pub(crate) fn unsafe_as_concrete(&self) -> Type {
        match self {
            TypeScheme::Concrete(t) => t.clone(),
            _ => unreachable!("Expected concrete type: {:#?}", self),
        }
    }

    pub(crate) fn generalize(&mut self, dtype_variables: &[TypeVariable]) {
        let free_variables = self.type_variables(true);

        let TypeScheme::Concrete(type_) = self else {
            // TODO: we currently don't report an error here because we run generalization
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

        *self = type_scheme;
    }

    fn type_variables(&self, including_type_parameters: bool) -> Vec<TypeVariable> {
        match self {
            TypeScheme::Concrete(t) => t.type_variables(including_type_parameters),
            TypeScheme::Quantified(_, qt) => qt.type_variables(including_type_parameters),
        }
    }

    // TODO: remove
    pub(crate) fn to_concrete_type(&self) -> Type {
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
        match self {
            TypeScheme::Concrete(t) => t.pretty_print(),
            ts @ TypeScheme::Quantified(n_gen, _) => {
                // TODO: is this a good idea? we don't take care of name clashes here
                let type_parameters = if *n_gen <= 26 {
                    (0..*n_gen)
                        .map(|n| TypeVariable::new(format!("{}", (b'A' + n as u8) as char)))
                        .collect::<Vec<_>>()
                } else {
                    (0..*n_gen)
                        .map(|n| TypeVariable::new(format!("T{n}")))
                        .collect::<Vec<_>>()
                };
                let instantiated_type = ts.instantiate_with(&type_parameters);

                let mut markup = m::empty();

                for type_parameter in &type_parameters {
                    markup += m::keyword("forall");
                    markup += m::space();
                    markup += m::type_identifier(type_parameter.unsafe_name());

                    if instantiated_type.bounds.is_dtype_bound(type_parameter) {
                        markup += m::operator(":");
                        markup += m::space();
                        markup += m::type_identifier("Dim");
                    }
                    markup += m::operator(".");
                    markup += m::space();
                }

                markup + instantiated_type.inner.pretty_print()
            }
        }
    }
}
