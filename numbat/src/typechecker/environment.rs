use crate::ast::{TypeAnnotation, TypeParameterBound};
use crate::dimension::DimensionRegistry;
use crate::pretty_print::PrettyPrint;
use crate::span::Span;
use crate::type_variable::TypeVariable;
use crate::typed_ast::pretty_print_function_signature;
use crate::Type;

use super::map_stack::MapStack;
use super::substitutions::{ApplySubstitution, Substitution, SubstitutionError};
use super::type_scheme::TypeScheme;

type Identifier = String;

#[derive(Clone, Debug)]
pub struct FunctionSignature {
    pub name: String,
    pub definition_span: Span,
    #[allow(dead_code)]
    pub type_parameters: Vec<(Span, String, Option<TypeParameterBound>)>,
    pub parameters: Vec<(Span, String, Option<TypeAnnotation>)>,
    pub return_type_annotation: Option<TypeAnnotation>,
    pub fn_type: TypeScheme,
}

impl FunctionSignature {
    pub fn pretty_print(&self, registry: &DimensionRegistry) -> crate::markup::Markup {
        let (fn_type, type_parameters) = self.fn_type.instantiate_for_printing(Some(
            self.type_parameters
                .iter()
                .map(|(_, name, _)| name.as_str()),
        ));

        let Type::Fn(ref parameter_types, ref return_type) = fn_type.inner else {
            unreachable!()
        };

        let parameters =
            self.parameters
                .iter()
                .zip(parameter_types)
                .map(|((_, name, annotation), type_)| {
                    let readable_type = match annotation {
                        Some(annotation) => annotation.pretty_print(),
                        None => type_.to_readable_type(registry),
                    };
                    (name.as_str(), readable_type)
                });

        let readable_return_type = match &self.return_type_annotation {
            Some(annotation) => annotation.pretty_print(),
            None => return_type.to_readable_type(registry),
        };

        pretty_print_function_signature(
            &self.name,
            &fn_type,
            &type_parameters,
            parameters,
            &readable_return_type,
        )
    }
}

#[derive(Clone, Debug)]
pub struct FunctionMetadata {
    pub name: Option<String>,
    pub url: Option<String>,
    pub description: Option<String>,
    pub examples: Vec<(String, Option<String>)>,
}

#[derive(Clone, Debug)]
pub enum IdentifierKind {
    /// A normal identifier (variable, unit) with the place where it has been defined.
    /// The boolean flag signifies whether the identifier is a unit or not
    Normal(TypeScheme, #[allow(dead_code)] Span, bool),
    /// A function
    Function(FunctionSignature, FunctionMetadata),
    /// Identifiers that are defined by the language: `_` and `ans` (see LAST_RESULT_IDENTIFIERS)
    Predefined(TypeScheme),
}

impl IdentifierKind {
    fn get_type(&self) -> TypeScheme {
        match self {
            IdentifierKind::Predefined(t) => t.clone(),
            IdentifierKind::Normal(t, _, _) => t.clone(),
            IdentifierKind::Function(s, _) => s.fn_type.clone(),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Environment {
    identifiers: MapStack<Identifier, IdentifierKind>,
}

impl Environment {
    pub fn add(&mut self, i: Identifier, type_: Type, span: Span, is_unit: bool) {
        self.identifiers.insert(
            i,
            IdentifierKind::Normal(TypeScheme::Concrete(type_), span, is_unit),
        );
    }

    pub fn add_scheme(&mut self, i: Identifier, scheme: TypeScheme, span: Span, is_unit: bool) {
        self.identifiers
            .insert(i, IdentifierKind::Normal(scheme, span, is_unit));
    }

    pub(crate) fn save(&mut self) {
        self.identifiers.save();
    }

    pub(crate) fn restore(&mut self) {
        self.identifiers.restore();
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
        self.find(v).map(|k| k.get_type())
    }

    pub(crate) fn iter_identifiers(&self) -> impl Iterator<Item = &Identifier> {
        self.identifiers.keys()
    }

    pub fn iter_relevant_matches(&self) -> impl Iterator<Item = (&Identifier, TypeScheme)> {
        self.identifiers
            .iter()
            .filter(|(_, kind)| {
                !matches!(
                    kind,
                    IdentifierKind::Normal(_, _, true) | IdentifierKind::Predefined(..)
                )
            })
            .map(|(id, kind)| (id, kind.get_type()))
    }

    fn find(&self, name: &str) -> Option<&IdentifierKind> {
        self.identifiers.get(name)
    }

    pub(crate) fn get_function_info(
        &self,
        name: &str,
    ) -> Option<(&FunctionSignature, &FunctionMetadata)> {
        match self.find(name) {
            Some(IdentifierKind::Function(signature, metadata)) => Some((signature, metadata)),
            _ => None,
        }
    }

    pub(crate) fn generalize_types(&mut self, dtype_variables: &[TypeVariable]) {
        for (_, kind) in self.identifiers.iter_mut() {
            match kind {
                IdentifierKind::Normal(t, _, _) => {
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
                IdentifierKind::Normal(t, _, _) => {
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
