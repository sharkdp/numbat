use compact_str::CompactString;

use crate::Type;
use crate::ast::{TypeAnnotation, TypeParameterBound, Visibility};
use crate::dimension::DimensionRegistry;
use crate::pretty_print::PrettyPrint;
use crate::span::Span;
use crate::type_variable::TypeVariable;
use crate::typed_ast::pretty_print_function_signature;

use super::map_stack::MapStack;
use super::substitutions::{ApplySubstitution, Substitution, SubstitutionError};
use super::type_scheme::TypeScheme;

type Identifier = CompactString;

/// Visibility information for an identifier, including its visibility
/// and the code source ID where it was defined.
#[derive(Clone, Debug)]
pub struct VisibilityInfo {
    pub visibility: Visibility,
    pub source_id: usize,
    /// Whether this item is from a module import.
    /// Visibility is only enforced for module-imported items.
    /// User-defined items (from REPL/files) are always accessible.
    pub from_module: bool,
}

#[derive(Clone, Debug)]
pub struct FunctionSignature {
    pub name: CompactString,
    pub definition_span: Span,
    #[allow(dead_code)]
    pub type_parameters: Vec<(Span, CompactString, Option<TypeParameterBound>)>,
    pub parameters: Vec<(Span, CompactString, Option<TypeAnnotation>)>,
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
    pub name: Option<CompactString>,
    pub url: Option<CompactString>,
    pub description: Option<CompactString>,
    pub examples: Vec<(CompactString, Option<CompactString>)>,
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
    /// Visibility information for each identifier
    visibility_info: MapStack<Identifier, VisibilityInfo>,
}

impl Environment {
    pub fn add(
        &mut self,
        i: Identifier,
        type_: Type,
        span: Span,
        is_unit: bool,
        visibility: Visibility,
        from_module: bool,
    ) {
        let source_id = span.code_source_id;
        self.identifiers.insert(
            i.clone(),
            IdentifierKind::Normal(TypeScheme::Concrete(type_), span, is_unit),
        );
        self.visibility_info.insert(
            i,
            VisibilityInfo {
                visibility,
                source_id,
                from_module,
            },
        );
    }

    pub fn add_scheme(
        &mut self,
        i: Identifier,
        scheme: TypeScheme,
        span: Span,
        is_unit: bool,
        visibility: Visibility,
        from_module: bool,
    ) {
        let source_id = span.code_source_id;
        self.identifiers
            .insert(i.clone(), IdentifierKind::Normal(scheme, span, is_unit));
        self.visibility_info.insert(
            i,
            VisibilityInfo {
                visibility,
                source_id,
                from_module,
            },
        );
    }

    pub(crate) fn save(&mut self) {
        self.identifiers.save();
        self.visibility_info.save();
    }

    pub(crate) fn restore(&mut self) {
        self.identifiers.restore();
        self.visibility_info.restore();
    }

    pub(crate) fn add_function(
        &mut self,
        v: CompactString,
        signature: FunctionSignature,
        metadata: FunctionMetadata,
        visibility: Visibility,
        from_module: bool,
    ) {
        let source_id = signature.definition_span.code_source_id;
        self.identifiers
            .insert(v.clone(), IdentifierKind::Function(signature, metadata));
        self.visibility_info.insert(
            v,
            VisibilityInfo {
                visibility,
                source_id,
                from_module,
            },
        );
    }

    pub fn add_predefined(&mut self, v: Identifier, type_: TypeScheme) {
        self.identifiers
            .insert(v, IdentifierKind::Predefined(type_));
        // Predefined identifiers are always public and have no source module
    }

    pub(crate) fn get_identifier_type(&self, v: &str) -> Option<TypeScheme> {
        self.find(v).map(|k| k.get_type())
    }

    /// Get visibility info for an identifier, if it exists
    pub(crate) fn get_visibility_info(&self, v: &str) -> Option<&VisibilityInfo> {
        self.visibility_info.get(v)
    }

    #[allow(dead_code)]
    pub(crate) fn iter_identifiers(&self) -> impl Iterator<Item = &Identifier> {
        self.identifiers.keys()
    }

    /// Iterate over identifiers that are accessible from a given source.
    /// An identifier is accessible if it's public, or if it's from the same source.
    pub(crate) fn iter_accessible_identifiers(
        &self,
        current_source_id: Option<usize>,
    ) -> impl Iterator<Item = &Identifier> {
        self.identifiers.keys().filter(move |id| {
            match self.visibility_info.get(id.as_str()) {
                Some(info) => {
                    info.visibility == Visibility::Public
                        || current_source_id == Some(info.source_id)
                }
                None => true, // Predefined identifiers are always accessible
            }
        })
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

    pub(crate) fn get_proper_function_reference<'a>(
        &self,
        expr: &crate::ast::Expression<'a>,
    ) -> Option<(&'a str, &FunctionSignature)> {
        match expr {
            crate::ast::Expression::Identifier(_, name) => self
                .get_function_info(name)
                .map(|(signature, _)| (*name, signature)),
            _ => None,
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
