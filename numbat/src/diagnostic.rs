use codespan_reporting::diagnostic::LabelStyle;

use crate::{
    interpreter::RuntimeError,
    parser::ParseError,
    resolver::ResolverError,
    typechecker::{IncompatibleDimensionsError, TypeCheckError},
    NameResolutionError,
};

pub type Diagnostic = codespan_reporting::diagnostic::Diagnostic<usize>;

pub trait ErrorDiagnostic {
    fn diagnostic(&self) -> Diagnostic;
}

impl ErrorDiagnostic for ParseError {
    fn diagnostic(&self) -> Diagnostic {
        Diagnostic::error()
            .with_message("while parsing")
            .with_labels(vec![self
                .span
                .diagnostic_label(LabelStyle::Primary)
                .with_message(self.kind.to_string())])
    }
}

impl ErrorDiagnostic for ResolverError {
    fn diagnostic(&self) -> Diagnostic {
        match self {
            ResolverError::UnknownModule(span, _) => Diagnostic::error()
                .with_message("while resolving imports in")
                .with_labels(vec![span
                    .diagnostic_label(LabelStyle::Primary)
                    .with_message("Unknown module")]),
            ResolverError::ParseError(inner) => inner.diagnostic(),
        }
    }
}

impl ErrorDiagnostic for NameResolutionError {
    fn diagnostic(&self) -> Diagnostic {
        match self {
            NameResolutionError::IdentifierClash {
                conflicting_identifier: _,
                conflict_span,
                original_span,
            } => Diagnostic::error()
                .with_message("identifier clash in definition")
                .with_labels(vec![
                    original_span
                        .diagnostic_label(LabelStyle::Secondary)
                        .with_message("Previously defined here"),
                    conflict_span
                        .diagnostic_label(LabelStyle::Primary)
                        .with_message("identifier is already in use"),
                ]),
            NameResolutionError::ReservedIdentifier(span) => Diagnostic::error()
                .with_message("reserved identifier may not be used")
                .with_labels(vec![span
                    .diagnostic_label(LabelStyle::Primary)
                    .with_message("reserved identifier")]),
        }
    }
}

impl ErrorDiagnostic for TypeCheckError {
    fn diagnostic(&self) -> Diagnostic {
        let d = Diagnostic::error().with_message("while type checking");
        let inner_error = format!("{}", self);

        match self {
            TypeCheckError::UnknownIdentifier(span, _, suggestion) => {
                let notes = if let Some(suggestion) = suggestion {
                    vec![format!("Did you mean '{suggestion}'?")]
                } else {
                    vec![]
                };
                d.with_labels(vec![span
                    .diagnostic_label(LabelStyle::Primary)
                    .with_message("unknown identifier")])
                    .with_notes(notes)
            }
            TypeCheckError::UnknownCallable(span, _) => d.with_labels(vec![span
                .diagnostic_label(LabelStyle::Primary)
                .with_message("unknown callable")]),
            TypeCheckError::IncompatibleDimensions(IncompatibleDimensionsError {
                operation,
                span_operation,
                span_actual,
                actual_type,
                span_expected,
                expected_type,
                ..
            }) => {
                let labels = vec![
                    span_operation
                        .diagnostic_label(LabelStyle::Secondary)
                        .with_message(format!("incompatible dimensions in {}", operation)),
                    span_expected
                        .diagnostic_label(LabelStyle::Primary)
                        .with_message(format!("{expected_type}")),
                    span_actual
                        .diagnostic_label(LabelStyle::Primary)
                        .with_message(format!("{actual_type}")),
                ];
                d.with_labels(labels).with_notes(vec![inner_error])
            }
            TypeCheckError::NonScalarExponent(span, type_)
            | TypeCheckError::NonScalarFactorialArgument(span, type_) => d
                .with_labels(vec![span
                    .diagnostic_label(LabelStyle::Primary)
                    .with_message(format!("{type_}"))])
                .with_notes(vec![inner_error]),
            TypeCheckError::UnsupportedConstEvalExpression(span, _) => d.with_labels(vec![span
                .diagnostic_label(LabelStyle::Primary)
                .with_message(inner_error)]),
            TypeCheckError::DivisionByZeroInConstEvalExpression(span) => d.with_labels(vec![span
                .diagnostic_label(LabelStyle::Primary)
                .with_message(inner_error)]),
            TypeCheckError::RegistryError(re) => match re {
                crate::registry::RegistryError::EntryExists(_) => d.with_notes(vec![inner_error]),
                crate::registry::RegistryError::UnknownEntry(name, suggestion) => {
                    d.with_notes(vec![format!(
                        "Unknown dimension '{name}'{maybe_suggestion}",
                        maybe_suggestion = if let Some(suggestion) = suggestion {
                            format!(" did you mean '{suggestion}'?")
                        } else {
                            "".into()
                        }
                    )])
                }
            },
            TypeCheckError::IncompatibleAlternativeDimensionExpression(
                _name,
                span1,
                type1,
                span2,
                type2,
            ) => d
                .with_labels(vec![
                    span1
                        .diagnostic_label(LabelStyle::Primary)
                        .with_message(type1.to_string()),
                    span2
                        .diagnostic_label(LabelStyle::Primary)
                        .with_message(type2.to_string()),
                ])
                .with_notes(vec![inner_error]),
            TypeCheckError::WrongArity {
                callable_span,
                callable_name: _,
                callable_definition_span,
                arity,
                num_args,
            } => {
                let mut labels = vec![callable_span
                    .diagnostic_label(LabelStyle::Primary)
                    .with_message(format!(
                        "{what}was called with {num}, but takes {range}",
                        what = if callable_definition_span.is_some() {
                            ""
                        } else {
                            "procedure "
                        },
                        num = if *num_args == 1 {
                            "one argument".into()
                        } else {
                            format!("{num_args} arguments")
                        },
                        range = if arity.start() == arity.end() {
                            format!("{}", arity.start())
                        } else {
                            format!("{} to {}", arity.start(), arity.end())
                        }
                    ))];
                if let Some(span) = callable_definition_span {
                    labels.insert(
                        0,
                        span.diagnostic_label(LabelStyle::Secondary)
                            .with_message("The function defined here"),
                    );
                }

                d.with_labels(labels)
            }
            TypeCheckError::TypeParameterNameClash(span, _) => d.with_labels(vec![span
                .diagnostic_label(LabelStyle::Primary)
                .with_message(inner_error)]),
            TypeCheckError::CanNotInferTypeParameters(
                span,
                callable_definition_span,
                _,
                params,
            ) => d.with_labels(vec![
                span.diagnostic_label(LabelStyle::Primary)
                    .with_message("… could not be infered for this function call"),
                callable_definition_span
                    .diagnostic_label(LabelStyle::Secondary)
                    .with_message(format!(
                        "The type parameter(s) {params} in this generic function"
                    )),
            ]),
            TypeCheckError::MultipleUnresolvedTypeParameters(span, parameter_span) => d
                .with_labels(vec![
                    span.diagnostic_label(LabelStyle::Secondary)
                        .with_message("In this function call"),
                    parameter_span
                        .diagnostic_label(LabelStyle::Primary)
                        .with_message(inner_error),
                ]),
            TypeCheckError::IncompatibleTypesInCondition(
                if_span,
                then_type,
                then_span,
                else_type,
                else_span,
            ) => d.with_labels(vec![
                then_span
                    .diagnostic_label(LabelStyle::Secondary)
                    .with_message(then_type.to_string()),
                else_span
                    .diagnostic_label(LabelStyle::Secondary)
                    .with_message(else_type.to_string()),
                if_span.diagnostic_label(LabelStyle::Primary).with_message(
                    "Incompatible types in 'then' and 'else' branches of conditional",
                ),
            ]),
            TypeCheckError::IncompatibleTypesInAssertEq(
                procedure_span,
                first_type,
                first_span,
                arg_type,
                arg_span,
            ) => d.with_labels(vec![
                first_span
                    .diagnostic_label(LabelStyle::Secondary)
                    .with_message(first_type.to_string()),
                arg_span
                    .diagnostic_label(LabelStyle::Secondary)
                    .with_message(arg_type.to_string()),
                procedure_span
                    .diagnostic_label(LabelStyle::Primary)
                    .with_message("Incompatible types in 'assert_eq' call"),
            ]),
            TypeCheckError::IncompatibleTypesInAnnotation(
                what,
                what_span,
                annotation,
                annotation_span,
                deduced_type,
                body_span,
            ) => d.with_labels(vec![
                annotation_span
                    .diagnostic_label(LabelStyle::Secondary)
                    .with_message(annotation.to_string()),
                body_span
                    .diagnostic_label(LabelStyle::Secondary)
                    .with_message(deduced_type.to_string()),
                what_span
                    .diagnostic_label(LabelStyle::Primary)
                    .with_message(format!("Incompatible types in {what}")),
            ]),
            TypeCheckError::NameAlreadyUsedBy(_, definition_span, previous_definition_span) => {
                let mut labels = vec![];

                if let Some(span) = previous_definition_span {
                    labels.push(
                        span.diagnostic_label(LabelStyle::Secondary)
                            .with_message("Previously defined here"),
                    );
                }

                labels.push(
                    definition_span
                        .diagnostic_label(LabelStyle::Primary)
                        .with_message(inner_error),
                );
                d.with_labels(labels)
            }
            TypeCheckError::ForeignFunctionNeedsTypeAnnotations(span, _)
            | TypeCheckError::UnknownForeignFunction(span, _)
            | TypeCheckError::NonRationalExponent(span)
            | TypeCheckError::OverflowInConstExpr(span)
            | TypeCheckError::ExpectedDimensionType(span, _)
            | TypeCheckError::ExpectedBool(span) => d.with_labels(vec![span
                .diagnostic_label(LabelStyle::Primary)
                .with_message(inner_error)]),
        }
    }
}

impl ErrorDiagnostic for RuntimeError {
    fn diagnostic(&self) -> Diagnostic {
        Diagnostic::error()
            .with_message("runtime error")
            .with_notes(vec![format!("{self:#}")])
    }
}
