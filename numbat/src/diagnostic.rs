use codespan_reporting::diagnostic::LabelStyle;

use crate::{
    interpreter::RuntimeError, parser::ParseError, resolver::ResolverError,
    typechecker::TypeCheckError, NameResolutionError,
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
            TypeCheckError::IncompatibleDimensions {
                operation,
                span_operation,
                span_actual,
                actual_type,
                span_expected,
                expected_type,
                ..
            } => {
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
                    .with_message(format!("… could not be infered for this function call")),
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
            TypeCheckError::ForeignFunctionNeedsTypeAnnotations(span, _)
            | TypeCheckError::UnknownForeignFunction(span, _)
            | TypeCheckError::NonRationalExponent(span)
            | TypeCheckError::OverflowInConstExpr(span) => d.with_labels(vec![span
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
