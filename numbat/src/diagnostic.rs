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
                conflicting_definition,
            } => Diagnostic::error()
                .with_message("identifier clash in definition")
                .with_labels(vec![conflicting_definition
                    .diagnostic_label(LabelStyle::Primary)
                    .with_message("Identifier is already in use")]),
        }
    }
}

impl ErrorDiagnostic for TypeCheckError {
    fn diagnostic(&self) -> Diagnostic {
        let d = Diagnostic::error().with_message("while type checking");

        match self {
            TypeCheckError::UnknownIdentifier(span, _) => {
                d.with_labels(vec![span.diagnostic_label(LabelStyle::Primary)])
            }
            TypeCheckError::UnknownFunction(_) => d.with_notes(vec![format!("{self:#}")]),
            TypeCheckError::IncompatibleDimensions {
                operation,
                span_operation,
                span_actual,
                actual_type,
                span_expected,
                expected_type,
                ..
            } => {
                let mut labels = vec![span_operation
                    .diagnostic_label(LabelStyle::Secondary)
                    .with_message(format!("incompatible dimensions in {}", operation))];
                if let Some(span_actual) = span_actual {
                    labels.push(
                        span_actual
                            .diagnostic_label(LabelStyle::Primary)
                            .with_message(format!("{actual_type}")),
                    );
                }
                if let Some(span_expected) = span_expected {
                    labels.push(
                        span_expected
                            .diagnostic_label(LabelStyle::Primary)
                            .with_message(format!("{expected_type}")),
                    );
                }
                d.with_labels(labels).with_notes(vec![format!("{self:#}")])
            }
            TypeCheckError::NonScalarExponent(span, type_) => d
                .with_labels(vec![span
                    .diagnostic_label(LabelStyle::Primary)
                    .with_message(format!("{type_}"))])
                .with_notes(vec![format!("{self:#}")]),
            TypeCheckError::UnsupportedConstEvalExpression(_) => {
                d.with_notes(vec![format!("{self:#}")])
            }
            TypeCheckError::DivisionByZeroInConstEvalExpression => {
                d.with_notes(vec![format!("{self:#}")])
            }
            TypeCheckError::RegistryError(_) => d.with_notes(vec![format!("{self:#}")]),
            TypeCheckError::IncompatibleAlternativeDimensionExpression(_) => {
                d.with_notes(vec![format!("{self:#}")])
            }
            TypeCheckError::WrongArity {
                callable_name: _,
                arity: _,
                num_args: _,
            } => d.with_notes(vec![format!("{self:#}")]),
            TypeCheckError::TypeParameterNameClash(_) => d.with_notes(vec![format!("{self:#}")]),
            TypeCheckError::CanNotInferTypeParameters(_, _) => {
                d.with_notes(vec![format!("{self:#}")])
            }
            TypeCheckError::MultipleUnresolvedTypeParameters => {
                d.with_notes(vec![format!("{self:#}")])
            }
            TypeCheckError::ForeignFunctionNeedsReturnTypeAnnotation(_) => {
                d.with_notes(vec![format!("{self:#}")])
            }
            TypeCheckError::UnknownForeignFunction(_) => d.with_notes(vec![format!("{self:#}")]),
            TypeCheckError::ParameterTypesCanNotBeDeduced => {
                d.with_notes(vec![format!("{self:#}")])
            }
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
