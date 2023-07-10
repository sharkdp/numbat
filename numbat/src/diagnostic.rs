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
        let inner_error = format!("{}", self);

        match self {
            TypeCheckError::UnknownIdentifier(span, _) => d.with_labels(vec![span
                .diagnostic_label(LabelStyle::Primary)
                .with_message("unknown identifier")]),
            TypeCheckError::UnknownFunction(span, _) => d.with_labels(vec![span
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
            TypeCheckError::NonScalarExponent(span, type_) => d
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
            TypeCheckError::RegistryError(_) => d.with_notes(vec![inner_error]),
            TypeCheckError::IncompatibleAlternativeDimensionExpression(_) => {
                d.with_notes(vec![inner_error])
            }
            TypeCheckError::WrongArity {
                callable_span,
                callable_name: _,
                arity,
                num_args,
            } => d.with_labels(vec![callable_span
                .diagnostic_label(LabelStyle::Primary)
                .with_message(format!(
                    "Function or procedure called with {num}, but takes {range}",
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
                ))]),
            TypeCheckError::TypeParameterNameClash(_) => d.with_notes(vec![inner_error]),
            TypeCheckError::CanNotInferTypeParameters(_, _) => d.with_notes(vec![inner_error]),
            TypeCheckError::MultipleUnresolvedTypeParameters => d.with_notes(vec![inner_error]),
            TypeCheckError::ForeignFunctionNeedsReturnTypeAnnotation(span, _) => {
                d.with_labels(vec![span
                    .diagnostic_label(LabelStyle::Primary)
                    .with_message(inner_error)])
            }
            TypeCheckError::UnknownForeignFunction(span, _) => d.with_labels(vec![span
                .diagnostic_label(LabelStyle::Primary)
                .with_message(inner_error)]),
            TypeCheckError::ParameterTypesCanNotBeDeduced => d.with_notes(vec![inner_error]),
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
