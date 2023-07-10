use codespan_reporting::diagnostic::LabelStyle;

use crate::{
    interpreter::RuntimeError, parser::ParseError, resolver::ResolverError,
    typechecker::TypeCheckError, NameResolutionError,
};

pub type Diagnostic = codespan_reporting::diagnostic::Diagnostic<usize>;

pub trait ErrorDiagnostic {
    fn diagnostic(self) -> Diagnostic;
}

impl ErrorDiagnostic for ParseError {
    fn diagnostic(self) -> Diagnostic {
        Diagnostic::error()
            .with_message("while parsing")
            .with_labels(vec![self
                .span
                .diagnostic_label(LabelStyle::Primary)
                .with_message(self.kind.to_string())])
    }
}

impl ErrorDiagnostic for ResolverError {
    fn diagnostic(self) -> Diagnostic {
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
    fn diagnostic(self) -> Diagnostic {
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
    fn diagnostic(self) -> Diagnostic {
        let d = Diagnostic::error()
            .with_message("while type checking")
            .with_notes(vec![format!("{self:#}")]);

        match self {
            TypeCheckError::UnknownIdentifier(span, _) => {
                d.with_labels(vec![span.diagnostic_label(LabelStyle::Primary)])
            }
            TypeCheckError::UnknownFunction(_) => d,
            TypeCheckError::IncompatibleDimensions(span, _, _, _, _, _) => {
                if let Some(span) = span {
                    d.with_labels(vec![span.diagnostic_label(LabelStyle::Primary)])
                } else {
                    d
                }
            }
            TypeCheckError::NonScalarExponent(_) => d,
            TypeCheckError::UnsupportedConstEvalExpression(_) => d,
            TypeCheckError::DivisionByZeroInConstEvalExpression => d,
            TypeCheckError::RegistryError(_) => d,
            TypeCheckError::IncompatibleAlternativeDimensionExpression(_) => d,
            TypeCheckError::WrongArity {
                callable_name: _,
                arity: _,
                num_args: _,
            } => d,
            TypeCheckError::TypeParameterNameClash(_) => d,
            TypeCheckError::CanNotInferTypeParameters(_, _) => d,
            TypeCheckError::MultipleUnresolvedTypeParameters => d,
            TypeCheckError::ForeignFunctionNeedsReturnTypeAnnotation(_) => d,
            TypeCheckError::UnknownForeignFunction(_) => d,
            TypeCheckError::ParameterTypesCanNotBeDeduced => d,
        }
    }
}

impl ErrorDiagnostic for RuntimeError {
    fn diagnostic(self) -> Diagnostic {
        Diagnostic::error()
            .with_message("runtime error")
            .with_notes(vec![format!("{self:#}")])
    }
}
