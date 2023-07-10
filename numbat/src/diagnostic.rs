use codespan_reporting::diagnostic::LabelStyle;

use crate::{parser::ParseError, resolver::ResolverError, NameResolutionError};

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
