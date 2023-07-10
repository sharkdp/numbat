use thiserror::Error;

use crate::span::Span;

pub const LAST_RESULT_IDENTIFIERS: &[&str] = &["ans", "_"];

#[derive(Debug, Error)]
pub enum NameResolutionError {
    #[error("Identifier is already in use: '{conflicting_identifier}'.")]
    IdentifierClash {
        conflicting_identifier: String,
        conflicting_definition: Span,
    },
}
