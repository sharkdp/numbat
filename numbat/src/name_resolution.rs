use thiserror::Error;

use crate::Diagnostic;

pub const LAST_RESULT_IDENTIFIERS: &[&str] = &["ans", "_"];

#[derive(Debug, Error)]
pub enum NameResolutionError {
    #[error("Identifier is already in use: '{0}'.")]
    IdentifierClash(String, Diagnostic),
}
