use thiserror::Error;

pub const LAST_RESULT_IDENTIFIERS: &'static [&'static str] = &["ans", "_"];

#[derive(Debug, Error)]
pub enum NameResolutionError {
    #[error("Identifier is already in use: '{0}'.")]
    IdentifierClash(String),
}
