use thiserror::Error;

#[derive(Debug, Error)]
pub enum NameResolutionError {
    #[error("Identifier is already in use: '{0}'.")]
    IdentifierClash(String),
}
