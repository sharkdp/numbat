use thiserror::Error;

#[derive(Debug, Error, Clone, PartialEq, Eq)]
#[error("line {line}, position {position}")]
pub struct Span {
    pub line: usize,
    pub position: usize,
}
