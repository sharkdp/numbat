use codespan_reporting::diagnostic::{Label, LabelStyle};
use std::ops::{Add, AddAssign};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ByteIndex(pub u32);

impl From<u32> for ByteIndex {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

impl Add<u32> for ByteIndex {
    type Output = Self;

    fn add(self, rhs: u32) -> Self::Output {
        Self(self.0 + rhs)
    }
}

impl AddAssign<u32> for ByteIndex {
    fn add_assign(&mut self, rhs: u32) {
        self.0 += rhs;
    }
}

impl ByteIndex {
    pub fn single_character_span(self, code_source_id: usize) -> Span {
        Span {
            start: self,
            end: self,
            code_source_id,
        }
    }

    pub fn as_usize(self) -> usize {
        self.0 as usize
    }
}

/// The span of text from `start` to `end`, associated with `code_source_id`. `start`
/// and `end` are both inclusive byte indices (so that if `start == end` we get a
/// one-byte span).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: ByteIndex,
    pub end: ByteIndex,
    pub code_source_id: usize,
}

impl Span {
    pub fn extend(&self, other: &Span) -> Span {
        assert_eq!(self.code_source_id, other.code_source_id);
        Span {
            start: std::cmp::min(self.start, other.start),
            end: std::cmp::max(self.end, other.end),
            code_source_id: self.code_source_id,
        }
    }

    pub fn diagnostic_label(&self, style: LabelStyle) -> Label<usize> {
        Label::new(
            style,
            self.code_source_id,
            (self.start.as_usize())..(self.end.as_usize()),
        )
    }

    #[cfg(test)]
    pub fn dummy() -> Span {
        Self {
            start: ByteIndex(0),
            end: ByteIndex(0),
            code_source_id: 0,
        }
    }
}
