#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceCodePositition {
    pub byte: usize,
    pub index: usize,
    pub line: usize,
    pub position: usize,
}

impl SourceCodePositition {
    pub fn start() -> Self {
        Self {
            byte: 0,
            index: 0,
            line: 1,
            position: 1,
        }
    }

    pub fn to_single_character_span(&self) -> Span {
        Span {
            start: self.clone(),
            end: self.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub start: SourceCodePositition,
    pub end: SourceCodePositition,
}
impl Span {
    pub fn extend(&self, other: &Span) -> Span {
        Span {
            start: self.start,
            end: other.end,
        }
    }
}
