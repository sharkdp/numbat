use codespan_reporting::diagnostic::{Label, LabelStyle};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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

    pub fn single_character_span(&self, code_source_index: usize) -> Span {
        Span {
            start: *self,
            end: *self,
            code_source_index,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: SourceCodePositition,
    pub end: SourceCodePositition,
    pub code_source_index: usize,
}

impl Span {
    pub fn extend(&self, other: &Span) -> Span {
        assert_eq!(self.code_source_index, other.code_source_index);
        Span {
            start: std::cmp::min(self.start, other.start),
            end: std::cmp::max(self.end, other.end),
            code_source_index: self.code_source_index,
        }
    }

    pub fn diagnostic_label(&self, style: LabelStyle) -> Label<usize> {
        Label::new(
            style,
            self.code_source_index,
            (self.start.byte)..(self.end.byte),
        )
    }

    #[cfg(test)]
    pub fn dummy() -> Span {
        Self {
            start: SourceCodePositition::start(),
            end: SourceCodePositition::start(),
            code_source_index: 0,
        }
    }
}
