use codespan_reporting::diagnostic::{Label, LabelStyle};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceCodePositition {
    pub byte: u32,
    pub line: u32,
    pub position: u32,
}

impl SourceCodePositition {
    pub fn start() -> Self {
        Self {
            byte: 0,
            line: 1,
            position: 1,
        }
    }

    pub fn single_character_span(&self, code_source_id: usize) -> Span {
        Span {
            start: *self,
            end: *self,
            code_source_id,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub start: SourceCodePositition,
    pub end: SourceCodePositition,
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
            (self.start.byte as usize)..(self.end.byte as usize),
        )
    }

    #[cfg(test)]
    pub fn dummy() -> Span {
        Self {
            start: SourceCodePositition::start(),
            end: SourceCodePositition::start(),
            code_source_id: 0,
        }
    }
}
