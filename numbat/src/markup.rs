use std::fmt::Display;

use compact_str::CompactString;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FormatType {
    Whitespace,
    Emphasized,
    Dimmed,
    Text,
    String,
    Keyword,
    Value,
    Unit,
    Identifier,
    TypeIdentifier,
    Operator,
    Decorator,
}

#[derive(Debug, Clone, PartialEq)]
pub enum OutputType {
    Normal,
    Optional,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CompactStrCow {
    Owned(CompactString),
    Static(&'static str),
}

impl From<CompactStrCow> for CompactString {
    fn from(value: CompactStrCow) -> Self {
        match value {
            CompactStrCow::Owned(compact_string) => compact_string,
            CompactStrCow::Static(s) => CompactString::const_new(s),
        }
    }
}

impl std::ops::Deref for CompactStrCow {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match self {
            CompactStrCow::Owned(compact_string) => compact_string,
            CompactStrCow::Static(s) => s,
        }
    }
}

impl From<CompactString> for CompactStrCow {
    fn from(value: CompactString) -> Self {
        Self::Owned(value)
    }
}

impl From<&'static str> for CompactStrCow {
    fn from(value: &'static str) -> Self {
        Self::Static(value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FormattedString(pub OutputType, pub FormatType, pub CompactStrCow);

#[derive(Debug, Clone, Default, PartialEq)]
pub struct Markup(pub Vec<FormattedString>);

impl Markup {
    pub fn from(f: FormattedString) -> Self {
        Self(vec![f])
    }
}

impl Display for Markup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", PlainTextFormatter {}.format(self, false))
    }
}

impl std::ops::Add for Markup {
    type Output = Markup;

    fn add(mut self, rhs: Self) -> Self::Output {
        self.0.extend(rhs.0);
        self
    }
}

impl std::ops::AddAssign for Markup {
    fn add_assign(&mut self, rhs: Self) {
        self.0.extend(rhs.0)
    }
}

impl std::iter::Sum for Markup {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(empty(), |acc, n| acc + n)
    }
}

pub fn space() -> Markup {
    Markup::from(FormattedString(
        OutputType::Normal,
        FormatType::Whitespace,
        " ".into(),
    ))
}

pub fn empty() -> Markup {
    Markup::default()
}

pub fn whitespace(text: impl Into<CompactStrCow>) -> Markup {
    Markup::from(FormattedString(
        OutputType::Normal,
        FormatType::Whitespace,
        text.into(),
    ))
}

pub fn emphasized(text: impl Into<CompactStrCow>) -> Markup {
    Markup::from(FormattedString(
        OutputType::Normal,
        FormatType::Emphasized,
        text.into(),
    ))
}

pub fn dimmed(text: impl Into<CompactStrCow>) -> Markup {
    Markup::from(FormattedString(
        OutputType::Normal,
        FormatType::Dimmed,
        text.into(),
    ))
}

pub fn text(text: impl Into<CompactStrCow>) -> Markup {
    Markup::from(FormattedString(
        OutputType::Normal,
        FormatType::Text,
        text.into(),
    ))
}

pub fn string(text: impl Into<CompactStrCow>) -> Markup {
    Markup::from(FormattedString(
        OutputType::Normal,
        FormatType::String,
        text.into(),
    ))
}

pub fn keyword(text: impl Into<CompactStrCow>) -> Markup {
    Markup::from(FormattedString(
        OutputType::Normal,
        FormatType::Keyword,
        text.into(),
    ))
}

pub fn value(text: impl Into<CompactStrCow>) -> Markup {
    Markup::from(FormattedString(
        OutputType::Normal,
        FormatType::Value,
        text.into(),
    ))
}

pub fn unit(text: impl Into<CompactStrCow>) -> Markup {
    Markup::from(FormattedString(
        OutputType::Normal,
        FormatType::Unit,
        text.into(),
    ))
}

pub fn identifier(text: impl Into<CompactStrCow>) -> Markup {
    Markup::from(FormattedString(
        OutputType::Normal,
        FormatType::Identifier,
        text.into(),
    ))
}

pub fn type_identifier(text: impl Into<CompactStrCow>) -> Markup {
    Markup::from(FormattedString(
        OutputType::Normal,
        FormatType::TypeIdentifier,
        text.into(),
    ))
}

pub fn operator(text: impl Into<CompactStrCow>) -> Markup {
    Markup::from(FormattedString(
        OutputType::Normal,
        FormatType::Operator,
        text.into(),
    ))
}

pub fn decorator(text: impl Into<CompactStrCow>) -> Markup {
    Markup::from(FormattedString(
        OutputType::Normal,
        FormatType::Decorator,
        text.into(),
    ))
}

pub fn nl() -> Markup {
    Markup::from(FormattedString(
        OutputType::Normal,
        FormatType::Whitespace,
        "\n".into(),
    ))
}

pub trait Formatter {
    fn format_part(&self, part: &FormattedString) -> CompactString;

    fn format(&self, markup: &Markup, indent: bool) -> CompactString {
        let spaces = self.format_part(&FormattedString(
            OutputType::Normal,
            FormatType::Whitespace,
            "  ".into(),
        ));

        let mut output = CompactString::with_capacity(spaces.len() + markup.0.len());
        if indent {
            output.push_str(&spaces);
        }
        for part in &markup.0 {
            output.push_str(&self.format_part(part));
            if indent && part.2.contains('\n') {
                output.push_str(&spaces);
            }
        }
        output
    }
}

pub struct PlainTextFormatter;

impl Formatter for PlainTextFormatter {
    fn format_part(&self, FormattedString(_, _, text): &FormattedString) -> CompactString {
        text.clone().into()
    }
}

pub fn plain_text_format(m: &Markup, indent: bool) -> CompactString {
    PlainTextFormatter {}.format(m, indent)
}
