use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum FormatType {
    Text,
    Keyword,
    Value,
    Unit,
    Identifier,
    TypeIdentifier,
    Operator,
    Decorator,
}

#[derive(Debug, Clone)]
pub enum OutputType {
    Normal,
    Optional,
}

#[derive(Debug, Clone)]
pub struct FormattedString(pub OutputType, pub FormatType, pub String);

#[derive(Debug, Clone, Default)]
pub struct Markup(pub Vec<FormattedString>);

impl Markup {
    fn from(f: FormattedString) -> Self {
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

    fn add(self, rhs: Self) -> Self::Output {
        let mut res = self.0;
        res.extend_from_slice(&rhs.0);
        Markup(res)
    }
}

impl std::iter::Sum for Markup {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(Markup::default(), |acc, n| acc + n)
    }
}

pub fn text(text: impl AsRef<str>) -> Markup {
    Markup::from(FormattedString(
        OutputType::Normal,
        FormatType::Text,
        text.as_ref().to_string(),
    ))
}

pub fn keyword(text: impl AsRef<str>) -> Markup {
    Markup::from(FormattedString(
        OutputType::Normal,
        FormatType::Keyword,
        text.as_ref().to_string(),
    ))
}

pub fn value(text: impl AsRef<str>) -> Markup {
    Markup::from(FormattedString(
        OutputType::Normal,
        FormatType::Value,
        text.as_ref().to_string(),
    ))
}

pub fn unit(text: impl AsRef<str>) -> Markup {
    Markup::from(FormattedString(
        OutputType::Normal,
        FormatType::Unit,
        text.as_ref().to_string(),
    ))
}

pub fn identifier(text: impl AsRef<str>) -> Markup {
    Markup::from(FormattedString(
        OutputType::Normal,
        FormatType::Identifier,
        text.as_ref().to_string(),
    ))
}

pub fn type_identifier(text: impl AsRef<str>) -> Markup {
    Markup::from(FormattedString(
        OutputType::Normal,
        FormatType::TypeIdentifier,
        text.as_ref().to_string(),
    ))
}

pub fn operator(text: impl AsRef<str>) -> Markup {
    Markup::from(FormattedString(
        OutputType::Normal,
        FormatType::Operator,
        text.as_ref().to_string(),
    ))
}

pub fn decorator(text: impl AsRef<str>) -> Markup {
    Markup::from(FormattedString(
        OutputType::Normal,
        FormatType::Decorator,
        text.as_ref().to_string(),
    ))
}

pub fn nl() -> Markup {
    Markup::from(FormattedString(
        OutputType::Normal,
        FormatType::Text,
        "\n".into(),
    ))
}

pub trait Formatter {
    fn format_part(&self, part: &FormattedString) -> String;

    fn format(&self, markup: &Markup, indent: bool) -> String {
        let spaces = self.format_part(&FormattedString(
            OutputType::Normal,
            FormatType::Text,
            "  ".into(),
        ));

        let mut output: String = String::new();
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

struct PlainTextFormatter;

impl Formatter for PlainTextFormatter {
    fn format_part(&self, FormattedString(_, _, text): &FormattedString) -> String {
        text.clone()
    }
}
