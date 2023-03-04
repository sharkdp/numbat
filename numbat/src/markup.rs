#[derive(Debug, Clone)]
pub enum FormatType {
    Text,
    Keyword,
    //Emphasized,
    //Error,
    Value,
    Unit,
    Identifier,
    TypeIdentifier,
    Operator,
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

    pub fn to_string(self) -> String {
        let mut result: String = String::new();
        for part in self.0 {
            result += &part.2;
        }
        result
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

pub trait Formatter {
    fn format_part(&self, part: &FormattedString) -> String;

    fn format(&self, markup: Markup) -> String {
        let mut output: String = String::new();
        for part in markup.0 {
            output.push_str(&self.format_part(&part));
        }
        output
    }
}
