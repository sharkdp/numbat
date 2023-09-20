use numbat::markup::{FormatType, FormattedString, Formatter};

pub struct JqueryTerminalFormatter;

pub fn jt_format(class: Option<&str>, content: &str) -> String {
    if content.is_empty() {
        return "".into();
    }

    let content = html_escape::encode_text(content)
        .replace("[", "&#91;")
        .replace("]", "&#93;");

    if let Some(class) = class {
        format!("[[;;;hl-{class}]{content}]")
    } else {
        content.into()
    }
}

impl Formatter for JqueryTerminalFormatter {
    fn format_part(
        &self,
        FormattedString(_output_type, format_type, s): &FormattedString,
    ) -> String {
        let css_class = match format_type {
            FormatType::Whitespace => None,
            FormatType::Dimmed => Some("dimmed"),
            FormatType::Text => None,
            FormatType::String => Some("string"),
            FormatType::Keyword => Some("keyword"),
            FormatType::Value => Some("value"),
            FormatType::Unit => Some("unit"),
            FormatType::Identifier => Some("identifier"),
            FormatType::TypeIdentifier => Some("type-identifier"),
            FormatType::Operator => Some("operator"),
            FormatType::Decorator => Some("decorator"),
        };
        jt_format(css_class, s)
    }
}
