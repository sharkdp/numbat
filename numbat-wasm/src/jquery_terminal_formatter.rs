use numbat::markup::{FormatType, FormattedString, Formatter};

pub struct JqueryTerminalFormatter;

pub fn jt_format(class: &str, content: &str) -> String {
    if content.is_empty() {
        "".into()
    } else {
        format!("[[;;;hl-{class}]{content}]")
    }
}

impl Formatter for JqueryTerminalFormatter {
    fn format_part(
        &self,
        FormattedString(_output_type, format_type, s): &FormattedString,
    ) -> String {
        let css_class = match format_type {
            FormatType::Whitespace => return s.clone(),
            FormatType::Dimmed => "dimmed",
            FormatType::Text => return s.clone(),
            FormatType::String => "string",
            FormatType::Keyword => "keyword",
            FormatType::Value => "value",
            FormatType::Unit => "unit",
            FormatType::Identifier => "identifier",
            FormatType::TypeIdentifier => "type-identifier",
            FormatType::Operator => "operator",
            FormatType::Decorator => "decorator",
        };
        jt_format(&css_class, s)
    }
}
