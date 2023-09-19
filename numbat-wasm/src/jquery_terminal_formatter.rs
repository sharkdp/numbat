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
        match format_type {
            FormatType::Whitespace => format!("{s}"),
            FormatType::Dimmed => format!("{s}"),
            FormatType::Text => format!("{s}"),
            FormatType::String => jt_format("emphasized", s),
            FormatType::Keyword => format!("{s}"),
            FormatType::Value => jt_format("value", s),
            FormatType::Unit => format!("{s}"),
            FormatType::Identifier => format!("{s}"),
            FormatType::TypeIdentifier => format!("{s}"),
            FormatType::Operator => jt_format("emphasized", s),
            FormatType::Decorator => format!("{s}"),
        }
    }
}
