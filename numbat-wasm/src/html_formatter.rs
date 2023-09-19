use numbat::markup::{FormatType, FormattedString, Formatter};

pub struct HtmlFormatter;

impl Formatter for HtmlFormatter {
    fn format_part(
        &self,
        FormattedString(_output_type, format_type, s): &FormattedString,
    ) -> String {
        match format_type {
            FormatType::Whitespace => format!("{s}"),
            FormatType::Dimmed => format!("{s}"),
            FormatType::Text => format!("{s}"),
            FormatType::String => format!("{s}"),
            FormatType::Keyword => format!("<b>{s}</b>"),
            FormatType::Value => format!("<span style=\"color:blue\">{s}</span>"),
            FormatType::Unit => format!("<span style=\"color:green\">{s}</span>"),
            FormatType::Identifier => format!("{s}"),
            FormatType::TypeIdentifier => format!("{s}"),
            FormatType::Operator => format!("{s}"),
            FormatType::Decorator => format!("{s}"),
        }
    }
}
