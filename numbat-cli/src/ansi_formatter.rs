use numbat::{
    compact_str::{CompactString, ToCompactString},
    markup::{FormatType, FormattedString, Formatter, Markup},
};

use colored::Colorize;

pub struct ANSIFormatter;

impl Formatter for ANSIFormatter {
    fn format_part(
        &self,
        FormattedString(_output_type, format_type, text): &FormattedString,
    ) -> CompactString {
        (match format_type {
            FormatType::Whitespace => text.normal(),
            FormatType::Emphasized => text.bold(),
            FormatType::Dimmed => text.dimmed(),
            FormatType::Text => text.normal(),
            FormatType::String => text.green(),
            FormatType::Keyword => text.magenta(),
            FormatType::Value => text.yellow(),
            FormatType::Unit => text.cyan(),
            FormatType::Identifier => text.normal(),
            FormatType::TypeIdentifier => text.blue().italic(),
            FormatType::Operator => text.bold(),
            FormatType::Decorator => text.green(),
        })
        .to_compact_string()
    }
}

pub fn ansi_format(m: &Markup, indent: bool) -> CompactString {
    ANSIFormatter {}.format(m, indent)
}
