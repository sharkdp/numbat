use crate::buffered_writer::BufferedWriter;
use crate::markup::{FormatType, FormattedString, Formatter};

use compact_str::{CompactString, format_compact};
use termcolor::{Color, WriteColor};

pub struct HtmlFormatter;

pub fn html_format(class: Option<&str>, content: &str) -> CompactString {
    if content.is_empty() {
        return CompactString::const_new("");
    }

    let content = html_escape::encode_text(content);

    if let Some(class) = class {
        format_compact!("<span class=\"numbat-{class}\">{content}</span>")
    } else {
        content.into()
    }
}

impl Formatter for HtmlFormatter {
    fn format_part(
        &self,
        FormattedString(_output_type, format_type, s): &FormattedString,
    ) -> CompactString {
        let css_class = match format_type {
            FormatType::Whitespace => None,
            FormatType::Emphasized => Some("emphasized"),
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
        html_format(css_class, s)
    }
}

pub struct HtmlWriter {
    buffer: Vec<u8>,
    color: Option<termcolor::ColorSpec>,
}

impl Default for HtmlWriter {
    fn default() -> Self {
        Self::new()
    }
}

impl HtmlWriter {
    pub fn new() -> Self {
        HtmlWriter {
            buffer: vec![],
            color: None,
        }
    }
}

impl BufferedWriter for HtmlWriter {
    fn to_string(&self) -> String {
        String::from_utf8_lossy(&self.buffer).into()
    }
}

impl std::io::Write for HtmlWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        if let Some(color) = &self.color {
            if color.fg() == Some(&Color::Red) {
                self.buffer
                    .write_all("<span class=\"numbat-diagnostic-red\">".as_bytes())?;
                let size = self.buffer.write(buf)?;
                self.buffer.write_all("</span>".as_bytes())?;
                Ok(size)
            } else if color.fg() == Some(&Color::Blue) {
                self.buffer
                    .write_all("<span class=\"numbat-diagnostic-blue\">".as_bytes())?;
                let size = self.buffer.write(buf)?;
                self.buffer.write_all("</span>".as_bytes())?;
                Ok(size)
            } else if color.bold() {
                self.buffer
                    .write_all("<span class=\"numbat-diagnostic-bold\">".as_bytes())?;
                let size = self.buffer.write(buf)?;
                self.buffer.write_all("</span>".as_bytes())?;
                Ok(size)
            } else {
                self.buffer.write(buf)
            }
        } else {
            self.buffer.write(buf)
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.buffer.flush()
    }
}

impl WriteColor for HtmlWriter {
    fn supports_color(&self) -> bool {
        true
    }

    fn set_color(&mut self, spec: &termcolor::ColorSpec) -> std::io::Result<()> {
        self.color = Some(spec.clone());
        Ok(())
    }

    fn reset(&mut self) -> std::io::Result<()> {
        self.color = None;
        Ok(())
    }
}
