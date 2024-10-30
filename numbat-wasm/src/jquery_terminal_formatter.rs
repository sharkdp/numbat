use numbat::buffered_writer::BufferedWriter;
use numbat::markup::{FormatType, FormattedString, Formatter};

use numbat::compact_str::{format_compact, CompactString};
use termcolor::{Color, WriteColor};

pub struct JqueryTerminalFormatter;

pub fn jt_format(class: Option<&str>, content: &str) -> CompactString {
    if content.is_empty() {
        return CompactString::const_new("");
    }

    let escaped = html_escape::encode_text(content);
    let mut content = CompactString::with_capacity(escaped.len());

    for c in escaped.chars() {
        match c {
            '[' => content.push_str("&#91;"),
            ']' => content.push_str("&#93;"),
            _ => content.push(c),
        }
    }

    if let Some(class) = class {
        format_compact!("[[;;;hl-{class}]{content}]")
    } else {
        content
    }
}

impl Formatter for JqueryTerminalFormatter {
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
        jt_format(css_class, s)
    }
}

pub struct JqueryTerminalWriter {
    buffer: Vec<u8>,
    color: Option<termcolor::ColorSpec>,
}

impl JqueryTerminalWriter {
    pub fn new() -> Self {
        JqueryTerminalWriter {
            buffer: vec![],
            color: None,
        }
    }
}

impl BufferedWriter for JqueryTerminalWriter {
    fn to_string(&self) -> String {
        String::from_utf8_lossy(&self.buffer).into()
    }
}

impl std::io::Write for JqueryTerminalWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        if let Some(color) = &self.color {
            if color.fg() == Some(&Color::Red) {
                self.buffer
                    .write_all("[[;;;hl-diagnostic-red]".as_bytes())?;
                let size = self.buffer.write(buf)?;
                self.buffer.write_all("]".as_bytes())?;
                Ok(size)
            } else if color.fg() == Some(&Color::Blue) {
                self.buffer
                    .write_all("[[;;;hl-diagnostic-blue]".as_bytes())?;
                let size = self.buffer.write(buf)?;
                self.buffer.write_all("]".as_bytes())?;
                Ok(size)
            } else if color.bold() {
                self.buffer
                    .write_all("[[;;;hl-diagnostic-bold]".as_bytes())?;
                let size = self.buffer.write(buf)?;
                self.buffer.write_all("]".as_bytes())?;
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

impl WriteColor for JqueryTerminalWriter {
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
