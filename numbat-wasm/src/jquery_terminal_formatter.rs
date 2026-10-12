use numbat::buffered_writer::BufferedWriter;
use numbat::markup::{FormatType, FormattedString, Formatter};

use numbat::compact_str::{CompactString, format_compact};
use termcolor::{Color, WriteColor};

pub struct JqueryTerminalFormatter;

// Escape user-visible text without altering jQuery Terminal's formatting codes.
fn escape_terminal_bytes(bytes: &[u8]) -> Vec<u8> {
    let mut escaped = Vec::with_capacity(bytes.len());

    for &byte in bytes {
        match byte {
            b'&' => escaped.extend_from_slice(b"&amp;"),
            b'<' => escaped.extend_from_slice(b"&lt;"),
            b'>' => escaped.extend_from_slice(b"&gt;"),
            b'\\' => escaped.extend_from_slice(b"&#92;"),
            b'[' => escaped.extend_from_slice(b"&#91;"),
            b']' => escaped.extend_from_slice(b"&#93;"),
            _ => escaped.push(byte),
        }
    }

    escaped
}

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
            '\\' => content.push_str("&#92;"),
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
        let escaped = escape_terminal_bytes(buf);

        if let Some(color) = &self.color {
            if color.fg() == Some(&Color::Red) {
                self.buffer
                    .write_all("[[;;;hl-diagnostic-red]".as_bytes())?;
                self.buffer.write_all(&escaped)?;
                self.buffer.write_all("]".as_bytes())?;
                Ok(buf.len())
            } else if color.fg() == Some(&Color::Blue) {
                self.buffer
                    .write_all("[[;;;hl-diagnostic-blue]".as_bytes())?;
                self.buffer.write_all(&escaped)?;
                self.buffer.write_all("]".as_bytes())?;
                Ok(buf.len())
            } else if color.bold() {
                self.buffer
                    .write_all("[[;;;hl-diagnostic-bold]".as_bytes())?;
                self.buffer.write_all(&escaped)?;
                self.buffer.write_all("]".as_bytes())?;
                Ok(buf.len())
            } else {
                self.buffer.write_all(&escaped)?;
                Ok(buf.len())
            }
        } else {
            self.buffer.write_all(&escaped)?;
            Ok(buf.len())
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

#[cfg(test)]
mod tests {
    use super::{JqueryTerminalWriter, jt_format};
    use numbat::buffered_writer::BufferedWriter;
    use std::io::Write;
    use termcolor::{Color, ColorSpec, WriteColor};

    #[test]
    fn escapes_backslashes_and_brackets_in_formatted_text() {
        assert_eq!(
            jt_format(Some("string"), r"\] [x]"),
            "[[;;;hl-string]&#92;&#93; &#91;x&#93;]"
        );
        assert_eq!(jt_format(None, r"\alpha"), "&#92;alpha");
    }

    #[test]
    fn escapes_html_entities_in_diagnostics() {
        let mut writer = JqueryTerminalWriter::new();
        let input = b"&lt; <tag>";

        assert_eq!(writer.write(input).unwrap(), input.len());
        assert_eq!(writer.to_string(), "&amp;lt; &lt;tag&gt;");
    }

    #[test]
    fn escapes_diagnostic_text_without_breaking_color_codes() {
        let mut writer = JqueryTerminalWriter::new();
        let mut color = ColorSpec::new();
        color.set_fg(Some(Color::Red));
        writer.set_color(&color).unwrap();

        let input = br"foo\] [bar]";
        assert_eq!(writer.write(input).unwrap(), input.len());

        assert_eq!(
            writer.to_string(),
            "[[;;;hl-diagnostic-red]foo&#92;&#93; &#91;bar&#93;]"
        );

        writer.reset().unwrap();

        // Escaping also works when the backslash and bracket arrive separately.
        writer.write_all(b"\\").unwrap();
        writer.write_all(b"]").unwrap();

        assert_eq!(
            writer.to_string(),
            "[[;;;hl-diagnostic-red]foo&#92;&#93; &#91;bar&#93;]&#92;&#93;"
        );
    }
}
