use compact_str::CompactString;

use crate::markup::Markup;

/// Options for formatting output values.
#[derive(Debug, Clone)]
pub struct FormatOptions {
    /// Digit separator for large integers (e.g., "_" or ","). Empty to disable.
    pub digit_separator: String,

    /// Minimum number of digits before adding separators.
    pub digit_grouping_threshold: usize,

    /// Maximum significant digits for floating-point display.
    pub significant_digits: usize,

    /// The strftime format string for DateTime values.
    pub datetime_format: String,
}

impl Default for FormatOptions {
    fn default() -> Self {
        Self {
            digit_separator: "_".to_string(),
            digit_grouping_threshold: 6,
            significant_digits: 6,
            datetime_format: crate::datetime::DEFAULT_DATETIME_FORMAT.to_string(),
        }
    }
}

pub trait PrettyPrint {
    /// Pretty prints with default options.
    fn pretty_print(&self) -> Markup;
}

impl PrettyPrint for bool {
    fn pretty_print(&self) -> Markup {
        crate::markup::keyword(if *self { "true" } else { "false" })
    }
}

pub fn escape_numbat_string(s: &str) -> CompactString {
    let mut out = CompactString::const_new("");
    for c in s.chars() {
        let replacement = match c {
            '\n' => r"\n",
            '\r' => r"\r",
            '\t' => r"\t",
            '"' => r#"\""#,
            '\0' => r"\0",
            '{' | '}' | '\\' => {
                out.push(c);
                out.push(c);
                continue;
            }
            _ => {
                out.push(c);
                continue;
            }
        };
        out.push_str(replacement);
    }
    out
}

impl PrettyPrint for CompactString {
    fn pretty_print(&self) -> Markup {
        crate::markup::operator("\"")
            + crate::markup::string(escape_numbat_string(self))
            + crate::markup::operator("\"")
    }
}
