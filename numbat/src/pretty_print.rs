use compact_str::CompactString;

use crate::markup::Markup;

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
            '\\' => r"\\",
            '\n' => r"\n",
            '\r' => r"\r",
            '\t' => r"\t",
            '"' => r#"\""#,
            '\0' => r"\0",
            '{' => r"\{",
            '}' => r"\}",
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
