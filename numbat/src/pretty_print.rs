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

pub fn escape_numbat_string(s: &str) -> String {
    s.replace("\\", "\\\\")
        .replace("\n", "\\n")
        .replace("\r", "\\r")
        .replace("\t", "\\t")
        .replace("\"", "\\\"")
        .replace("\0", "\\0")
        .replace("{", "\\{")
        .replace("}", "\\}")
}

impl PrettyPrint for String {
    fn pretty_print(&self) -> Markup {
        crate::markup::operator("\"")
            + crate::markup::string(escape_numbat_string(self))
            + crate::markup::operator("\"")
    }
}
