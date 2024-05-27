use crate::markup::Markup;

pub trait PrettyPrint {
    fn pretty_print(&self) -> Markup;
}

impl PrettyPrint for bool {
    fn pretty_print(&self) -> Markup {
        crate::markup::keyword(if *self { "true" } else { "false" })
    }
}

impl PrettyPrint for String {
    fn pretty_print(&self) -> Markup {
        crate::markup::operator("\"") + crate::markup::string(self) + crate::markup::operator("\"")
    }
}
