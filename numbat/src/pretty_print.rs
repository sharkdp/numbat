use crate::markup::Markup;

pub trait PrettyPrint {
    fn pretty_print(&self) -> Markup;
}

impl PrettyPrint for bool {
    fn pretty_print(&self) -> Markup {
        crate::markup::keyword(if *self { "true" } else { "false" })
    }
}
