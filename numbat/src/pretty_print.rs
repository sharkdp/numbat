use crate::markup::Markup;

pub trait PrettyPrint {
    fn pretty_print(&self) -> Markup;
}
