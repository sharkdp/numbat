use crate::{pretty_print::PrettyPrint, quantity::Quantity};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Quantity(Quantity),
    Boolean(bool),
}

impl Value {
    pub fn unsafe_as_quantity(self) -> Quantity {
        if let Value::Quantity(q) = self {
            q
        } else {
            panic!("Expected value to be a quantity");
        }
    }

    pub fn unsafe_as_bool(self) -> bool {
        if let Value::Boolean(b) = self {
            b
        } else {
            panic!("Expected value to be a bool");
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Quantity(q) => write!(f, "{}", q),
            Value::Boolean(b) => write!(f, "{}", b),
        }
    }
}

impl PrettyPrint for Value {
    fn pretty_print(&self) -> crate::markup::Markup {
        match self {
            Value::Quantity(q) => q.pretty_print(),
            Value::Boolean(b) => b.pretty_print(),
        }
    }
}
