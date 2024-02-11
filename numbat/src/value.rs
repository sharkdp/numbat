use crate::{pretty_print::PrettyPrint, quantity::Quantity};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Quantity(Quantity),
    Boolean(bool),
    String(String),
    /// A DateTime with an associated offset used when pretty printing
    DateTime(chrono::DateTime<chrono::Utc>, chrono::FixedOffset),
}

impl Value {
    pub fn unsafe_as_quantity(&self) -> &Quantity {
        if let Value::Quantity(q) = self {
            q
        } else {
            panic!("Expected value to be a quantity");
        }
    }

    pub fn unsafe_as_bool(&self) -> bool {
        if let Value::Boolean(b) = self {
            *b
        } else {
            panic!("Expected value to be a bool");
        }
    }

    pub fn unsafe_as_string(&self) -> &str {
        if let Value::String(s) = self {
            s
        } else {
            panic!("Expected value to be a string");
        }
    }

    pub fn unsafe_as_datetime(&self) -> &chrono::DateTime<chrono::Utc> {
        if let Value::DateTime(dt, _) = self {
            dt
        } else {
            panic!("Expected value to be a string");
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Quantity(q) => write!(f, "{}", q),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::DateTime(dt, _) => write!(f, "{:?}", dt),
        }
    }
}

impl PrettyPrint for Value {
    fn pretty_print(&self) -> crate::markup::Markup {
        match self {
            Value::Quantity(q) => q.pretty_print(),
            Value::Boolean(b) => b.pretty_print(),
            Value::String(s) => s.pretty_print(),
            Value::DateTime(dt, offset) => {
                let l: chrono::DateTime<chrono::FixedOffset> =
                    chrono::DateTime::from_naive_utc_and_offset(dt.naive_utc(), *offset);
                crate::markup::string(l.to_rfc2822())
            }
        }
    }
}
