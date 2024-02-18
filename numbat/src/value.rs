use crate::{pretty_print::PrettyPrint, quantity::Quantity};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FunctionReference {
    Foreign(String),
    Normal(String),
    // TODO: We can get rid of this variant once we implement closures:
    TzConversion(String),
}

impl std::fmt::Display for FunctionReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionReference::Foreign(name) => write!(f, "<builtin function: {name}>"),
            FunctionReference::Normal(name) => write!(f, "<function: {name}>"),
            FunctionReference::TzConversion(tz) => {
                write!(f, "<builtin timezone conversion function: {tz}>")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Quantity(Quantity),
    Boolean(bool),
    String(String),
    /// A DateTime with an associated offset used when pretty printing
    DateTime(chrono::DateTime<chrono::Utc>, chrono::FixedOffset),
    FunctionReference(FunctionReference),
}

impl Value {
    #[track_caller]
    pub fn unsafe_as_quantity(&self) -> &Quantity {
        if let Value::Quantity(q) = self {
            q
        } else {
            panic!("Expected value to be a quantity");
        }
    }

    #[track_caller]
    pub fn unsafe_as_bool(&self) -> bool {
        if let Value::Boolean(b) = self {
            *b
        } else {
            panic!("Expected value to be a bool");
        }
    }

    #[track_caller]
    pub fn unsafe_as_string(&self) -> &str {
        if let Value::String(s) = self {
            s
        } else {
            panic!("Expected value to be a string");
        }
    }

    #[track_caller]
    pub fn unsafe_as_datetime(&self) -> &chrono::DateTime<chrono::Utc> {
        if let Value::DateTime(dt, _) = self {
            dt
        } else {
            panic!("Expected value to be a string");
        }
    }

    #[track_caller]
    pub fn unsafe_as_function_reference(&self) -> &FunctionReference {
        if let Value::FunctionReference(inner) = self {
            inner
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
            Value::DateTime(dt, _) => write!(f, "datetime(\"{}\")", dt),
            Value::FunctionReference(r) => write!(f, "{}", r),
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
            Value::FunctionReference(r) => crate::markup::string(r.to_string()),
        }
    }
}
