use std::sync::Arc;

use itertools::Itertools;

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
    DateTime(chrono::DateTime<chrono::FixedOffset>),
    FunctionReference(FunctionReference),
    FormatSpecifiers(Option<String>),
    Struct(Arc<[(String, usize)]>, Vec<Value>),
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
    pub fn unsafe_as_datetime(&self) -> &chrono::DateTime<chrono::FixedOffset> {
        if let Value::DateTime(dt) = self {
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
            Value::DateTime(dt) => write!(f, "datetime(\"{}\")", dt),
            Value::FunctionReference(r) => write!(f, "{}", r),
            Value::FormatSpecifiers(_) => write!(f, "<format specfiers>"),
            Value::Struct(field_meta, values) => write!(
                f,
                "${{ {} }}",
                field_meta
                    .iter()
                    .map(|(name, idx)| name.to_owned() + ": " + &values[*idx].to_string())
                    .join(", ")
            ),
        }
    }
}

impl PrettyPrint for Value {
    fn pretty_print(&self) -> crate::markup::Markup {
        match self {
            Value::Quantity(q) => q.pretty_print(),
            Value::Boolean(b) => b.pretty_print(),
            Value::String(s) => s.pretty_print(),
            Value::DateTime(dt) => crate::markup::string(crate::datetime::to_rfc2822_save(dt)),
            Value::FunctionReference(r) => crate::markup::string(r.to_string()),
            Value::FormatSpecifiers(Some(s)) => crate::markup::string(s.to_string()),
            Value::FormatSpecifiers(None) => crate::markup::empty(),
            Value::Struct(field_meta, values) => {
                crate::markup::operator("${")
                    + itertools::Itertools::intersperse(
                        field_meta.iter().map(|(name, idx)| {
                            let val = &values[*idx];

                            crate::markup::identifier(name)
                                + crate::markup::operator(":")
                                + crate::markup::space()
                                + val.pretty_print()
                        }),
                        crate::markup::operator(",") + crate::markup::space(),
                    )
                    .sum()
                    + crate::markup::operator("}")
            }
        }
    }
}
