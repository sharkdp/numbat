use std::sync::Arc;

use itertools::Itertools;
use jiff::Zoned;

use crate::{
    list::NumbatList, pretty_print::PrettyPrint, quantity::Quantity, typed_ast::StructInfo,
};

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
    DateTime(Zoned),
    FunctionReference(FunctionReference),
    FormatSpecifiers(Option<String>),
    StructInstance(Arc<StructInfo>, Vec<Value>),
    List(NumbatList<Value>),
}

impl Value {
    #[track_caller]
    pub fn unsafe_as_quantity(self) -> Quantity {
        if let Value::Quantity(q) = self {
            q
        } else {
            panic!("Expected value to be a quantity");
        }
    }

    #[track_caller]
    pub fn unsafe_as_bool(self) -> bool {
        if let Value::Boolean(b) = self {
            b
        } else {
            panic!("Expected value to be a bool");
        }
    }

    #[track_caller]
    pub fn unsafe_as_string(self) -> String {
        if let Value::String(s) = self {
            s
        } else {
            panic!("Expected value to be a string");
        }
    }

    #[track_caller]
    pub fn unsafe_as_datetime(self) -> jiff::Zoned {
        if let Value::DateTime(dt) = self {
            dt
        } else {
            panic!("Expected value to be a string");
        }
    }

    #[track_caller]
    pub fn unsafe_as_function_reference(self) -> FunctionReference {
        if let Value::FunctionReference(inner) = self {
            inner
        } else {
            panic!("Expected value to be a string");
        }
    }

    #[track_caller]
    pub fn unsafe_as_struct_fields(self) -> Vec<Value> {
        if let Value::StructInstance(_, values) = self {
            values
        } else {
            panic!("Expected value to be a struct");
        }
    }

    #[track_caller]
    pub fn unsafe_as_list(self) -> NumbatList<Value> {
        if let Value::List(values) = self {
            values
        } else {
            panic!("Expected value to be a list");
        }
    }

    pub(crate) fn is_quantity(&self) -> bool {
        matches!(self, Value::Quantity(_))
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Quantity(q) => write!(f, "{q}"),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::String(s) => write!(f, "\"{s}\""),
            Value::DateTime(dt) => write!(f, "datetime(\"{dt}\")"),
            Value::FunctionReference(r) => write!(f, "{r}"),
            Value::FormatSpecifiers(_) => write!(f, "<format specfiers>"),
            Value::StructInstance(struct_info, values) => write!(
                f,
                "{} {{{}}}",
                struct_info.name,
                if values.is_empty() {
                    "".to_owned()
                } else {
                    format!(
                        " {} ",
                        struct_info
                            .fields
                            .keys()
                            .zip(values)
                            .map(|(name, value)| name.to_owned() + ": " + &value.to_string())
                            .join(", ")
                    )
                }
            ),
            Value::List(elements) => write!(
                f,
                "[{}]",
                elements
                    .iter()
                    .map(|element| element.to_string())
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
            Value::DateTime(dt) => crate::markup::string(crate::datetime::to_string(dt)),
            Value::FunctionReference(r) => crate::markup::string(r.to_string()),
            Value::FormatSpecifiers(Some(s)) => crate::markup::string(s),
            Value::FormatSpecifiers(None) => crate::markup::empty(),
            Value::StructInstance(struct_info, values) => {
                crate::markup::type_identifier(struct_info.name.clone())
                    + crate::markup::space()
                    + crate::markup::operator("{")
                    + if values.is_empty() {
                        crate::markup::empty()
                    } else {
                        crate::markup::space()
                            + itertools::Itertools::intersperse(
                                struct_info.fields.keys().zip(values).map(|(name, val)| {
                                    crate::markup::identifier(name)
                                        + crate::markup::operator(":")
                                        + crate::markup::space()
                                        + val.pretty_print()
                                }),
                                crate::markup::operator(",") + crate::markup::space(),
                            )
                            .sum()
                            + crate::markup::space()
                    }
                    + crate::markup::operator("}")
            }
            Value::List(elements) => {
                crate::markup::operator("[")
                    + itertools::Itertools::intersperse(
                        elements.iter().map(|element| element.pretty_print()),
                        crate::markup::operator(",") + crate::markup::space(),
                    )
                    .sum()
                    + crate::markup::operator("]")
            }
        }
    }
}
