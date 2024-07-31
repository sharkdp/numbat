use std::{collections::VecDeque, fmt, sync::Arc};

use itertools::Itertools;

use crate::{pretty_print::PrettyPrint, quantity::Quantity, typed_ast::StructInfo, RuntimeError};

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

pub type NumbatList<T> = VecDeque<T>;

/// Reference counted list / list view
#[derive(Clone, Eq)]
pub struct ArcListView<T> {
    /// The original alloc shared between all values
    alloc: Arc<VecDeque<T>>,
    /// The indexes accessible to us. If `None` we own the whole allocation
    view: Option<(usize, usize)>,
}

impl<T: fmt::Debug + Clone> fmt::Debug for ArcListView<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}

impl<T: PartialEq> PartialEq for ArcListView<T> {
    fn eq(&self, other: &Self) -> bool {
        // Best case scenario, the slice don't have the same len and we can early exit
        if self.len() != other.len() {
            return false;
        }
        // Second best case scenario, the slice comes from the same allocation
        // and have the same view => they are equal
        if Arc::as_ptr(&self.alloc) == Arc::as_ptr(&other.alloc) && self.view == other.view {
            true
        } else {
            // Worst case scenario, we need to compare all the elements one by one
            self.iter().zip(other.iter()).all(|(l, r)| l == r)
        }
    }
}

impl<T> ArcListView<T> {
    pub fn len(&self) -> usize {
        if let Some(view) = self.view {
            view.1 - view.0
        } else {
            self.alloc.len()
        }
    }

    pub fn front(&self) -> Option<&T> {
        if let Some(view) = self.view {
            self.alloc.get(view.0)
        } else {
            self.alloc.front()
        }
    }

    /// Advance the view you have of the list by one.
    pub fn advance_view(&mut self) -> Result<(), RuntimeError> {
        if self.len() == 0 {
            return Err(RuntimeError::EmptyList);
        }
        if let Some(view) = &mut self.view {
            view.0 += 1;
            // should be ensured by the if above
            debug_assert!(view.0 <= view.1);
        } else {
            self.view = Some((1, self.len()));
        }
        Ok(())
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.alloc
            .iter()
            .skip(self.view.map_or(0, |(start, _end)| start))
            .take(
                self.view
                    .map_or(self.alloc.len(), |(start, end)| end - start),
            )
    }
}

impl<T: Clone> ArcListView<T> {
    /// Allocate iif the list being used by another value at the same time
    pub fn push_front(&mut self, element: T) {
        let inner = Arc::make_mut(&mut self.alloc);
        if let Some((start, end)) = &mut self.view {
            // if we were alone on the allocation and had a view of the inner allocation
            // we can keep the allocation and overwrite the start-1 element.
            // but we need to take care of the special case where the start is 0.
            if *start == 0 {
                inner.push_front(element);
                *end += 1;
            } else {
                *start -= 1;
                inner[*start] = element;
            }
        } else {
            inner.push_front(element);
        }
    }
}

impl From<ArcListView<Value>> for Value {
    fn from(list: ArcListView<Value>) -> Self {
        Value::List(list)
    }
}

impl From<VecDeque<Value>> for Value {
    fn from(list: VecDeque<Value>) -> Self {
        Value::List(ArcListView {
            alloc: Arc::new(list),
            view: None,
        })
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
    StructInstance(Arc<StructInfo>, Vec<Value>),
    List(ArcListView<Value>),
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
    pub fn unsafe_as_datetime(self) -> chrono::DateTime<chrono::FixedOffset> {
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
    pub fn unsafe_as_list(self) -> ArcListView<Value> {
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
            Value::Quantity(q) => write!(f, "{}", q),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::DateTime(dt) => write!(f, "datetime(\"{}\")", dt),
            Value::FunctionReference(r) => write!(f, "{}", r),
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
            Value::DateTime(dt) => crate::markup::string(crate::datetime::to_rfc2822_save(dt)),
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
