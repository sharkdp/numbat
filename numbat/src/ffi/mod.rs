mod currency;
mod datetime;
mod functions;
mod lists;
mod lookup;
mod macros;
mod math;
mod plot;
mod procedures;
mod strings;

use std::collections::VecDeque;

use crate::interpreter::{RuntimeError, RuntimeErrorKind};
use crate::span::Span;
use crate::value::Value;
use crate::vm::ExecutionContext;

type ControlFlow = std::ops::ControlFlow<RuntimeErrorKind>;

pub(crate) type ArityRange = std::ops::RangeInclusive<usize>;

type Result<T, E = Box<RuntimeError>> = std::result::Result<T, E>;

pub(crate) type Args = VecDeque<Value>;

pub(crate) enum Callable {
    Function(fn(Args) -> Result<Value, Box<RuntimeErrorKind>>),
    Procedure(fn(&mut ExecutionContext, Args, Vec<Span>) -> ControlFlow),
}

pub(crate) struct ForeignFunction {
    pub(crate) arity: ArityRange,
    pub(crate) callable: Callable,
}

pub(crate) use functions::functions;
pub(crate) use procedures::procedures;
