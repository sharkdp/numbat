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

use crate::interpreter::RuntimeError;
use crate::span::Span;
use crate::value::Value;
use crate::vm::ExecutionContext;

type ControlFlow = std::ops::ControlFlow<RuntimeError>;

pub(crate) type ArityRange = std::ops::RangeInclusive<usize>;

type Result<T> = std::result::Result<T, Box<RuntimeError>>;

pub(crate) type Args = VecDeque<Value>;

pub(crate) enum Callable {
    Function(fn(Args) -> Result<Value>),
    Procedure(fn(&mut ExecutionContext, Args, Vec<Span>) -> ControlFlow),
}

pub(crate) struct ForeignFunction {
    pub(crate) arity: ArityRange,
    pub(crate) callable: Callable,
}

pub(crate) use functions::functions;
pub(crate) use procedures::procedures;
