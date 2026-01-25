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
use crate::typechecker::type_scheme::TypeScheme;
use crate::value::Value;
use crate::vm::{Constant, ExecutionContext};

type ControlFlow = std::ops::ControlFlow<RuntimeErrorKind>;

pub(crate) type ArityRange = std::ops::RangeInclusive<usize>;

type Result<T, E = Box<RuntimeError>> = std::result::Result<T, E>;

pub(crate) struct Arg {
    pub value: Value,
    pub type_: TypeScheme,
    pub span: Span,
}

pub(crate) type Args = VecDeque<Arg>;

/// FFI function signature: (context, constants, args, return_type) -> Result
type FfiFunctionFn = fn(
    &mut ExecutionContext,
    &[Constant],
    Args,
    &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>>;

pub(crate) enum Callable {
    Function(FfiFunctionFn),
    Procedure(fn(&mut ExecutionContext, Args) -> ControlFlow),
}

pub(crate) struct ForeignFunction {
    pub(crate) arity: ArityRange,
    pub(crate) callable: Callable,
}

pub(crate) use functions::functions;
pub(crate) use procedures::procedures;
