mod datetime;
mod functions;
mod lists;
mod macros;
mod math;
mod procedures;
mod strings;

use crate::interpreter::RuntimeError;
use crate::value::Value;
use crate::vm::ExecutionContext;

type ControlFlow = std::ops::ControlFlow<RuntimeError>;

pub(crate) type ArityRange = std::ops::RangeInclusive<usize>;

type Result<T> = std::result::Result<T, RuntimeError>;

type BoxedFunction = Box<dyn Fn(&[Value]) -> Result<Value> + Send + Sync>;

pub(crate) enum Callable {
    Function(BoxedFunction),
    Procedure(fn(&mut ExecutionContext, &[Value]) -> ControlFlow),
}

pub(crate) struct ForeignFunction {
    pub(crate) name: String,
    pub(crate) arity: ArityRange,
    pub(crate) callable: Callable,
}

pub(crate) use functions::functions;
pub(crate) use procedures::procedures;
