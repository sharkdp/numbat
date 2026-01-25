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
use crate::unit::Unit;
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

/// Context passed to FFI functions, providing access to runtime state.
pub struct FfiContext<'a, 'b> {
    pub ctx: &'a mut ExecutionContext<'b>,
    constants: &'a [Constant],
}

impl<'a, 'b> FfiContext<'a, 'b> {
    pub fn new(ctx: &'a mut ExecutionContext<'b>, constants: &'a [Constant]) -> Self {
        Self { ctx, constants }
    }

    /// Look up a unit by name from the constants table.
    pub fn lookup_unit(&self, name: &str) -> Option<Unit> {
        self.ctx
            .unit_name_to_constant_idx
            .get(name)
            .and_then(|&idx| self.constants.get(idx as usize))
            .and_then(|c| match c {
                Constant::Unit(u) => Some(u.clone()),
                _ => None,
            })
    }
}

/// FFI function signature: (ffi_context, args, return_type) -> Result
type FfiFunctionFn = fn(&mut FfiContext, Args, &TypeScheme) -> Result<Value, Box<RuntimeErrorKind>>;

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
