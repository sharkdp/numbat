use std::collections::HashMap;

use std::sync::OnceLock;

use super::macros::*;
use crate::{
    ast::ProcedureKind, ffi::ControlFlow, pretty_print::PrettyPrint, value::Value,
    vm::ExecutionContext, RuntimeError,
};

use super::{Args, Callable, ForeignFunction};

static FFI_PROCEDURES: OnceLock<HashMap<ProcedureKind, ForeignFunction>> = OnceLock::new();

pub(crate) fn procedures() -> &'static HashMap<ProcedureKind, ForeignFunction> {
    FFI_PROCEDURES.get_or_init(|| {
        let mut m = HashMap::new();

        m.insert(
            ProcedureKind::Print,
            ForeignFunction {
                name: "print".into(),
                arity: 0..=1,
                callable: Callable::Procedure(print),
            },
        );
        m.insert(
            ProcedureKind::Assert,
            ForeignFunction {
                name: "assert".into(),
                arity: 1..=1,
                callable: Callable::Procedure(assert),
            },
        );
        m.insert(
            ProcedureKind::AssertEq,
            ForeignFunction {
                name: "assert_eq".into(),
                arity: 2..=3,
                callable: Callable::Procedure(assert_eq),
            },
        );
        // Note: The 'type' procedure is missing here because it has special handling code in the compiler

        m
    })
}

fn print(ctx: &mut ExecutionContext, mut args: Args) -> ControlFlow {
    assert!(args.len() <= 1);

    if args.is_empty() {
        (ctx.print_fn)(&crate::markup::text(""))
    } else {
        match arg!(args) {
            Value::String(string) => (ctx.print_fn)(&crate::markup::text(string)), // print string without quotes
            arg => (ctx.print_fn)(&arg.pretty_print()),
        }
    }

    ControlFlow::Continue(())
}

fn assert(_: &mut ExecutionContext, mut args: Args) -> ControlFlow {
    assert!(args.len() == 1);

    if arg!(args).unsafe_as_bool() {
        ControlFlow::Continue(())
    } else {
        ControlFlow::Break(RuntimeError::AssertFailed)
    }
}

fn assert_eq(_: &mut ExecutionContext, mut args: Args) -> ControlFlow {
    assert!(args.len() == 2 || args.len() == 3);

    if args.len() == 2 {
        let lhs = arg!(args);
        let rhs = arg!(args);

        let error = ControlFlow::Break(RuntimeError::AssertEq2Failed(lhs.clone(), rhs.clone()));

        if lhs.is_quantity() {
            let lhs = lhs.unsafe_as_quantity();
            let rhs = rhs.unsafe_as_quantity();

            if let Ok(args1_converted) = rhs.convert_to(lhs.unit()) {
                if lhs == args1_converted {
                    ControlFlow::Continue(())
                } else {
                    error
                }
            } else {
                error
            }
        } else {
            if lhs == rhs {
                ControlFlow::Continue(())
            } else {
                error
            }
        }
    } else {
        let lhs = quantity_arg!(args);
        let rhs = quantity_arg!(args);
        let result = &lhs - &rhs;
        let eps = quantity_arg!(args);

        match result {
            Ok(diff) => match diff.convert_to(eps.unit()) {
                Err(e) => ControlFlow::Break(RuntimeError::QuantityError(e)),
                Ok(diff_converted) => {
                    if diff_converted.unsafe_value().to_f64().abs() < eps.unsafe_value().to_f64() {
                        ControlFlow::Continue(())
                    } else {
                        ControlFlow::Break(RuntimeError::AssertEq3Failed(
                            lhs.clone(),
                            rhs.clone(),
                            eps.clone(),
                        ))
                    }
                }
            },
            Err(e) => ControlFlow::Break(RuntimeError::QuantityError(e)),
        }
    }
}
