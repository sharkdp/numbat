use std::collections::HashMap;

use std::sync::OnceLock;

use super::macros::*;
use crate::{
    ast::ProcedureKind,
    ffi::ControlFlow,
    interpreter::{
        RuntimeErrorKind,
        assert_eq::{AssertEq2Error, AssertEq3Error},
    },
    pretty_print::PrettyPrint,
    span::Span,
    value::Value,
    vm::ExecutionContext,
};

use super::{Args, Callable, ForeignFunction};

static FFI_PROCEDURES: OnceLock<HashMap<ProcedureKind, ForeignFunction>> = OnceLock::new();

pub(crate) fn procedures() -> &'static HashMap<ProcedureKind, ForeignFunction> {
    FFI_PROCEDURES.get_or_init(|| {
        let mut m = HashMap::new();

        m.insert(
            ProcedureKind::Print,
            ForeignFunction {
                arity: 0..=1,
                callable: Callable::Procedure(print),
            },
        );
        m.insert(
            ProcedureKind::Assert,
            ForeignFunction {
                arity: 1..=1,
                callable: Callable::Procedure(assert),
            },
        );
        m.insert(
            ProcedureKind::AssertEq,
            ForeignFunction {
                arity: 2..=3,
                callable: Callable::Procedure(assert_eq),
            },
        );
        // Note: The 'type' procedure is missing here because it has special handling code in the compiler

        m
    })
}

fn print(ctx: &mut ExecutionContext, mut args: Args, _: Vec<Span>) -> ControlFlow {
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

fn assert(_: &mut ExecutionContext, mut args: Args, arg_spans: Vec<Span>) -> ControlFlow {
    assert!(args.len() == 1);

    if arg!(args).unsafe_as_bool() {
        ControlFlow::Continue(())
    } else {
        ControlFlow::Break(RuntimeErrorKind::AssertFailed(arg_spans[0]))
    }
}

fn assert_eq(_: &mut ExecutionContext, mut args: Args, arg_spans: Vec<Span>) -> ControlFlow {
    assert!(args.len() == 2 || args.len() == 3);

    let span_lhs = arg_spans[0];
    let span_rhs = arg_spans[1];

    if args.len() == 2 {
        let lhs = arg!(args);
        let rhs = arg!(args);

        let error = ControlFlow::Break(RuntimeErrorKind::AssertEq2Failed(AssertEq2Error {
            span_lhs,
            lhs: lhs.clone(),
            span_rhs,
            rhs: rhs.clone(),
        }));

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
        } else if lhs == rhs {
            ControlFlow::Continue(())
        } else {
            error
        }
    } else {
        let lhs_original = quantity_arg!(args);
        let rhs_original = quantity_arg!(args);
        let eps = quantity_arg!(args);

        let lhs_converted = lhs_original.convert_to(eps.unit());
        let lhs_converted = match lhs_converted {
            Err(e) => return ControlFlow::Break(RuntimeErrorKind::QuantityError(e)),
            Ok(q) => q,
        };
        let rhs_converted = rhs_original.convert_to(eps.unit());
        let rhs_converted = match rhs_converted {
            Err(e) => return ControlFlow::Break(RuntimeErrorKind::QuantityError(e)),
            Ok(q) => q,
        };

        let result = &lhs_converted - &rhs_converted;

        match result {
            Err(e) => ControlFlow::Break(RuntimeErrorKind::QuantityError(e)),
            Ok(diff) => {
                let diff_abs = diff.abs();
                if diff_abs <= eps {
                    ControlFlow::Continue(())
                } else {
                    ControlFlow::Break(RuntimeErrorKind::AssertEq3Failed(AssertEq3Error {
                        span_lhs,
                        lhs_original,
                        lhs_converted,
                        span_rhs,
                        rhs_original,
                        rhs_converted,
                        eps,
                        diff_abs,
                    }))
                }
            }
        }
    }
}
