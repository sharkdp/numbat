use std::collections::HashMap;

use once_cell::sync::OnceCell;

use crate::interpreter::RuntimeError;
use crate::{ast::ProcedureKind, number::Number, quantity::Quantity};

type ControlFlow = std::ops::ControlFlow<RuntimeError>;

pub(crate) type ArityRange = std::ops::RangeInclusive<usize>;

#[derive(Clone)]
pub(crate) enum Callable {
    Function(fn(&[Quantity]) -> Quantity),
    Procedure(fn(&[Quantity]) -> ControlFlow),
}

#[derive(Clone)]
pub(crate) struct ForeignFunction {
    pub(crate) name: String,
    pub(crate) arity: ArityRange,
    pub(crate) callable: Callable,
}

static FFI_PROCEDURES: OnceCell<HashMap<ProcedureKind, ForeignFunction>> = OnceCell::new();
static FFI_FUNCTIONS: OnceCell<HashMap<&'static str, ForeignFunction>> = OnceCell::new();

pub(crate) fn procedures() -> &'static HashMap<ProcedureKind, ForeignFunction> {
    FFI_PROCEDURES.get_or_init(|| {
        let mut m = HashMap::new();

        m.insert(
            ProcedureKind::Print,
            ForeignFunction {
                name: "print".into(),
                arity: 1..=1,
                callable: Callable::Procedure(print),
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

        m
    })
}

pub(crate) fn functions() -> &'static HashMap<&'static str, ForeignFunction> {
    FFI_FUNCTIONS.get_or_init(|| {
        let mut m = HashMap::new();

        m.insert(
            "abs",
            ForeignFunction {
                name: "abs".into(),
                arity: 1..=1,
                callable: Callable::Function(abs),
            },
        );
        m.insert(
            "sin",
            ForeignFunction {
                name: "sin".into(),
                arity: 1..=1,
                callable: Callable::Function(sin),
            },
        );
        m.insert(
            "atan2",
            ForeignFunction {
                name: "atan2".into(),
                arity: 2..=2,
                callable: Callable::Function(atan2),
            },
        );

        m
    })
}

fn print(args: &[Quantity]) -> ControlFlow {
    assert!(args.len() == 1);

    println!("{}", args[0]);

    ControlFlow::Continue(())
}

fn assert_eq(args: &[Quantity]) -> ControlFlow {
    assert!(args.len() == 2 || args.len() == 3);

    // TODO: make sure that phys. dimension line up

    if args.len() == 2 {
        if args[0] == args[1] {
            ControlFlow::Continue(())
        } else {
            ControlFlow::Break(RuntimeError::AssertEq2Failed(
                args[0].clone(),
                args[1].clone(),
            ))
        }
    } else {
        let result = &args[0] - &args[1];

        match result {
            Ok(diff) => {
                if let Err(e) = args[2].convert_to(diff.unit()) {
                    return ControlFlow::Break(RuntimeError::ConversionError(e));
                }

                if diff.unsafe_value().to_f64().abs() < args[2].unsafe_value().to_f64() {
                    ControlFlow::Continue(())
                } else {
                    ControlFlow::Break(RuntimeError::AssertEq3Failed(
                        args[0].clone(),
                        args[1].clone(),
                        args[2].clone(),
                    ))
                }
            }
            Err(e) => ControlFlow::Break(RuntimeError::ConversionError(e)),
        }
    }
}

fn abs(args: &[Quantity]) -> Quantity {
    assert!(args.len() == 1);

    let value = args[0].unsafe_value().to_f64();
    Quantity::new(Number::from_f64(value.abs()), args[0].unit().clone())
}

fn sin(args: &[Quantity]) -> Quantity {
    assert!(args.len() == 1);

    let input = args[0].as_scalar().unwrap().to_f64();
    Quantity::from_scalar(input.sin())
}

fn atan2(args: &[Quantity]) -> Quantity {
    assert!(args.len() == 2);

    let input0 = args[0].unsafe_value().to_f64();
    let input1 = args[1].unsafe_value().to_f64();
    Quantity::from_scalar(input0.atan2(input1))
}
