use std::collections::HashMap;

use std::sync::OnceLock;

use crate::interpreter::RuntimeError;
use crate::{ast::ProcedureKind, quantity::Quantity};

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

static FFI_PROCEDURES: OnceLock<HashMap<ProcedureKind, ForeignFunction>> = OnceLock::new();
static FFI_FUNCTIONS: OnceLock<HashMap<&'static str, ForeignFunction>> = OnceLock::new();

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
            "round",
            ForeignFunction {
                name: "round".into(),
                arity: 1..=1,
                callable: Callable::Function(round),
            },
        );
        m.insert(
            "floor",
            ForeignFunction {
                name: "floor".into(),
                arity: 1..=1,
                callable: Callable::Function(floor),
            },
        );
        m.insert(
            "ceil",
            ForeignFunction {
                name: "ceil".into(),
                arity: 1..=1,
                callable: Callable::Function(ceil),
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
            "cos",
            ForeignFunction {
                name: "cos".into(),
                arity: 1..=1,
                callable: Callable::Function(cos),
            },
        );
        m.insert(
            "tan",
            ForeignFunction {
                name: "tan".into(),
                arity: 1..=1,
                callable: Callable::Function(tan),
            },
        );
        m.insert(
            "asin",
            ForeignFunction {
                name: "asin".into(),
                arity: 1..=1,
                callable: Callable::Function(asin),
            },
        );
        m.insert(
            "acos",
            ForeignFunction {
                name: "acos".into(),
                arity: 1..=1,
                callable: Callable::Function(acos),
            },
        );
        m.insert(
            "atan",
            ForeignFunction {
                name: "atan".into(),
                arity: 1..=1,
                callable: Callable::Function(atan),
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

        m.insert(
            "sinh",
            ForeignFunction {
                name: "sinh".into(),
                arity: 1..=1,
                callable: Callable::Function(sinh),
            },
        );
        m.insert(
            "cosh",
            ForeignFunction {
                name: "cosh".into(),
                arity: 1..=1,
                callable: Callable::Function(cosh),
            },
        );
        m.insert(
            "tanh",
            ForeignFunction {
                name: "tanh".into(),
                arity: 1..=1,
                callable: Callable::Function(tanh),
            },
        );
        m.insert(
            "asinh",
            ForeignFunction {
                name: "asinh".into(),
                arity: 1..=1,
                callable: Callable::Function(asinh),
            },
        );
        m.insert(
            "acosh",
            ForeignFunction {
                name: "acosh".into(),
                arity: 1..=1,
                callable: Callable::Function(acosh),
            },
        );
        m.insert(
            "atanh",
            ForeignFunction {
                name: "atanh".into(),
                arity: 1..=1,
                callable: Callable::Function(atanh),
            },
        );

        m.insert(
            "exp",
            ForeignFunction {
                name: "exp".into(),
                arity: 1..=1,
                callable: Callable::Function(exp),
            },
        );
        m.insert(
            "ln",
            ForeignFunction {
                name: "ln".into(),
                arity: 1..=1,
                callable: Callable::Function(ln),
            },
        );
        m.insert(
            "log10",
            ForeignFunction {
                name: "log10".into(),
                arity: 1..=1,
                callable: Callable::Function(log10),
            },
        );
        m.insert(
            "log2",
            ForeignFunction {
                name: "log2".into(),
                arity: 1..=1,
                callable: Callable::Function(log2),
            },
        );

        m.insert(
            "mean",
            ForeignFunction {
                name: "mean".into(),
                arity: 1..=usize::MAX,
                callable: Callable::Function(mean),
            },
        );
        m.insert(
            "maximum",
            ForeignFunction {
                name: "maximum".into(),
                arity: 1..=usize::MAX,
                callable: Callable::Function(maximum),
            },
        );
        m.insert(
            "minimum",
            ForeignFunction {
                name: "minimum".into(),
                arity: 1..=usize::MAX,
                callable: Callable::Function(minimum),
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
            Ok(diff) => match diff.convert_to(args[2].unit()) {
                Err(e) => ControlFlow::Break(RuntimeError::ConversionError(e)),
                Ok(diff_converted) => {
                    if diff_converted.unsafe_value().to_f64().abs()
                        < args[2].unsafe_value().to_f64()
                    {
                        ControlFlow::Continue(())
                    } else {
                        ControlFlow::Break(RuntimeError::AssertEq3Failed(
                            args[0].clone(),
                            args[1].clone(),
                            args[2].clone(),
                        ))
                    }
                }
            },
            Err(e) => ControlFlow::Break(RuntimeError::ConversionError(e)),
        }
    }
}

fn abs(args: &[Quantity]) -> Quantity {
    assert!(args.len() == 1);

    let value = args[0].unsafe_value().to_f64();
    Quantity::new_f64(value.abs(), args[0].unit().clone())
}

fn round(args: &[Quantity]) -> Quantity {
    assert!(args.len() == 1);

    let value = args[0].unsafe_value().to_f64();
    Quantity::new_f64(value.round(), args[0].unit().clone())
}

fn floor(args: &[Quantity]) -> Quantity {
    assert!(args.len() == 1);

    let value = args[0].unsafe_value().to_f64();
    Quantity::new_f64(value.floor(), args[0].unit().clone())
}

fn ceil(args: &[Quantity]) -> Quantity {
    assert!(args.len() == 1);

    let value = args[0].unsafe_value().to_f64();
    Quantity::new_f64(value.ceil(), args[0].unit().clone())
}

fn sin(args: &[Quantity]) -> Quantity {
    assert!(args.len() == 1);

    let input = args[0].as_scalar().unwrap().to_f64();
    Quantity::from_scalar(input.sin())
}

fn cos(args: &[Quantity]) -> Quantity {
    assert!(args.len() == 1);

    let input = args[0].as_scalar().unwrap().to_f64();
    Quantity::from_scalar(input.cos())
}

fn tan(args: &[Quantity]) -> Quantity {
    assert!(args.len() == 1);

    let input = args[0].as_scalar().unwrap().to_f64();
    Quantity::from_scalar(input.tan())
}

fn asin(args: &[Quantity]) -> Quantity {
    assert!(args.len() == 1);

    let input = args[0].as_scalar().unwrap().to_f64();
    Quantity::from_scalar(input.asin())
}

fn acos(args: &[Quantity]) -> Quantity {
    assert!(args.len() == 1);

    let input = args[0].as_scalar().unwrap().to_f64();
    Quantity::from_scalar(input.acos())
}

fn atan(args: &[Quantity]) -> Quantity {
    assert!(args.len() == 1);

    let input = args[0].as_scalar().unwrap().to_f64();
    Quantity::from_scalar(input.atan())
}

fn atan2(args: &[Quantity]) -> Quantity {
    assert!(args.len() == 2);

    let input0 = args[0].unsafe_value().to_f64(); // TODO: properly convert to the same unit here!
    let input1 = args[1].unsafe_value().to_f64();
    Quantity::from_scalar(input0.atan2(input1))
}

fn sinh(args: &[Quantity]) -> Quantity {
    assert!(args.len() == 1);

    let input = args[0].as_scalar().unwrap().to_f64();
    Quantity::from_scalar(input.sinh())
}

fn cosh(args: &[Quantity]) -> Quantity {
    assert!(args.len() == 1);

    let input = args[0].as_scalar().unwrap().to_f64();
    Quantity::from_scalar(input.cosh())
}

fn tanh(args: &[Quantity]) -> Quantity {
    assert!(args.len() == 1);

    let input = args[0].as_scalar().unwrap().to_f64();
    Quantity::from_scalar(input.tanh())
}

fn asinh(args: &[Quantity]) -> Quantity {
    assert!(args.len() == 1);

    let input = args[0].as_scalar().unwrap().to_f64();
    Quantity::from_scalar(input.asinh())
}

fn acosh(args: &[Quantity]) -> Quantity {
    assert!(args.len() == 1);

    let input = args[0].as_scalar().unwrap().to_f64();
    Quantity::from_scalar(input.acosh())
}

fn atanh(args: &[Quantity]) -> Quantity {
    assert!(args.len() == 1);

    let input = args[0].as_scalar().unwrap().to_f64();
    Quantity::from_scalar(input.atanh())
}

fn exp(args: &[Quantity]) -> Quantity {
    assert!(args.len() == 1);

    let input = args[0].as_scalar().unwrap().to_f64();
    Quantity::from_scalar(input.exp())
}

fn ln(args: &[Quantity]) -> Quantity {
    assert!(args.len() == 1);

    let input = args[0].as_scalar().unwrap().to_f64();
    Quantity::from_scalar(input.ln())
}

fn log10(args: &[Quantity]) -> Quantity {
    assert!(args.len() == 1);

    let input = args[0].as_scalar().unwrap().to_f64();
    Quantity::from_scalar(input.log10())
}

fn log2(args: &[Quantity]) -> Quantity {
    assert!(args.len() == 1);

    let input = args[0].as_scalar().unwrap().to_f64();
    Quantity::from_scalar(input.log2())
}

fn mean(args: &[Quantity]) -> Quantity {
    assert!(!args.is_empty());

    let output_unit = args[0].unit();
    Quantity::new_f64(
        args.iter()
            .map(|q| q.convert_to(output_unit).unwrap().unsafe_value().to_f64())
            .sum::<f64>()
            / (args.len() as f64),
        output_unit.clone(),
    )
}

fn maximum(args: &[Quantity]) -> Quantity {
    assert!(!args.is_empty());

    let output_unit = args[0].unit();
    Quantity::new(
        args.iter()
            .map(|q| *q.convert_to(output_unit).unwrap().unsafe_value())
            .max_by(|l, r| l.partial_cmp(r).unwrap())
            .unwrap(),
        output_unit.clone(),
    )
}

fn minimum(args: &[Quantity]) -> Quantity {
    assert!(!args.is_empty());

    let output_unit = args[0].unit();
    Quantity::new(
        args.iter()
            .map(|q| *q.convert_to(output_unit).unwrap().unsafe_value())
            .min_by(|l, r| l.partial_cmp(r).unwrap())
            .unwrap(),
        output_unit.clone(),
    )
}
