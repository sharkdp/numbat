use std::collections::HashMap;

use std::sync::OnceLock;

use crate::currency::ExchangeRatesCache;
use crate::interpreter::RuntimeError;
use crate::{ast::ProcedureKind, quantity::Quantity};

type ControlFlow = std::ops::ControlFlow<RuntimeError>;

pub(crate) type ArityRange = std::ops::RangeInclusive<usize>;

pub(crate) enum Callable {
    Function(Box<dyn Fn(&[Quantity]) -> Quantity + Send + Sync>),
    Procedure(fn(&[Quantity]) -> ControlFlow),
}

pub(crate) struct ForeignFunction {
    pub(crate) name: String,
    pub(crate) arity: ArityRange,
    pub(crate) callable: Callable,
}

static FFI_PROCEDURES: OnceLock<HashMap<ProcedureKind, ForeignFunction>> = OnceLock::new();
static FFI_FUNCTIONS: OnceLock<HashMap<String, ForeignFunction>> = OnceLock::new();

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
        // Note: The 'type' procedure is missing here because it has special handling code in the compiler

        m
    })
}

pub(crate) fn functions() -> &'static HashMap<String, ForeignFunction> {
    FFI_FUNCTIONS.get_or_init(|| {
        let mut m = HashMap::new();

        m.insert(
            "abs".to_string(),
            ForeignFunction {
                name: "abs".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(abs)),
            },
        );
        m.insert(
            "round".to_string(),
            ForeignFunction {
                name: "round".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(round)),
            },
        );
        m.insert(
            "floor".to_string(),
            ForeignFunction {
                name: "floor".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(floor)),
            },
        );
        m.insert(
            "ceil".to_string(),
            ForeignFunction {
                name: "ceil".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(ceil)),
            },
        );

        m.insert(
            "sin".to_string(),
            ForeignFunction {
                name: "sin".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(sin)),
            },
        );
        m.insert(
            "cos".to_string(),
            ForeignFunction {
                name: "cos".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(cos)),
            },
        );
        m.insert(
            "tan".to_string(),
            ForeignFunction {
                name: "tan".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(tan)),
            },
        );
        m.insert(
            "asin".to_string(),
            ForeignFunction {
                name: "asin".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(asin)),
            },
        );
        m.insert(
            "acos".to_string(),
            ForeignFunction {
                name: "acos".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(acos)),
            },
        );
        m.insert(
            "atan".to_string(),
            ForeignFunction {
                name: "atan".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(atan)),
            },
        );
        m.insert(
            "atan2".to_string(),
            ForeignFunction {
                name: "atan2".into(),
                arity: 2..=2,
                callable: Callable::Function(Box::new(atan2)),
            },
        );

        m.insert(
            "sinh".to_string(),
            ForeignFunction {
                name: "sinh".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(sinh)),
            },
        );
        m.insert(
            "cosh".to_string(),
            ForeignFunction {
                name: "cosh".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(cosh)),
            },
        );
        m.insert(
            "tanh".to_string(),
            ForeignFunction {
                name: "tanh".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(tanh)),
            },
        );
        m.insert(
            "asinh".to_string(),
            ForeignFunction {
                name: "asinh".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(asinh)),
            },
        );
        m.insert(
            "acosh".to_string(),
            ForeignFunction {
                name: "acosh".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(acosh)),
            },
        );
        m.insert(
            "atanh".to_string(),
            ForeignFunction {
                name: "atanh".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(atanh)),
            },
        );

        m.insert(
            "mod".to_string(),
            ForeignFunction {
                name: "mod".into(),
                arity: 2..=2,
                callable: Callable::Function(Box::new(mod_)),
            },
        );
        m.insert(
            "exp".to_string(),
            ForeignFunction {
                name: "exp".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(exp)),
            },
        );
        m.insert(
            "ln".to_string(),
            ForeignFunction {
                name: "ln".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(ln)),
            },
        );
        m.insert(
            "log10".to_string(),
            ForeignFunction {
                name: "log10".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(log10)),
            },
        );
        m.insert(
            "log2".to_string(),
            ForeignFunction {
                name: "log2".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(log2)),
            },
        );

        m.insert(
            "mean".to_string(),
            ForeignFunction {
                name: "mean".into(),
                arity: 1..=usize::MAX,
                callable: Callable::Function(Box::new(mean)),
            },
        );
        m.insert(
            "maximum".to_string(),
            ForeignFunction {
                name: "maximum".into(),
                arity: 1..=usize::MAX,
                callable: Callable::Function(Box::new(maximum)),
            },
        );
        m.insert(
            "minimum".to_string(),
            ForeignFunction {
                name: "minimum".into(),
                arity: 1..=usize::MAX,
                callable: Callable::Function(Box::new(minimum)),
            },
        );

        for currency in ["USD", "JPY", "GBP", "CNY", "AUD", "CAD", "CHF"] {
            m.insert(
                format!("exchange_rate_{currency}"),
                ForeignFunction {
                    name: format!("exchange_rate_{currency}"),
                    arity: 0..=0,
                    callable: Callable::Function(exchange_rate(currency)),
                },
            );
        }

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
        let error = ControlFlow::Break(RuntimeError::AssertEq2Failed(
            args[0].clone(),
            args[1].clone(),
        ));
        if let Ok(args1_converted) = args[1].convert_to(args[0].unit()) {
            if args[0] == args1_converted {
                ControlFlow::Continue(())
            } else {
                error
            }
        } else {
            error
        }
    } else {
        let result = &args[0] - &args[1];

        match result {
            Ok(diff) => match diff.convert_to(args[2].unit()) {
                Err(e) => ControlFlow::Break(RuntimeError::QuantityError(e)),
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
            Err(e) => ControlFlow::Break(RuntimeError::QuantityError(e)),
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

    let input0 = args[0].unsafe_value().to_f64();
    let input1 = args[1]
        .convert_to(args[0].unit())
        .unwrap()
        .unsafe_value()
        .to_f64();
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

fn mod_(args: &[Quantity]) -> Quantity {
    assert!(args.len() == 2);

    let input0 = args[0].unsafe_value().to_f64();
    let input1 = args[1]
        .convert_to(args[0].unit())
        .unwrap()
        .unsafe_value()
        .to_f64();
    Quantity::new_f64(input0.rem_euclid(input1), args[0].unit().clone())
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

fn exchange_rate(rate: &'static str) -> Box<dyn Fn(&[Quantity]) -> Quantity + Send + Sync> {
    Box::new(|_args: &[Quantity]| -> Quantity {
        let exchange_rates = ExchangeRatesCache::new();
        Quantity::from_scalar(exchange_rates.get_rate(rate).unwrap_or(f64::NAN))
    })
}
