use std::collections::HashMap;

use std::sync::OnceLock;

use chrono::Offset;

use crate::currency::ExchangeRatesCache;
use crate::interpreter::RuntimeError;
use crate::pretty_print::PrettyPrint;
use crate::value::{FunctionReference, Value};
use crate::vm::ExecutionContext;
use crate::{ast::ProcedureKind, quantity::Quantity};

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

static FFI_PROCEDURES: OnceLock<HashMap<ProcedureKind, ForeignFunction>> = OnceLock::new();
static FFI_FUNCTIONS: OnceLock<HashMap<String, ForeignFunction>> = OnceLock::new();

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

pub(crate) fn functions() -> &'static HashMap<String, ForeignFunction> {
    FFI_FUNCTIONS.get_or_init(|| {
        let mut m = HashMap::new();

        m.insert(
            "error".to_string(),
            ForeignFunction {
                name: "error".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(error)),
            },
        );
        m.insert(
            "unit_of".to_string(),
            ForeignFunction {
                name: "unit_of".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(unit_of)),
            },
        );
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
            "gamma".to_string(),
            ForeignFunction {
                name: "gamma".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(gamma)),
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

        m.insert(
            "exchange_rate".to_string(),
            ForeignFunction {
                name: "exchange_rate".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(exchange_rate)),
            },
        );

        m.insert(
            "str_length".to_string(),
            ForeignFunction {
                name: "str_length".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(str_length)),
            },
        );
        m.insert(
            "lowercase".to_string(),
            ForeignFunction {
                name: "lowercase".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(lowercase)),
            },
        );
        m.insert(
            "uppercase".to_string(),
            ForeignFunction {
                name: "uppercase".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(uppercase)),
            },
        );
        m.insert(
            "str_slice".to_string(),
            ForeignFunction {
                name: "str_slice".into(),
                arity: 3..=3,
                callable: Callable::Function(Box::new(str_slice)),
            },
        );
        m.insert(
            "chr".to_string(),
            ForeignFunction {
                name: "chr".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(chr)),
            },
        );
        m.insert(
            "now".to_string(),
            ForeignFunction {
                name: "now".into(),
                arity: 0..=0,
                callable: Callable::Function(Box::new(now)),
            },
        );
        m.insert(
            "datetime".to_string(),
            ForeignFunction {
                name: "datetime".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(datetime)),
            },
        );

        m.insert(
            "format_datetime".to_string(),
            ForeignFunction {
                name: "format_datetime".into(),
                arity: 2..=2,
                callable: Callable::Function(Box::new(format_datetime)),
            },
        );

        m.insert(
            "get_local_timezone".to_string(),
            ForeignFunction {
                name: "get_local_timezone".into(),
                arity: 0..=0,
                callable: Callable::Function(Box::new(get_local_timezone)),
            },
        );

        m.insert(
            "tz".to_string(),
            ForeignFunction {
                name: "tz".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(tz)),
            },
        );

        m.insert(
            "unixtime".to_string(),
            ForeignFunction {
                name: "unixtime".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(unixtime)),
            },
        );

        m.insert(
            "from_unixtime".to_string(),
            ForeignFunction {
                name: "from_unixtime".into(),
                arity: 1..=1,
                callable: Callable::Function(Box::new(from_unixtime)),
            },
        );

        m
    })
}

fn print(ctx: &mut ExecutionContext, args: &[Value]) -> ControlFlow {
    assert!(args.len() <= 1);

    if args.is_empty() {
        (ctx.print_fn)(&crate::markup::text(""))
    } else {
        match &args[0] {
            Value::String(string) => (ctx.print_fn)(&crate::markup::text(string)), // print string without quotes
            arg => (ctx.print_fn)(&arg.pretty_print()),
        }
    }

    ControlFlow::Continue(())
}

fn assert(_: &mut ExecutionContext, args: &[Value]) -> ControlFlow {
    assert!(args.len() == 1);

    if args[0].unsafe_as_bool() {
        ControlFlow::Continue(())
    } else {
        ControlFlow::Break(RuntimeError::AssertFailed)
    }
}

fn assert_eq(_: &mut ExecutionContext, args: &[Value]) -> ControlFlow {
    assert!(args.len() == 2 || args.len() == 3);

    let lhs = args[0].unsafe_as_quantity();
    let rhs = args[1].unsafe_as_quantity();

    if args.len() == 2 {
        let error = ControlFlow::Break(RuntimeError::AssertEq2Failed(lhs.clone(), rhs.clone()));
        if let Ok(args1_converted) = rhs.convert_to(lhs.unit()) {
            if lhs == &args1_converted {
                ControlFlow::Continue(())
            } else {
                error
            }
        } else {
            error
        }
    } else {
        let result = lhs - rhs;
        let eps = args[2].unsafe_as_quantity();

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

fn error(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    Err(RuntimeError::UserError(args[0].unsafe_as_string().into()))
}

fn unit_of(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    Ok(Value::Quantity(Quantity::new_f64(
        1.0,
        args[0].unsafe_as_quantity().unit().clone(),
    )))
}

fn abs(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    let arg = args[0].unsafe_as_quantity();

    let value = arg.unsafe_value().to_f64();
    Ok(Value::Quantity(Quantity::new_f64(
        value.abs(),
        arg.unit().clone(),
    )))
}

fn round(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    let arg = args[0].unsafe_as_quantity();

    let value = arg.unsafe_value().to_f64();
    Ok(Value::Quantity(Quantity::new_f64(
        value.round(),
        arg.unit().clone(),
    )))
}

fn floor(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    let arg = args[0].unsafe_as_quantity();

    let value = arg.unsafe_value().to_f64();
    Ok(Value::Quantity(Quantity::new_f64(
        value.floor(),
        arg.unit().clone(),
    )))
}

fn ceil(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    let arg = args[0].unsafe_as_quantity();

    let value = arg.unsafe_value().to_f64();
    Ok(Value::Quantity(Quantity::new_f64(
        value.ceil(),
        arg.unit().clone(),
    )))
}

fn sin(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    let arg = args[0].unsafe_as_quantity();

    let input = arg.as_scalar().unwrap().to_f64();
    Ok(Value::Quantity(Quantity::from_scalar(input.sin())))
}

fn cos(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    let arg = args[0].unsafe_as_quantity();

    let input = arg.as_scalar().unwrap().to_f64();
    Ok(Value::Quantity(Quantity::from_scalar(input.cos())))
}

fn tan(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    let arg = args[0].unsafe_as_quantity();

    let input = arg.as_scalar().unwrap().to_f64();
    Ok(Value::Quantity(Quantity::from_scalar(input.tan())))
}

fn asin(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    let arg = args[0].unsafe_as_quantity();

    let input = arg.as_scalar().unwrap().to_f64();
    Ok(Value::Quantity(Quantity::from_scalar(input.asin())))
}

fn acos(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    let arg = args[0].unsafe_as_quantity();

    let input = arg.as_scalar().unwrap().to_f64();
    Ok(Value::Quantity(Quantity::from_scalar(input.acos())))
}

fn atan(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    let arg = args[0].unsafe_as_quantity();

    let input = arg.as_scalar().unwrap().to_f64();
    Ok(Value::Quantity(Quantity::from_scalar(input.atan())))
}

fn atan2(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 2);

    let y = args[0].unsafe_as_quantity();
    let x = args[1].unsafe_as_quantity();

    let input0 = y.unsafe_value().to_f64();
    let input1 = x.convert_to(y.unit()).unwrap().unsafe_value().to_f64();
    Ok(Value::Quantity(Quantity::from_scalar(input0.atan2(input1))))
}

fn sinh(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    let arg = args[0].unsafe_as_quantity();

    let input = arg.as_scalar().unwrap().to_f64();
    Ok(Value::Quantity(Quantity::from_scalar(input.sinh())))
}

fn cosh(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    let arg = args[0].unsafe_as_quantity();

    let input = arg.as_scalar().unwrap().to_f64();
    Ok(Value::Quantity(Quantity::from_scalar(input.cosh())))
}

fn tanh(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    let arg = args[0].unsafe_as_quantity();

    let input = arg.as_scalar().unwrap().to_f64();
    Ok(Value::Quantity(Quantity::from_scalar(input.tanh())))
}

fn asinh(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    let arg = args[0].unsafe_as_quantity();

    let input = arg.as_scalar().unwrap().to_f64();
    Ok(Value::Quantity(Quantity::from_scalar(input.asinh())))
}

fn acosh(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    let arg = args[0].unsafe_as_quantity();

    let input = arg.as_scalar().unwrap().to_f64();
    Ok(Value::Quantity(Quantity::from_scalar(input.acosh())))
}

fn atanh(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    let arg = args[0].unsafe_as_quantity();

    let input = arg.as_scalar().unwrap().to_f64();
    Ok(Value::Quantity(Quantity::from_scalar(input.atanh())))
}

fn mod_(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 2);

    let x = args[0].unsafe_as_quantity();
    let y = args[1].unsafe_as_quantity();

    let input0 = x.unsafe_value().to_f64();
    let input1 = y.convert_to(x.unit()).unwrap().unsafe_value().to_f64();
    Ok(Value::Quantity(Quantity::new_f64(
        input0.rem_euclid(input1),
        x.unit().clone(),
    )))
}

fn exp(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    let arg = args[0].unsafe_as_quantity();

    let input = arg.as_scalar().unwrap().to_f64();
    Ok(Value::Quantity(Quantity::from_scalar(input.exp())))
}

fn ln(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    let arg = args[0].unsafe_as_quantity();

    let input = arg.as_scalar().unwrap().to_f64();
    Ok(Value::Quantity(Quantity::from_scalar(input.ln())))
}

fn log10(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    let arg = args[0].unsafe_as_quantity();

    let input = arg.as_scalar().unwrap().to_f64();
    Ok(Value::Quantity(Quantity::from_scalar(input.log10())))
}

fn log2(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    let arg = args[0].unsafe_as_quantity();

    let input = arg.as_scalar().unwrap().to_f64();
    Ok(Value::Quantity(Quantity::from_scalar(input.log2())))
}

fn gamma(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    let arg = args[0].unsafe_as_quantity();

    let input = arg.as_scalar().unwrap().to_f64();
    Ok(Value::Quantity(Quantity::from_scalar(crate::gamma::gamma(
        input,
    ))))
}

fn mean(args: &[Value]) -> Result<Value> {
    assert!(!args.is_empty());

    let output_unit = args[0].unsafe_as_quantity().unit();
    Ok(Value::Quantity(Quantity::new_f64(
        args.iter()
            .map(|q| {
                q.unsafe_as_quantity()
                    .convert_to(output_unit)
                    .unwrap()
                    .unsafe_value()
                    .to_f64()
            })
            .sum::<f64>()
            / (args.len() as f64),
        output_unit.clone(),
    )))
}

fn maximum(args: &[Value]) -> Result<Value> {
    assert!(!args.is_empty());

    let output_unit = args[0].unsafe_as_quantity().unit();
    Ok(Value::Quantity(Quantity::new(
        args.iter()
            .map(|q| {
                *q.unsafe_as_quantity()
                    .convert_to(output_unit)
                    .unwrap()
                    .unsafe_value()
            })
            .max_by(|l, r| l.partial_cmp(r).unwrap())
            .unwrap(),
        output_unit.clone(),
    )))
}

fn minimum(args: &[Value]) -> Result<Value> {
    assert!(!args.is_empty());

    let output_unit = args[0].unsafe_as_quantity().unit();
    Ok(Value::Quantity(Quantity::new(
        args.iter()
            .map(|q| {
                *q.unsafe_as_quantity()
                    .convert_to(output_unit)
                    .unwrap()
                    .unsafe_value()
            })
            .min_by(|l, r| l.partial_cmp(r).unwrap())
            .unwrap(),
        output_unit.clone(),
    )))
}

fn exchange_rate(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    let rate = args[0].unsafe_as_string();

    let exchange_rates = ExchangeRatesCache::new();

    Ok(Value::Quantity(Quantity::from_scalar(
        exchange_rates.get_rate(rate).unwrap_or(f64::NAN),
    )))
}

fn str_length(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    let len = args[0].unsafe_as_string().len();
    Ok(Value::Quantity(Quantity::from_scalar(len as f64)))
}

fn lowercase(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    Ok(Value::String(args[0].unsafe_as_string().to_lowercase()))
}

fn uppercase(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    Ok(Value::String(args[0].unsafe_as_string().to_uppercase()))
}

fn str_slice(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 3);

    let input = args[0].unsafe_as_string();
    let start = args[1].unsafe_as_quantity().unsafe_value().to_f64() as usize;
    let end = args[2].unsafe_as_quantity().unsafe_value().to_f64() as usize;

    let output = input.get(start..end).unwrap_or_default();

    Ok(Value::String(output.into()))
}

fn chr(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    let idx = args[0].unsafe_as_quantity().unsafe_value().to_f64() as u32;

    let output = char::from_u32(idx).unwrap_or('�');

    Ok(Value::String(output.to_string()))
}

fn now(args: &[Value]) -> Result<Value> {
    assert!(args.is_empty());
    let now = chrono::Utc::now();

    let offset = now.with_timezone(&chrono::Local).offset().fix();

    Ok(Value::DateTime(now, offset))
}

fn datetime(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    let input = args[0].unsafe_as_string();

    let output = crate::datetime::parse_datetime(input)
        .map_err(RuntimeError::DateParsingError)?
        .ok_or(RuntimeError::DateParsingErrorUnknown)?;

    let offset = crate::datetime::local_offset_for_datetime(&output);

    Ok(Value::DateTime(output.into(), offset))
}

fn format_datetime(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 2);

    let format = args[0].unsafe_as_string();
    let dt = args[1].unsafe_as_datetime();

    let output = dt.format(format).to_string();

    Ok(Value::String(output))
}

fn get_local_timezone(args: &[Value]) -> Result<Value> {
    assert!(args.is_empty());

    let local_tz = iana_time_zone::get_timezone().unwrap_or("UTC".to_string());

    Ok(Value::String(local_tz))
}

fn tz(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    let tz = args[0].unsafe_as_string();

    Ok(Value::FunctionReference(FunctionReference::TzConversion(
        tz.into(),
    )))
}

fn unixtime(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    let input = args[0].unsafe_as_datetime();

    let output = input.timestamp();

    Ok(Value::Quantity(Quantity::from_scalar(output as f64)))
}

fn from_unixtime(args: &[Value]) -> Result<Value> {
    assert!(args.len() == 1);

    let timestamp = args[0].unsafe_as_quantity().unsafe_value().to_f64() as i64;

    let dt = chrono::DateTime::from_timestamp(timestamp, 0).unwrap();
    let offset = dt.offset().fix();

    Ok(Value::DateTime(dt, offset))
}
