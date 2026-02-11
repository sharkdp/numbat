use std::collections::HashMap;
use std::sync::OnceLock;

use super::{Args, FfiContext, macros::*};
use crate::parse_quantity::parse_quantity_literal;
use crate::typechecker::type_scheme::TypeScheme;
use crate::{interpreter::RuntimeErrorKind, quantity::Quantity, value::Value};

use super::{Callable, ForeignFunction, Result};

static FFI_FUNCTIONS: OnceLock<HashMap<&'static str, ForeignFunction>> = OnceLock::new();

pub(crate) fn functions() -> &'static HashMap<&'static str, ForeignFunction> {
    use super::currency::*;
    use super::datetime::*;
    use super::lists::*;
    use super::lookup::*;
    use super::math::*;
    use super::plot::*;
    use super::strings::*;

    FFI_FUNCTIONS.get_or_init(|| {
        let mut m = HashMap::new();

        macro_rules! insert_function {
            ($fn_name:expr, $callable:expr, $arity:expr) => {
                m.insert(
                    $fn_name,
                    ForeignFunction {
                        arity: $arity,
                        callable: Callable::Function($callable),
                    },
                );
            };
            ($callable:expr, $arity:expr) => {
                insert_function!(stringify!($callable), $callable, $arity);
            };
        }

        // Core
        insert_function!(error, 1..=1);
        insert_function!(inspect, 1..=1);
        insert_function!(value_of, 1..=1);
        insert_function!(base_unit_of, 1..=1);
        insert_function!(has_unit, 2..=2);
        insert_function!(is_dimensionless, 1..=1);
        insert_function!(unit_name, 1..=1);
        insert_function!(quantity_cast, 2..=2);
        insert_function!("parse", parse, 1..=1);
        insert_function!("args", args_, 0..=0);

        // Math
        insert_function!("mod", mod_, 2..=2);

        insert_function!(abs, 1..=1);
        insert_function!(round, 1..=1);
        insert_function!(floor, 1..=1);
        insert_function!(ceil, 1..=1);
        insert_function!(trunc, 1..=1);
        insert_function!(fract, 1..=1);

        insert_function!(sin, 1..=1);
        insert_function!(cos, 1..=1);
        insert_function!(tan, 1..=1);
        insert_function!(asin, 1..=1);
        insert_function!(acos, 1..=1);
        insert_function!(atan, 1..=1);
        insert_function!(atan2, 2..=2);
        insert_function!(sinh, 1..=1);
        insert_function!(cosh, 1..=1);
        insert_function!(tanh, 1..=1);
        insert_function!(asinh, 1..=1);
        insert_function!(acosh, 1..=1);
        insert_function!(atanh, 1..=1);
        insert_function!(exp, 1..=1);
        insert_function!(ln, 1..=1);
        insert_function!(log10, 1..=1);
        insert_function!(log2, 1..=1);
        insert_function!(gamma, 1..=1);

        insert_function!(is_nan, 1..=1);
        insert_function!(is_infinite, 1..=1);

        insert_function!(random, 0..=0);

        insert_function!("re", re_fn, 1..=1);
        insert_function!("im", im_fn, 1..=1);
        insert_function!("conj", conj, 1..=1);
        insert_function!("arg", arg_fn, 1..=1);
        insert_function!(is_real, 1..=1);

        // Lists
        insert_function!(len, 1..=1);
        insert_function!(head, 1..=1);
        insert_function!(tail, 1..=1);
        insert_function!(cons, 2..=2);
        insert_function!(cons_end, 2..=2);

        // Strings
        insert_function!(str_length, 1..=1);
        insert_function!(lowercase, 1..=1);
        insert_function!(uppercase, 1..=1);
        insert_function!(str_slice, 3..=3);
        insert_function!(chr, 1..=1);
        insert_function!(ord, 1..=1);

        // Date and time
        insert_function!(now, 0..=0);
        insert_function!(datetime, 1..=1);
        insert_function!(format_datetime, 2..=2);
        insert_function!(get_local_timezone, 0..=0);
        insert_function!(tz, 1..=1);
        insert_function!("_unixtime_µs", unixtime_us, 1..=1);
        insert_function!("_from_unixtime_µs", from_unixtime_us, 1..=1);

        insert_function!(_add_days, 2..=2);
        insert_function!(_add_months, 2..=2);
        insert_function!(_add_years, 2..=2);

        // Currency
        insert_function!(exchange_rate, 1..=1);

        // Database lookup
        insert_function!(_get_chemical_element_data_raw, 1..=1);

        // Plotting
        insert_function!(show, 1..=1);

        m
    })
}

fn error(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    Err(Box::new(RuntimeErrorKind::UserError(
        arg!(args).unsafe_as_string().to_string(),
    )))
}

fn inspect(
    ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    use crate::markup as m;

    let arg = args.pop_front().unwrap();

    // Only show the type if it's concrete (not a quantified/generic type)
    let type_markup = if arg.type_.is_polymorphic() {
        m::empty()
    } else {
        m::dimmed("    [")
            + arg
                .type_
                .to_readable_type(ctx.ctx.typechecker.registry(), false)
            + m::dimmed("]")
    };

    let output = m::text("inspect: ") + arg.value.pretty_print() + type_markup;
    (ctx.ctx.print_fn)(&output);
    Ok(arg.value)
}

fn value_of(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let quantity = quantity_arg!(args);

    let n = quantity.unsafe_value().try_as_real().ok_or_else(|| {
        Box::new(RuntimeErrorKind::ExpectedRealNumberInFunction(
            "value_of".into(),
        ))
    })?;
    return_scalar!(n)
}

fn base_unit_of(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let quantity = quantity_arg!(args);

    if quantity.is_zero() {
        return Err(Box::new(RuntimeErrorKind::UserError(
            "Invalid argument: cannot call `base_unit_of` on a value that evaluates to 0".into(),
        )));
    }

    return_quantity!(1.0, quantity.unit().without_prefixes())
}

fn has_unit(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let quantity = quantity_arg!(args);
    let unit_query = quantity_arg!(args);

    return_boolean!(quantity.is_zero() || quantity.unit() == unit_query.unit())
}

fn is_dimensionless(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let quantity = quantity_arg!(args);

    return_boolean!(quantity.as_scalar().is_ok())
}

fn unit_name(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let quantity = quantity_arg!(args);

    return_string!(from = &quantity.unit().to_string())
}

fn quantity_cast(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let value_from = quantity_arg!(args);
    let _ = quantity_arg!(args);

    Ok(Value::Quantity(value_from))
}

fn parse(
    ctx: &mut FfiContext,
    mut args: Args,
    return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let input = string_arg!(args);

    if return_type.is_polymorphic() {
        return Err(Box::new(RuntimeErrorKind::UserError(
            "parse() requires a type annotation for the return type, e.g. `let x: Length = parse(\"1.5 km\")`".into(),
        )));
    }

    let unit_lookup = |name: &str| ctx.lookup_unit(name);

    match parse_quantity_literal(
        &input,
        ctx.ctx.prefix_transformer,
        ctx.ctx.typechecker,
        unit_lookup,
    ) {
        Ok((quantity, input_type)) => {
            // Runtime type check: ensure that the parsed quantity's type matches the expected return type
            if quantity.is_zero() || input_type.to_concrete_type() == return_type.to_concrete_type()
            {
                Ok(Value::Quantity(quantity))
            } else {
                Err(Box::new(RuntimeErrorKind::UserError(format!(
                    "Type mismatch: expected `{}`, got `{}`",
                    return_type.to_concrete_type(),
                    input_type.to_concrete_type()
                ))))
            }
        }
        Err(e) => Err(Box::new(RuntimeErrorKind::UserError(format!(
            "Could not parse '{}': {}",
            input, e
        )))),
    }
}

fn args_(
    _ctx: &mut FfiContext,
    _args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let args: std::collections::VecDeque<Value> = std::env::args()
        .skip(1)
        .map(|s| Value::String(s.into()))
        .collect();
    Ok(args.into())
}
