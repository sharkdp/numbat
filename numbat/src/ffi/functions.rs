use std::collections::HashMap;
use std::sync::OnceLock;

use super::{Args, macros::*};
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
        insert_function!(value_of, 1..=1);
        insert_function!(has_unit, 2..=2);
        insert_function!(is_dimensionless, 1..=1);
        insert_function!(unit_name, 1..=1);
        insert_function!(quantity_cast, 2..=2);

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
        insert_function!(unixtime, 1..=1);
        insert_function!(from_unixtime, 1..=1);

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

fn error(mut args: Args) -> Result<Value, Box<RuntimeErrorKind>> {
    Err(Box::new(RuntimeErrorKind::UserError(
        arg!(args).unsafe_as_string().to_string(),
    )))
}

fn value_of(mut args: Args) -> Result<Value, Box<RuntimeErrorKind>> {
    let quantity = quantity_arg!(args);

    return_scalar!(quantity.unsafe_value().to_f64())
}

fn has_unit(mut args: Args) -> Result<Value, Box<RuntimeErrorKind>> {
    let quantity = quantity_arg!(args);
    let unit_query = quantity_arg!(args);

    return_boolean!(quantity.is_zero() || quantity.unit() == unit_query.unit())
}

fn is_dimensionless(mut args: Args) -> Result<Value, Box<RuntimeErrorKind>> {
    let quantity = quantity_arg!(args);

    return_boolean!(quantity.as_scalar().is_ok())
}

fn unit_name(mut args: Args) -> Result<Value, Box<RuntimeErrorKind>> {
    let quantity = quantity_arg!(args);

    return_string!(from = &quantity.unit().to_string())
}

fn quantity_cast(mut args: Args) -> Result<Value, Box<RuntimeErrorKind>> {
    let value_from = quantity_arg!(args);
    let _ = quantity_arg!(args);

    Ok(Value::Quantity(value_from))
}
