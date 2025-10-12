
use std::ops::{Shl, Shr};



use super::macros::*;
use super::Args;
use super::Result;

use crate::quantity::Quantity;
use crate::value::Value;

pub fn mod_(mut args: Args) -> Result<Value> {
    let x = quantity_arg!(args);
    let y = quantity_arg!(args);

    let x_value = x.unsafe_value().to_f64();
    let y_value = y.convert_to(x.unit()).unwrap().unsafe_value().to_f64();

    return_quantity!(x_value.rem_euclid(y_value), x.unit().clone())
}

pub fn wrapped_shl(mut args: Args) -> Result<Value> {
    let lhs = scalar_arg!(args);
    let rhs = scalar_arg!(args);

    let result =  lhs.shl(rhs).to_f64();  //TODO: Implement wrapped left shift

    return_scalar!(result)
}

pub fn wrapped_shr(mut args: Args) -> Result<Value> {
    let lhs = scalar_arg!(args);
    let rhs = scalar_arg!(args);

    let result = lhs.shr(rhs).to_f64(); //TODO: Implement wrapped right shift

    return_scalar!(result)
}

// A simple math function with signature 'Fn[(Scalar) -> Scalar]'
macro_rules! simple_scalar_math_function {
    ($name:ident, $op:ident) => {
        pub fn $name(mut args: Args) -> Result<Value> {
            let value = scalar_arg!(args).to_f64();
            return_scalar!(value.$op())
        }
    };
}

pub fn abs(mut args: Args) -> Result<Value> {
    let arg = quantity_arg!(args);
    return_quantity!(arg.unsafe_value().to_f64().abs(), arg.unit().clone())
}

simple_scalar_math_function!(round, round);
simple_scalar_math_function!(floor, floor);
simple_scalar_math_function!(ceil, ceil);
simple_scalar_math_function!(trunc, trunc);
simple_scalar_math_function!(fract, fract);

simple_scalar_math_function!(sin, sin);
simple_scalar_math_function!(cos, cos);
simple_scalar_math_function!(tan, tan);
simple_scalar_math_function!(asin, asin);
simple_scalar_math_function!(acos, acos);
simple_scalar_math_function!(atan, atan);

pub fn atan2(mut args: Args) -> Result<Value> {
    let y = quantity_arg!(args);
    let x = quantity_arg!(args);

    let y_value = y.unsafe_value().to_f64();
    let x_value = x.convert_to(y.unit()).unwrap().unsafe_value().to_f64();

    return_scalar!(y_value.atan2(x_value))
}

simple_scalar_math_function!(sinh, sinh);
simple_scalar_math_function!(cosh, cosh);
simple_scalar_math_function!(tanh, tanh);
simple_scalar_math_function!(asinh, asinh);
simple_scalar_math_function!(acosh, acosh);
simple_scalar_math_function!(atanh, atanh);
simple_scalar_math_function!(exp, exp);
simple_scalar_math_function!(ln, ln);
simple_scalar_math_function!(log10, log10);
simple_scalar_math_function!(log2, log2);

pub fn gamma(mut args: Args) -> Result<Value> {
    let input = scalar_arg!(args).to_f64();

    return_scalar!(crate::gamma::gamma(input))
}

pub fn is_nan(mut args: Args) -> Result<Value> {
    let arg = quantity_arg!(args);

    return_boolean!(arg.unsafe_value().to_f64().is_nan())
}

pub fn is_infinite(mut args: Args) -> Result<Value> {
    let arg = quantity_arg!(args);

    return_boolean!(arg.unsafe_value().to_f64().is_infinite())
}

pub fn random(_args: Args) -> Result<Value> {
    return_scalar!(rand::random::<f64>())
}
