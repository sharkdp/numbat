use super::macros::*;
use super::Result;

use crate::quantity::Quantity;
use crate::value::Value;

pub fn mod_(args: &[Value]) -> Result<Value> {
    let x = quantity_arg!(args, 0);
    let y = quantity_arg!(args, 1);

    let x_value = x.unsafe_value().to_f64();
    let y_value = y.convert_to(x.unit()).unwrap().unsafe_value().to_f64();

    return_quantity!(x_value.rem_euclid(y_value), x.unit().clone())
}

// A simple math function with signature 'Dim D. Fn[(D) -> D]', which only operates on the value of the quantity
macro_rules! simple_polymorphic_math_function {
    ($name:ident, $op:ident) => {
        pub fn $name(args: &[Value]) -> Result<Value> {
            let arg = args[0].unsafe_as_quantity();

            let value = arg.unsafe_value().to_f64();
            Ok(Value::Quantity(Quantity::new_f64(
                value.$op(),
                arg.unit().clone(),
            )))
        }
    };
}

// Similar, but with signature 'Fn[(Scalar) -> Scalar]'
macro_rules! simple_scalar_math_function {
    ($name:ident, $op:ident) => {
        pub fn $name(args: &[Value]) -> Result<Value> {
            let value = scalar_arg!(args, 0).to_f64();
            return_scalar!(value.$op())
        }
    };
}

simple_polymorphic_math_function!(abs, abs);
simple_polymorphic_math_function!(round, round);
simple_polymorphic_math_function!(floor, floor);
simple_polymorphic_math_function!(ceil, ceil);

simple_scalar_math_function!(sin, sin);
simple_scalar_math_function!(cos, cos);
simple_scalar_math_function!(tan, tan);
simple_scalar_math_function!(asin, asin);
simple_scalar_math_function!(acos, acos);
simple_scalar_math_function!(atan, atan);

pub fn atan2(args: &[Value]) -> Result<Value> {
    let y = quantity_arg!(args, 0);
    let x = quantity_arg!(args, 1);

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

pub fn gamma(args: &[Value]) -> Result<Value> {
    let input = scalar_arg!(args, 0).to_f64();

    return_scalar!(crate::gamma::gamma(input))
}

pub fn is_nan(args: &[Value]) -> Result<Value> {
    let arg = quantity_arg!(args, 0);

    return_boolean!(arg.unsafe_value().to_f64().is_nan())
}

pub fn is_infinite(args: &[Value]) -> Result<Value> {
    let arg = quantity_arg!(args, 0);

    return_boolean!(arg.unsafe_value().to_f64().is_infinite())
}
