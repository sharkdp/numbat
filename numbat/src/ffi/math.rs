use super::Args;
use super::FfiContext;
use super::Result;
use super::macros::*;

use crate::interpreter::RuntimeErrorKind;
use crate::number::Number;
use crate::quantity::Quantity;
use crate::typechecker::type_scheme::TypeScheme;
use crate::value::Value;

fn require_real(num: &Number, fn_name: &str) -> std::result::Result<f64, Box<RuntimeErrorKind>> {
    num.try_as_real()
        .ok_or_else(|| Box::new(RuntimeErrorKind::ExpectedRealNumberInFunction(fn_name.into())))
}

pub fn mod_(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let x = quantity_arg!(args);
    let y = quantity_arg!(args);

    let x_value = require_real(x.unsafe_value(), "mod")?;
    let y_value = require_real(y.convert_to(x.unit()).unwrap().unsafe_value(), "mod")?;

    return_quantity!(x_value.rem_euclid(y_value), x.unit().clone())
}

/// A real-only scalar math function — errors on complex input
macro_rules! real_only_scalar_math_function {
    ($name:ident, $op:ident) => {
        pub fn $name(
            _ctx: &mut FfiContext,
            mut args: Args,
            _return_type: &TypeScheme,
        ) -> Result<Value, Box<RuntimeErrorKind>> {
            let value = require_real(&scalar_arg!(args), stringify!($name))?;
            return_scalar!(value.$op())
        }
    };
}

pub fn abs(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let arg = quantity_arg!(args);
    Ok(Value::Quantity(Quantity::new(
        arg.unsafe_value().abs(),
        arg.unit().clone(),
    )))
}

real_only_scalar_math_function!(round, round);
real_only_scalar_math_function!(floor, floor);
real_only_scalar_math_function!(ceil, ceil);
real_only_scalar_math_function!(trunc, trunc);
real_only_scalar_math_function!(fract, fract);

// Complex-aware trig/math functions

pub fn sin(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let z = scalar_arg!(args);
    if let Some(x) = z.try_as_real() {
        return_scalar!(x.sin())
    } else {
        // sin(a+bi) = sin(a)cosh(b) + i*cos(a)sinh(b)
        let (a, b) = (z.re, z.im);
        return_complex_scalar!(a.sin() * b.cosh(), a.cos() * b.sinh())
    }
}

pub fn cos(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let z = scalar_arg!(args);
    if let Some(x) = z.try_as_real() {
        return_scalar!(x.cos())
    } else {
        // cos(a+bi) = cos(a)cosh(b) - i*sin(a)sinh(b)
        let (a, b) = (z.re, z.im);
        return_complex_scalar!(a.cos() * b.cosh(), -(a.sin() * b.sinh()))
    }
}

pub fn tan(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let z = scalar_arg!(args);
    if let Some(x) = z.try_as_real() {
        return_scalar!(x.tan())
    } else {
        // tan(z) = sin(z)/cos(z)
        let sin_z = complex_sin(z);
        let cos_z = complex_cos(z);
        let result = Number::new(sin_z.0, sin_z.1) / Number::new(cos_z.0, cos_z.1);
        return_complex_scalar!(result.re, result.im)
    }
}

pub fn asin(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let z = scalar_arg!(args);
    if let Some(x) = z.try_as_real() {
        if (-1.0..=1.0).contains(&x) {
            return return_scalar!(x.asin());
        }
        // Fall through to complex
    }
    // asin(z) = -i * ln(iz + sqrt(1-z²))
    let i = Number::new(0.0, 1.0);
    let neg_i = Number::new(0.0, -1.0);
    let one = Number::from_f64(1.0);
    let z2 = z * z;
    let sqrt_arg = one - z2;
    let sqrt_val = complex_sqrt(sqrt_arg);
    let iz = i * z;
    let ln_arg = iz + sqrt_val;
    let ln_val = complex_ln(ln_arg);
    let result = neg_i * ln_val;
    return_complex_scalar!(result.re, result.im)
}

pub fn acos(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let z = scalar_arg!(args);
    if let Some(x) = z.try_as_real() {
        if (-1.0..=1.0).contains(&x) {
            return return_scalar!(x.acos());
        }
        // Fall through to complex
    }
    // acos(z) = -i * ln(z + sqrt(z²-1))
    let neg_i = Number::new(0.0, -1.0);
    let one = Number::from_f64(1.0);
    let z2 = z * z;
    let sqrt_arg = z2 - one;
    let sqrt_val = complex_sqrt(sqrt_arg);
    let ln_arg = z + sqrt_val;
    let ln_val = complex_ln(ln_arg);
    let result = neg_i * ln_val;
    return_complex_scalar!(result.re, result.im)
}

pub fn atan(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let z = scalar_arg!(args);
    if let Some(x) = z.try_as_real() {
        return_scalar!(x.atan())
    } else {
        // atan(z) = i/2 * ln((i+z)/(i-z))
        let i = Number::new(0.0, 1.0);
        let i_over_2 = Number::new(0.0, 0.5);
        let num = i + z;
        let den = i - z;
        let ratio = num / den;
        let ln_val = complex_ln(ratio);
        let result = i_over_2 * ln_val;
        return_complex_scalar!(result.re, result.im)
    }
}

pub fn atan2(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let y = quantity_arg!(args);
    let x = quantity_arg!(args);

    let y_value = require_real(y.unsafe_value(), "atan2")?;
    let x_value = require_real(x.convert_to(y.unit()).unwrap().unsafe_value(), "atan2")?;

    return_scalar!(y_value.atan2(x_value))
}

pub fn sinh(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let z = scalar_arg!(args);
    if let Some(x) = z.try_as_real() {
        return_scalar!(x.sinh())
    } else {
        // sinh(a+bi) = sinh(a)cos(b) + i*cosh(a)sin(b)
        let (a, b) = (z.re, z.im);
        return_complex_scalar!(a.sinh() * b.cos(), a.cosh() * b.sin())
    }
}

pub fn cosh(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let z = scalar_arg!(args);
    if let Some(x) = z.try_as_real() {
        return_scalar!(x.cosh())
    } else {
        // cosh(a+bi) = cosh(a)cos(b) + i*sinh(a)sin(b)
        let (a, b) = (z.re, z.im);
        return_complex_scalar!(a.cosh() * b.cos(), a.sinh() * b.sin())
    }
}

pub fn tanh(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let z = scalar_arg!(args);
    if let Some(x) = z.try_as_real() {
        return_scalar!(x.tanh())
    } else {
        // tanh(z) = sinh(z)/cosh(z)
        let (a, b) = (z.re, z.im);
        let sinh_z = Number::new(a.sinh() * b.cos(), a.cosh() * b.sin());
        let cosh_z = Number::new(a.cosh() * b.cos(), a.sinh() * b.sin());
        let result = sinh_z / cosh_z;
        return_complex_scalar!(result.re, result.im)
    }
}

pub fn asinh(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let z = scalar_arg!(args);
    if let Some(x) = z.try_as_real() {
        return_scalar!(x.asinh())
    } else {
        // asinh(z) = ln(z + sqrt(z²+1))
        let one = Number::from_f64(1.0);
        let z2 = z * z;
        let sqrt_val = complex_sqrt(z2 + one);
        let ln_val = complex_ln(z + sqrt_val);
        return_complex_scalar!(ln_val.re, ln_val.im)
    }
}

pub fn acosh(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let z = scalar_arg!(args);
    if let Some(x) = z.try_as_real() {
        if x >= 1.0 {
            return return_scalar!(x.acosh());
        }
        // Fall through to complex
    }
    // acosh(z) = ln(z + sqrt(z²-1))
    let one = Number::from_f64(1.0);
    let z2 = z * z;
    let sqrt_val = complex_sqrt(z2 - one);
    let ln_val = complex_ln(z + sqrt_val);
    return_complex_scalar!(ln_val.re, ln_val.im)
}

pub fn atanh(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let z = scalar_arg!(args);
    if let Some(x) = z.try_as_real() {
        if (-1.0..1.0).contains(&x) {
            return return_scalar!(x.atanh());
        }
        // Fall through to complex
    }
    // atanh(z) = 1/2 * ln((1+z)/(1-z))
    let one = Number::from_f64(1.0);
    let half = Number::from_f64(0.5);
    let num = one + z;
    let den = one - z;
    let ratio = num / den;
    let ln_val = complex_ln(ratio);
    let result = half * ln_val;
    return_complex_scalar!(result.re, result.im)
}

pub fn exp(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let z = scalar_arg!(args);
    if let Some(x) = z.try_as_real() {
        return_scalar!(x.exp())
    } else {
        // exp(a+bi) = exp(a)(cos(b) + i*sin(b))
        let (a, b) = (z.re, z.im);
        let exp_a = a.exp();
        return_complex_scalar!(exp_a * b.cos(), exp_a * b.sin())
    }
}

pub fn ln(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let z = scalar_arg!(args);
    if let Some(x) = z.try_as_real() {
        if x > 0.0 {
            return return_scalar!(x.ln());
        }
        // Fall through to complex for negative reals
    }
    let result = complex_ln(z);
    return_complex_scalar!(result.re, result.im)
}

pub fn log10(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let z = scalar_arg!(args);
    if let Some(x) = z.try_as_real() {
        if x > 0.0 {
            return return_scalar!(x.log10());
        }
    }
    // log10(z) = ln(z) / ln(10)
    let ln_z = complex_ln(z);
    let ln_10 = Number::from_f64(10.0_f64.ln());
    let result = ln_z / ln_10;
    return_complex_scalar!(result.re, result.im)
}

pub fn log2(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let z = scalar_arg!(args);
    if let Some(x) = z.try_as_real() {
        if x > 0.0 {
            return return_scalar!(x.log2());
        }
    }
    // log2(z) = ln(z) / ln(2)
    let ln_z = complex_ln(z);
    let ln_2 = Number::from_f64(2.0_f64.ln());
    let result = ln_z / ln_2;
    return_complex_scalar!(result.re, result.im)
}

pub fn gamma(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let input = require_real(&scalar_arg!(args), "gamma")?;

    return_scalar!(crate::gamma::gamma(input))
}

pub fn is_nan(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let arg = quantity_arg!(args);
    let n = arg.unsafe_value();

    return_boolean!(n.re.is_nan() || n.im.is_nan())
}

pub fn is_infinite(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let arg = quantity_arg!(args);
    let n = arg.unsafe_value();

    return_boolean!(n.re.is_infinite() || n.im.is_infinite())
}

pub fn random(
    _ctx: &mut FfiContext,
    _args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    return_scalar!(rand::random::<f64>())
}

// New functions: re, im, conj, arg_fn

pub fn re_fn(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let q = quantity_arg!(args);
    Ok(Value::Quantity(Quantity::new(
        Number::from_f64(q.unsafe_value().re()),
        q.unit().clone(),
    )))
}

pub fn im_fn(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let q = quantity_arg!(args);
    Ok(Value::Quantity(Quantity::new(
        Number::from_f64(q.unsafe_value().im()),
        q.unit().clone(),
    )))
}

pub fn conj(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let q = quantity_arg!(args);
    Ok(Value::Quantity(Quantity::new(
        q.unsafe_value().conj(),
        q.unit().clone(),
    )))
}

pub fn arg_fn(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let q = quantity_arg!(args);
    let n = q.as_scalar().map_err(|_| {
        Box::new(RuntimeErrorKind::UserError(
            "arg() requires a scalar (dimensionless) argument".into(),
        ))
    })?;
    return_scalar!(n.im.atan2(n.re))
}

pub fn is_real(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let q = quantity_arg!(args);
    return_boolean!(q.unsafe_value().is_real())
}

// Helper functions for complex math

fn complex_sin(z: Number) -> (f64, f64) {
    let (a, b) = (z.re, z.im);
    (a.sin() * b.cosh(), a.cos() * b.sinh())
}

fn complex_cos(z: Number) -> (f64, f64) {
    let (a, b) = (z.re, z.im);
    (a.cos() * b.cosh(), -(a.sin() * b.sinh()))
}

fn complex_ln(z: Number) -> Number {
    let abs_z = z.re.hypot(z.im);
    let arg_z = z.im.atan2(z.re);
    Number::new(abs_z.ln(), arg_z)
}

fn complex_sqrt(z: Number) -> Number {
    let half = Number::from_f64(0.5);
    z.pow(&half)
}
