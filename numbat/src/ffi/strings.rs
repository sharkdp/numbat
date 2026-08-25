use super::Args;
use super::FfiContext;
use super::Result;
use super::macros::*;
use crate::interpreter::RuntimeErrorKind;
use crate::quantity::Quantity;
use crate::typechecker::type_scheme::TypeScheme;
use crate::value::Value;

pub fn str_length(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let len = string_arg!(args).len();
    return_scalar!(len as f64)
}

pub fn lowercase(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    return_string!(owned = string_arg!(args).to_lowercase())
}

pub fn uppercase(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    return_string!(owned = string_arg!(args).to_uppercase())
}

pub fn str_slice(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let start_q = quantity_arg!(args);
    let start = start_q.unsafe_value().try_as_real().ok_or_else(|| {
        Box::new(RuntimeErrorKind::ExpectedRealNumberInFunction("str_slice".into()))
    })? as usize;
    let end_q = quantity_arg!(args);
    let end = end_q.unsafe_value().try_as_real().ok_or_else(|| {
        Box::new(RuntimeErrorKind::ExpectedRealNumberInFunction("str_slice".into()))
    })? as usize;
    let input = string_arg!(args);

    let output = input.get(start..end).unwrap_or_default();

    return_string!(borrowed = output)
}

pub fn chr(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let q = quantity_arg!(args);
    let idx = q.unsafe_value().try_as_real().ok_or_else(|| {
        Box::new(RuntimeErrorKind::ExpectedRealNumberInFunction("chr".into()))
    })? as u32;

    let output = char::from_u32(idx).unwrap_or('�');

    return_string!(from = &output)
}

pub fn ord(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let input = string_arg!(args);

    if input.is_empty() {
        return Err(Box::new(RuntimeErrorKind::EmptyList));
    }

    let output = input.chars().next().unwrap() as u32;

    return_scalar!(output as f64)
}
