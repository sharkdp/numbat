use super::Args;
use super::Result;
use super::macros::*;
use crate::interpreter::RuntimeErrorKind;
use crate::quantity::Quantity;
use crate::typechecker::type_scheme::TypeScheme;
use crate::value::Value;
use crate::vm::ExecutionContext;

pub fn str_length(
    _ctx: &mut ExecutionContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let len = string_arg!(args).len();
    return_scalar!(len as f64)
}

pub fn lowercase(
    _ctx: &mut ExecutionContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    return_string!(owned = string_arg!(args).to_lowercase())
}

pub fn uppercase(
    _ctx: &mut ExecutionContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    return_string!(owned = string_arg!(args).to_uppercase())
}

pub fn str_slice(
    _ctx: &mut ExecutionContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let start = quantity_arg!(args).unsafe_value().to_f64() as usize;
    let end = quantity_arg!(args).unsafe_value().to_f64() as usize;
    let input = string_arg!(args);

    let output = input.get(start..end).unwrap_or_default();

    return_string!(borrowed = output)
}

pub fn chr(
    _ctx: &mut ExecutionContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let idx = quantity_arg!(args).unsafe_value().to_f64() as u32;

    let output = char::from_u32(idx).unwrap_or('�');

    return_string!(from = &output)
}

pub fn ord(
    _ctx: &mut ExecutionContext,
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
