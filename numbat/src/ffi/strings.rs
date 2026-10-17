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
    let len = string_arg!(args).chars().count();
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

// Like `str::get(start..end)`, but `start` and `end` count characters instead of bytes.
fn char_slice(input: &str, start: usize, end: usize) -> Option<&str> {
    if start > end {
        return None;
    }
    let mut boundaries = input
        .char_indices()
        .map(|(i, _)| i)
        .chain(std::iter::once(input.len()));
    let start_byte = boundaries.by_ref().nth(start)?;
    let end_byte = if end == start {
        start_byte
    } else {
        boundaries.nth(end - start - 1)?
    };
    Some(&input[start_byte..end_byte])
}

pub fn str_slice(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let start = quantity_arg!(args).unsafe_value().to_f64() as usize;
    let end = quantity_arg!(args).unsafe_value().to_f64() as usize;
    let input = string_arg!(args);

    let output = char_slice(&input, start, end).unwrap_or_default();

    return_string!(borrowed = output)
}

pub fn chr(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let idx = quantity_arg!(args).unsafe_value().to_f64() as u32;

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
