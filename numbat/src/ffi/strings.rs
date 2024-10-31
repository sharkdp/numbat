use super::macros::*;
use super::Args;
use super::Result;
use crate::quantity::Quantity;
use crate::value::Value;
use crate::RuntimeError;

pub fn str_length(mut args: Args) -> Result<Value> {
    let len = string_arg!(args).len();
    return_scalar!(len as f64)
}

pub fn lowercase(mut args: Args) -> Result<Value> {
    return_string!(owned = string_arg!(args).to_lowercase())
}

pub fn uppercase(mut args: Args) -> Result<Value> {
    return_string!(owned = string_arg!(args).to_uppercase())
}

pub fn str_slice(mut args: Args) -> Result<Value> {
    let start = quantity_arg!(args).unsafe_value().to_f64() as usize;
    let end = quantity_arg!(args).unsafe_value().to_f64() as usize;
    let input = string_arg!(args);

    let output = input.get(start..end).unwrap_or_default();

    return_string!(borrowed = output)
}

pub fn chr(mut args: Args) -> Result<Value> {
    let idx = quantity_arg!(args).unsafe_value().to_f64() as u32;

    let output = char::from_u32(idx).unwrap_or('�');

    return_string!(from = &output)
}

pub fn ord(mut args: Args) -> Result<Value> {
    let input = string_arg!(args);

    if input.is_empty() {
        return Err(Box::new(RuntimeError::EmptyList));
    }

    let output = input.chars().next().unwrap() as u32;

    return_scalar!(output as f64)
}
