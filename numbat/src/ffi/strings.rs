use super::macros::*;
use super::Result;
use crate::quantity::Quantity;
use crate::value::Value;

pub fn str_length(args: &[Value]) -> Result<Value> {
    let len = string_arg!(args, 0).len();
    return_scalar!(len as f64)
}

pub fn lowercase(args: &[Value]) -> Result<Value> {
    return_string!(string_arg!(args, 0).to_lowercase())
}

pub fn uppercase(args: &[Value]) -> Result<Value> {
    return_string!(string_arg!(args, 0).to_uppercase())
}

pub fn str_slice(args: &[Value]) -> Result<Value> {
    let input = string_arg!(args, 0);
    let start = quantity_arg!(args, 1).unsafe_value().to_f64() as usize;
    let end = quantity_arg!(args, 2).unsafe_value().to_f64() as usize;

    let output = input.get(start..end).unwrap_or_default();

    return_string!(output)
}

pub fn chr(args: &[Value]) -> Result<Value> {
    let idx = quantity_arg!(args, 0).unsafe_value().to_f64() as u32;

    let output = char::from_u32(idx).unwrap_or('�');

    return_string!(output)
}
