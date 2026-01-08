use super::macros::*;
use super::{Args, Result};
use crate::interpreter::RuntimeErrorKind;
use crate::quantity::Quantity;
use crate::value::Value;
use crate::vm::ExecutionContext;

pub fn len(_ctx: &mut ExecutionContext, mut args: Args) -> Result<Value, Box<RuntimeErrorKind>> {
    let list = list_arg!(args);

    return_scalar!(list.len() as f64)
}

pub fn head(_ctx: &mut ExecutionContext, mut args: Args) -> Result<Value, Box<RuntimeErrorKind>> {
    let list = list_arg!(args);

    if let Some(first) = list.head() {
        Ok(first)
    } else {
        Err(Box::new(RuntimeErrorKind::EmptyList))
    }
}

pub fn tail(_ctx: &mut ExecutionContext, mut args: Args) -> Result<Value, Box<RuntimeErrorKind>> {
    let mut list = list_arg!(args);

    list.tail()?;
    Ok(list.into())
}

pub fn cons(_ctx: &mut ExecutionContext, mut args: Args) -> Result<Value, Box<RuntimeErrorKind>> {
    let element = arg!(args);
    let mut list = list_arg!(args);
    list.push_front(element);

    return_list!(list)
}

pub fn cons_end(
    _ctx: &mut ExecutionContext,
    mut args: Args,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let element = arg!(args);
    let mut list = list_arg!(args);
    list.push_back(element);

    return_list!(list)
}
