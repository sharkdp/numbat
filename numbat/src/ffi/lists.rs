use super::macros::*;
use super::{Args, Result};
use crate::quantity::Quantity;
use crate::value::Value;
use crate::RuntimeError;

pub fn len(mut args: Args) -> Result<Value> {
    let list = list_arg!(args);

    return_scalar!(list.len() as f64)
}

pub fn head(mut args: Args) -> Result<Value> {
    let list = list_arg!(args);

    if let Some(first) = list.head() {
        Ok(first)
    } else {
        Err(Box::new(RuntimeError::EmptyList))
    }
}

pub fn tail(mut args: Args) -> Result<Value> {
    let mut list = list_arg!(args);

    list.tail()?;
    Ok(list.into())
}

pub fn cons(mut args: Args) -> Result<Value> {
    let element = arg!(args);
    let mut list = list_arg!(args);
    list.push_front(element);

    return_list!(list)
}

pub fn cons_end(mut args: Args) -> Result<Value> {
    let element = arg!(args);
    let mut list = list_arg!(args);
    list.push_back(element);

    return_list!(list)
}
