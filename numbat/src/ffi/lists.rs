use super::macros::*;
use super::Result;
use crate::quantity::Quantity;
use crate::value::Value;
use crate::RuntimeError;

pub fn len(args: &[Value]) -> Result<Value> {
    let list = args[0].unsafe_as_list();

    return_scalar!(list.len() as f64)
}

pub fn head(args: &[Value]) -> Result<Value> {
    let list = args[0].unsafe_as_list();

    if let Some(first) = list.first() {
        Ok(first.clone())
    } else {
        Err(RuntimeError::EmptyList)
    }
}

pub fn tail(args: &[Value]) -> Result<Value> {
    let mut list = args[0].unsafe_as_list();

    if list.is_empty() {
        Err(RuntimeError::EmptyList)
    } else {
        list.remove(0);

        return_list!(list)
    }
}

pub fn cons(args: &[Value]) -> Result<Value> {
    let mut list = args[1].unsafe_as_list().clone();
    list.insert(0, args[0].clone());

    return_list!(list)
}
