use super::Args;
use super::FfiContext;
use super::Result;
use super::macros::*;
use crate::interpreter::RuntimeErrorKind;
use crate::quantity::Quantity;
use crate::typechecker::type_scheme::TypeScheme;
use crate::value::Value;

pub fn exchange_rate(
    _ctx: &mut FfiContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let rate = string_arg!(args);

    return_scalar!(crate::currency::get_rate(&rate).unwrap_or(f64::NAN))
}
