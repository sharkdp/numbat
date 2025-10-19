use super::Args;
use super::Result;
use super::macros::*;
use crate::currency::ExchangeRatesCache;
use crate::interpreter::RuntimeErrorKind;
use crate::quantity::Quantity;
use crate::value::Value;

pub fn exchange_rate(mut args: Args) -> Result<Value, Box<RuntimeErrorKind>> {
    let rate = string_arg!(args);

    let exchange_rates = ExchangeRatesCache::new();

    return_scalar!(exchange_rates.get_rate(&rate).unwrap_or(f64::NAN))
}
