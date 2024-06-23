use super::macros::*;
use super::Result;
use crate::currency::ExchangeRatesCache;
use crate::quantity::Quantity;
use crate::value::Value;

pub fn exchange_rate(args: &[Value]) -> Result<Value> {
    let rate = string_arg!(args, 0);

    let exchange_rates = ExchangeRatesCache::new();

    return_scalar!(exchange_rates.get_rate(rate).unwrap_or(f64::NAN))
}
