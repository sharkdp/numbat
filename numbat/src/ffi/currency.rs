use super::macros::*;
use super::Result;
use crate::currency::ExchangeRatesCache;
use crate::quantity::Quantity;
use crate::value::Value;

pub fn exchange_rate(args: &[Value]) -> Result<Value> {
    let rate = args[0].unsafe_as_string();

    let exchange_rates = ExchangeRatesCache::new();

    return_scalar!(exchange_rates.get_rate(rate).unwrap_or(f64::NAN))
}
