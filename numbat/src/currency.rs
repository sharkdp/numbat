use std::sync::{Mutex, MutexGuard, OnceLock};

use numbat_exchange_rates::{fetch_exchange_rates, parse_exchange_rates, ExchangeRates};

static EXCHANGE_RATES: OnceLock<Mutex<Option<ExchangeRates>>> = OnceLock::new();

pub struct ExchangeRatesCache {}

impl ExchangeRatesCache {
    pub fn new() -> Self {
        Self {}
    }

    pub fn get_rate(&self, currency: &str) -> Option<f64> {
        let rates = Self::fetch();
        rates.as_ref().and_then(|r| r.get(currency)).cloned()
    }

    pub fn set_from_xml(xml_content: &str) {
        EXCHANGE_RATES
            .set(Mutex::new(parse_exchange_rates(xml_content)))
            .unwrap();
    }

    pub fn fetch() -> MutexGuard<'static, Option<ExchangeRates>> {
        EXCHANGE_RATES
            .get_or_init(|| Mutex::new(fetch_exchange_rates()))
            .lock()
            .unwrap()
    }
}
