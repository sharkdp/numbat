use std::sync::{Mutex, MutexGuard, OnceLock};

use numbat_exchange_rates::{fetch_exchange_rates, ExchangeRates, parse_exchange_rates};

static EXCHANGE_RATES: OnceLock<Mutex<ExchangeRates>> = OnceLock::new();

pub struct ExchangeRatesCache {}

impl ExchangeRatesCache {
    pub fn new() -> Self {
        Self {}
    }

    pub fn get_rate(&self, currency: &str) -> Option<f64> {
        let rates = self.fetch();
        rates.get(currency).cloned()
    }

    pub fn set_from_xml(xml_content: &str) {
        EXCHANGE_RATES.set(Mutex::new(parse_exchange_rates(xml_content).unwrap_or_default())).unwrap();
    }

    pub fn fetch(&self) -> MutexGuard<ExchangeRates> {
        EXCHANGE_RATES
            .get_or_init(|| Mutex::new(fetch_exchange_rates().unwrap_or_default()))
            .lock()
            .unwrap()
    }
}
