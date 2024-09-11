use std::sync::{Mutex, MutexGuard, OnceLock};

use numbat_exchange_rates::parse_exchange_rates;

#[derive(Debug)]
pub(crate) enum ExchangeRates {
    Real(numbat_exchange_rates::ExchangeRates),
    TestRates,
}

static EXCHANGE_RATES: OnceLock<Mutex<Option<ExchangeRates>>> = OnceLock::new();

pub struct ExchangeRatesCache {}

impl ExchangeRatesCache {
    pub fn new() -> Self {
        Self {}
    }

    pub fn get_rate(&self, currency: &str) -> Option<f64> {
        let rates = Self::fetch();
        rates
            .as_ref()
            .and_then(|er| match er {
                ExchangeRates::Real(er) => er.get(currency),
                ExchangeRates::TestRates => Some(&1.0),
            })
            .cloned()
    }

    pub fn set_from_xml(xml_content: &str) {
        EXCHANGE_RATES
            .set(Mutex::new(
                parse_exchange_rates(xml_content).map(ExchangeRates::Real),
            ))
            .unwrap();
    }

    #[cfg(feature = "fetch-exchangerates")]
    pub fn fetch() -> MutexGuard<'static, Option<ExchangeRates>> {
        EXCHANGE_RATES
            .get_or_init(|| {
                Mutex::new(numbat_exchange_rates::fetch_exchange_rates().map(ExchangeRates::Real))
            })
            .lock()
            .unwrap()
    }

    #[cfg(not(feature = "fetch-exchangerates"))]
    pub fn fetch() -> MutexGuard<'static, Option<ExchangeRates>> {
        EXCHANGE_RATES.get().unwrap().lock().unwrap()
    }

    pub fn use_test_rates() {
        EXCHANGE_RATES.get_or_init(|| Mutex::new(Some(ExchangeRates::TestRates)));
    }
}
