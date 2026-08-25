use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{LazyLock, Mutex, OnceLock};

use numbat_exchange_rates::{ExchangeRates, parse_exchange_rates};

pub trait CurrencyManager: Send + Sync {
    fn get_rate(&self, currency: &str) -> Option<f64>;
    fn is_known(&self, currency: &str) -> bool;
    fn is_loaded(&self) -> bool;
    fn load(&self) -> Result<Option<String>, CouldNotLoadCurrencyManager>;
}

pub struct CouldNotLoadCurrencyManager;

#[derive(Debug, Clone, Default)]
pub struct NullCurrencyManager {}

impl CurrencyManager for NullCurrencyManager {
    fn get_rate(&self, _: &str) -> Option<f64> {
        None
    }

    fn is_known(&self, _: &str) -> bool {
        false
    }

    fn is_loaded(&self) -> bool {
        true
    }

    fn load(&self) -> Result<Option<String>, CouldNotLoadCurrencyManager> {
        Ok(None)
    }
}

#[derive(Debug, Default)]
pub struct OnDemandCurrencyManager {
    loaded: AtomicBool,
    rates: OnceLock<Option<ExchangeRates>>,
}

impl OnDemandCurrencyManager {
    const CURRENCY_IDENTIFIERS: &[&str] = &include!(concat!(env!("OUT_DIR"), "/currencies.rs"));

    pub fn preloaded() -> Self {
        let manager = Self::default();
        manager.load_rates();
        manager
    }

    pub fn from_xml(xml_content: &str) -> Self {
        let manager = Self::default();
        manager
            .rates
            .set(parse_exchange_rates(xml_content))
            .unwrap();
        manager
    }

    fn load_rates(&self) {
        self.rates.get_or_init(fetch_exchange_rates);
    }
}

impl CurrencyManager for OnDemandCurrencyManager {
    fn get_rate(&self, currency: &str) -> Option<f64> {
        self.rates.get()?.as_ref()?.get(currency).copied()
    }

    fn is_known(&self, currency: &str) -> bool {
        Self::CURRENCY_IDENTIFIERS.contains(&currency)
    }

    fn is_loaded(&self) -> bool {
        self.loaded.load(Ordering::Acquire)
    }

    fn load(&self) -> Result<Option<String>, CouldNotLoadCurrencyManager> {
        self.load_rates();

        if self.rates.get().is_none() {
            return Err(CouldNotLoadCurrencyManager);
        }

        self.loaded.store(true, Ordering::Release);

        Ok(Some("use units::currencies".into()))
    }
}

static MANAGER: LazyLock<Mutex<Box<dyn CurrencyManager>>> =
    LazyLock::new(|| Mutex::new(Box::new(NullCurrencyManager {})));

pub(crate) fn set_manager(currency_manager: impl CurrencyManager + 'static) {
    *MANAGER.lock().unwrap() = Box::new(currency_manager);
}

pub(crate) fn get_rate(currency: &str) -> Option<f64> {
    MANAGER.try_lock().ok()?.get_rate(currency)
}

pub(crate) fn is_known(currency: &str) -> bool {
    MANAGER.try_lock().is_ok_and(|m| m.is_known(currency))
}

pub(crate) fn is_loaded() -> bool {
    MANAGER.try_lock().map_or(true, |m| m.is_loaded())
}

pub(crate) fn load_manager() -> Result<Option<String>, CouldNotLoadCurrencyManager> {
    MANAGER
        .try_lock()
        .map_err(|_| CouldNotLoadCurrencyManager)
        .and_then(|m| m.load())
}

#[cfg(feature = "fetch-exchangerates")]
use numbat_exchange_rates::fetch_exchange_rates;

#[cfg(not(feature = "fetch-exchangerates"))]
fn fetch_exchange_rates() -> Option<ExchangeRates> {
    None
}
