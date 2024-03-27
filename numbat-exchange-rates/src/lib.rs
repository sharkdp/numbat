use std::collections::HashMap;

use quick_xml::events::Event;
use quick_xml::reader::Reader;

pub type ExchangeRates = HashMap<String, f64>;

pub fn parse_exchange_rates(xml_content: &str) -> Option<ExchangeRates> {
    let mut rates = ExchangeRates::default();

    let mut reader = Reader::from_str(xml_content);
    loop {
        match reader.read_event().ok()? {
            Event::Eof => break,
            Event::Empty(e) => {
                if e.local_name().as_ref() != b"Cube" {
                    continue;
                }
                let currency = &e
                    .try_get_attribute("currency")
                    .ok()??
                    .unescape_value()
                    .ok()?;
                let rate = &e.try_get_attribute("rate").ok()??.unescape_value().ok()?;
                let rate = rate.parse().ok()?;

                rates.insert(currency.to_string(), rate);
            }
            _ => {}
        }
    }

    Some(rates)
}

#[cfg(feature = "fetch-exchangerates")]
const ECB_XML_URL: &str = "https://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml";

#[cfg(feature = "fetch-exchangerates")]
fn fetch_ecb_xml() -> Option<String> {
    attohttpc::get(ECB_XML_URL).send().ok()?.text().ok()
}

#[cfg(feature = "fetch-exchangerates")]
pub fn fetch_exchange_rates() -> Option<ExchangeRates> {
    use std::{
        fs,
        path::PathBuf,
        time::{Duration, SystemTime},
    };

    let cache_dir = dirs::cache_dir()
        .unwrap_or_else(|| PathBuf::from("cache"))
        .join("numbat/");
    if !cache_dir.exists() {
        let _ = fs::create_dir(&cache_dir);
    }
    let cache_path = cache_dir.join("eurofxref-daily.xml");

    if let Ok(mtime) = fs::metadata(&cache_path).and_then(|m| m.modified()) {
        if let Ok(dur) = SystemTime::now().duration_since(mtime) {
            // try to load cached rates if fetched less than 1 day ago
            if dur < Duration::from_secs(60 * 60 * 24) {
                let xml_content = fs::read_to_string(&cache_path);
                if let Ok(Some(rates)) = xml_content.as_deref().map(parse_exchange_rates) {
                    return Some(rates);
                }
            }
        }
    }

    let xml_content = fetch_ecb_xml()?;
    parse_exchange_rates(&xml_content).map(|rates| {
        let _ = fs::write(cache_path, &xml_content);
        rates
    })
}

#[cfg(test)]
#[cfg(feature = "fetch-exchangerates")]
mod tests {
    use super::*;

    #[test]
    fn fetch_exchange_rates_works() {
        fetch_exchange_rates();
    }
}
