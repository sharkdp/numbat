use std::collections::HashMap;

use quick_xml::events::Event;
use quick_xml::reader::Reader;

pub type ExchangeRates = HashMap<String, f64>;

pub fn fetch_exchange_rates() -> Option<ExchangeRates> {
    let mut rates = ExchangeRates::default();

    let xml =
        reqwest::blocking::get("https://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml").ok()?
            .text().ok()?;

    let mut reader = Reader::from_str(&xml);
    loop {
        match reader.read_event().ok()? {
            Event::Eof => break,
            Event::Empty(e) => {
                if e.local_name().as_ref() != b"Cube" {
                    continue;
                }
                let currency = &e.try_get_attribute("currency").ok()??.unescape_value().ok()?;
                let rate = &e.try_get_attribute("rate").ok()??.unescape_value().ok()?;
                let rate = rate.parse().ok()?;

                rates.insert(currency.to_string(), rate);
            }
            _ => {},
        }
    }

    Some(rates)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fetch_exchange_rates_works() {
        fetch_exchange_rates();
    }
}
