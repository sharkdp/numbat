use once_cell::sync::OnceCell;

use crate::prefix::Prefix;

static PREFIXES: OnceCell<Vec<(&'static str, &'static str, Prefix)>> = OnceCell::new();

#[derive(Debug)]
pub struct PrefixParser {
    prefixable_units: Vec<(String, String)>,
}

impl PrefixParser {
    pub fn new() -> Self {
        Self {
            prefixable_units: vec![],
        }
    }

    pub fn add_prefixable_unit(&mut self, unit_long: &str, unit_short: &str) {
        self.prefixable_units
            .push((unit_long.into(), unit_short.into())) // TODO: check for duplicates here?
    }

    pub fn parse(&self, input: &str) -> (Option<Prefix>, String) {
        let prefixes = PREFIXES.get_or_init(|| {
            vec![
                ("nano", "n", Prefix::Decimal(-9)),
                ("micro", "µ", Prefix::Decimal(-6)), // TODO
                ("milli", "m", Prefix::Decimal(-3)),
                ("centi", "c", Prefix::Decimal(-2)),
                ("deci", "d", Prefix::Decimal(-1)),
                ("hecto", "h", Prefix::Decimal(2)),
                ("kilo", "k", Prefix::Decimal(3)),
                ("mega", "M", Prefix::Decimal(6)),
                ("giga", "G", Prefix::Decimal(9)),
            ]
        });

        for (prefix_long, prefix_short, prefix) in prefixes {
            if input.starts_with(prefix_long)
                && self
                    .prefixable_units
                    .iter()
                    .any(|u| u.0 == &input[prefix_long.len()..])
            {
                return (Some(prefix.clone()), input[prefix_long.len()..].into());
            }

            if input.starts_with(prefix_short)
                && self
                    .prefixable_units
                    .iter()
                    .any(|u| u.1 == &input[prefix_short.len()..])
            {
                return (Some(prefix.clone()), input[prefix_short.len()..].into());
            }
        }

        (None, input.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        let mut prefix_parser = PrefixParser::new();
        prefix_parser.add_prefixable_unit("meter", "m");

        assert_eq!(prefix_parser.parse("meter"), (None, "meter".to_string()));
        assert_eq!(prefix_parser.parse("m"), (None, "m".to_string()));
        assert_eq!(
            prefix_parser.parse("kilometer"),
            (Some(Prefix::Decimal(3)), "meter".to_string())
        );
        assert_eq!(
            prefix_parser.parse("km"),
            (Some(Prefix::Decimal(3)), "m".to_string())
        );
        assert_eq!(
            prefix_parser.parse("millimeter"),
            (Some(Prefix::Decimal(-3)), "meter".to_string())
        );
        assert_eq!(
            prefix_parser.parse("mm"),
            (Some(Prefix::Decimal(-3)), "m".to_string())
        );

        assert_eq!(
            prefix_parser.parse("kilomete"),
            (None, "kilomete".to_string())
        );
        assert_eq!(
            prefix_parser.parse("kilometerr"),
            (None, "kilometerr".to_string())
        );

        assert_eq!(
            prefix_parser.parse("foometer"),
            (None, "foometer".to_string())
        );

        assert_eq!(prefix_parser.parse("kilom"), (None, "kilom".to_string()));
        assert_eq!(prefix_parser.parse("kmeter"), (None, "kmeter".to_string()));
    }
}
