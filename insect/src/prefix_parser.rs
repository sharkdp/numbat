use std::collections::HashSet;

use once_cell::sync::OnceCell;

use crate::{name_resolution::NameResolutionError, prefix::Prefix};

static PREFIXES: OnceCell<Vec<(&'static str, &'static str, Prefix)>> = OnceCell::new();

#[derive(Debug, Clone, PartialEq)]
pub enum PrefixParserResult {
    Identifier(String),
    UnitIdentifier(Prefix, String),
}

type Result<T> = std::result::Result<T, NameResolutionError>;

#[derive(Debug)]
pub struct PrefixParser {
    prefixable_units: HashSet<String>,
    non_prefixable_units: HashSet<String>,
    other_identifiers: HashSet<String>,
}

impl PrefixParser {
    pub fn new() -> Self {
        Self {
            prefixable_units: HashSet::new(),
            non_prefixable_units: HashSet::new(),
            other_identifiers: HashSet::new(),
        }
    }

    fn is_registered_name(&self, name: &str) -> bool {
        self.other_identifiers.contains(name)
            || self.prefixable_units.contains(name)
            || self.non_prefixable_units.contains(name)
    }

    pub fn add_prefixable_unit(&mut self, unit_name: &str) -> Result<()> {
        if self.is_registered_name(unit_name) {
            return Err(NameResolutionError::IdentifierClash(unit_name.into()));
        }

        self.prefixable_units.insert(unit_name.into());

        Ok(())
    }

    pub fn add_non_prefixable_unit(&mut self, unit_name: &str) -> Result<()> {
        if self.is_registered_name(unit_name) {
            return Err(NameResolutionError::IdentifierClash(unit_name.into()));
        }

        self.non_prefixable_units.insert(unit_name.into());

        Ok(())
    }

    pub fn add_other_identifier(&mut self, identifier: &str) -> Result<()> {
        match self.parse(identifier) {
            PrefixParserResult::Identifier(_) => {}
            PrefixParserResult::UnitIdentifier(_, _) => {
                return Err(NameResolutionError::IdentifierClash(identifier.into()));
            }
        }

        if self.other_identifiers.insert(identifier.into()) {
            Ok(())
        } else {
            Err(NameResolutionError::IdentifierClash(identifier.into()))
        }
    }

    pub fn parse(&self, input: &str) -> PrefixParserResult {
        if self.prefixable_units.iter().any(|u| u == input)
            || self.non_prefixable_units.iter().any(|u| u == input)
        {
            return PrefixParserResult::UnitIdentifier(Prefix::none(), input.into());
        }

        let prefixes = PREFIXES.get_or_init(|| {
            vec![
                ("atto", "a", Prefix::Decimal(-18)),
                ("femto", "f", Prefix::Decimal(-15)),
                ("pico", "p", Prefix::Decimal(-12)),
                ("nano", "n", Prefix::Decimal(-9)),
                ("micro", "µ", Prefix::Decimal(-6)), // TODO: support 'u' as well. and other unicode characters
                ("milli", "m", Prefix::Decimal(-3)),
                ("centi", "c", Prefix::Decimal(-2)),
                ("deci", "d", Prefix::Decimal(-1)),
                ("hecto", "h", Prefix::Decimal(2)),
                ("kilo", "k", Prefix::Decimal(3)),
                ("mega", "M", Prefix::Decimal(6)),
                ("giga", "G", Prefix::Decimal(9)),
                ("tera", "T", Prefix::Decimal(12)),
            ]
        });

        for (prefix_long, _prefix_short, prefix) in prefixes {
            if input.starts_with(prefix_long)
                && self
                    .prefixable_units
                    .iter()
                    .any(|u| u == &input[prefix_long.len()..])
            {
                return PrefixParserResult::UnitIdentifier(
                    prefix.clone(),
                    input[prefix_long.len()..].into(),
                );
            }
        }

        PrefixParserResult::Identifier(input.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        let mut prefix_parser = PrefixParser::new();
        prefix_parser.add_prefixable_unit("meter").unwrap();

        assert_eq!(
            prefix_parser.parse("meter"),
            PrefixParserResult::UnitIdentifier(Prefix::none(), "meter".into())
        );
        assert_eq!(
            prefix_parser.parse("kilometer"),
            PrefixParserResult::UnitIdentifier(Prefix::kilo(), "meter".into())
        );
        assert_eq!(
            prefix_parser.parse("millimeter"),
            PrefixParserResult::UnitIdentifier(Prefix::milli(), "meter".into())
        );

        assert_eq!(
            prefix_parser.parse("kilomete"),
            PrefixParserResult::Identifier("kilomete".into())
        );
        assert_eq!(
            prefix_parser.parse("kilometerr"),
            PrefixParserResult::Identifier("kilometerr".into())
        );

        assert_eq!(
            prefix_parser.parse("foometer"),
            PrefixParserResult::Identifier("foometer".into())
        );
    }
}
