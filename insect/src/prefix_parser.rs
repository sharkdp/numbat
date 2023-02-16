use std::collections::{HashMap, HashSet};

use once_cell::sync::OnceCell;

use crate::{name_resolution::NameResolutionError, prefix::Prefix};

static PREFIXES: OnceCell<Vec<(&'static str, &'static str, Prefix)>> = OnceCell::new();

#[derive(Debug, Clone, PartialEq)]
pub enum PrefixParserResult {
    Identifier(String),
    UnitIdentifier(Prefix, String),
}

type Result<T> = std::result::Result<T, NameResolutionError>;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnitKind {
    Short,
    Long,
}

#[derive(Debug, Clone)]
struct UnitInfo {
    kind: UnitKind,
    metric_prefixes: bool,
    binary_prefixes: bool,
}

#[derive(Debug)]
pub struct PrefixParser {
    units: HashMap<String, UnitInfo>,
    other_identifiers: HashSet<String>,
}

impl PrefixParser {
    pub fn new() -> Self {
        Self {
            units: HashMap::new(),
            other_identifiers: HashSet::new(),
        }
    }

    fn prefixes() -> &'static [(&'static str, &'static str, Prefix)] {
        PREFIXES.get_or_init(|| {
            vec![
                // Metric prefixes:
                ("quecto", "q", Prefix::Metric(-30)),
                ("ronto", "r", Prefix::Metric(-27)),
                ("yocto", "y", Prefix::Metric(-24)),
                ("zepto", "z", Prefix::Metric(-21)),
                ("atto", "a", Prefix::Metric(-18)),
                ("femto", "f", Prefix::Metric(-15)),
                ("pico", "p", Prefix::Metric(-12)),
                ("nano", "n", Prefix::Metric(-9)),
                ("micro", "µ", Prefix::Metric(-6)), // TODO: support 'u' as well. and other unicode characters
                ("milli", "m", Prefix::Metric(-3)),
                ("centi", "c", Prefix::Metric(-2)),
                ("deci", "d", Prefix::Metric(-1)),
                ("deca", "da", Prefix::Metric(1)),
                ("hecto", "h", Prefix::Metric(2)),
                ("kilo", "k", Prefix::Metric(3)),
                ("mega", "M", Prefix::Metric(6)),
                ("giga", "G", Prefix::Metric(9)),
                ("tera", "T", Prefix::Metric(12)),
                ("peta", "P", Prefix::Metric(15)),
                ("exa", "E", Prefix::Metric(18)),
                ("zetta", "Z", Prefix::Metric(21)),
                ("yotta", "Y", Prefix::Metric(24)),
                ("ronna", "R", Prefix::Metric(27)),
                ("quetta", "Q", Prefix::Metric(30)),
                // Binary prefixes:
                ("kibi", "Ki", Prefix::Binary(10)),
                ("mebi", "Mi", Prefix::Binary(20)),
                ("gibi", "Gi", Prefix::Binary(30)),
                ("tebi", "Ti", Prefix::Binary(40)),
                ("pebi", "Pi", Prefix::Binary(50)),
                ("exbi", "Ei", Prefix::Binary(60)),
                ("zebi", "Zi", Prefix::Binary(70)),
                ("yobi", "Yi", Prefix::Binary(80)),
                // The following two prefixes are not yet approved by IEC as of 2023-02-16
                // ("robi", "Ri", Prefix::Binary(90)),
                // ("quebi", "Qi", Prefix::Binary(100)),
            ]
        })
    }

    fn ensure_name_is_available(&self, name: &str) -> Result<()> {
        if self.other_identifiers.contains(name) {
            return Err(NameResolutionError::IdentifierClash(name.into()));
        }

        match self.parse(name) {
            PrefixParserResult::Identifier(_) => Ok(()),
            PrefixParserResult::UnitIdentifier(_, _) => {
                Err(NameResolutionError::IdentifierClash(name.into()))
            }
        }
    }

    pub fn add_unit(
        &mut self,
        unit_name: &str,
        kind: UnitKind,
        metric: bool,
        binary: bool,
    ) -> Result<()> {
        self.ensure_name_is_available(unit_name)?;

        for (prefix_long, prefix_short, prefix) in Self::prefixes() {
            if !(prefix.is_metric() && metric || prefix.is_binary() && binary) {
                continue;
            }

            match kind {
                UnitKind::Long => {
                    self.ensure_name_is_available(&format!("{}{}", prefix_long, unit_name))?
                }
                UnitKind::Short => {
                    self.ensure_name_is_available(&format!("{}{}", prefix_short, unit_name))?
                }
            }
        }

        self.units.insert(
            unit_name.into(),
            UnitInfo {
                kind,
                metric_prefixes: metric,
                binary_prefixes: binary,
            },
        );

        Ok(())
    }

    pub fn add_other_identifier(&mut self, identifier: &str) -> Result<()> {
        self.ensure_name_is_available(identifier)?;

        if self.other_identifiers.insert(identifier.into()) {
            Ok(())
        } else {
            Err(NameResolutionError::IdentifierClash(identifier.into()))
        }
    }

    pub fn parse(&self, input: &str) -> PrefixParserResult {
        if self.units.iter().any(|(name, _)| name == input) {
            return PrefixParserResult::UnitIdentifier(Prefix::none(), input.into());
        }

        for (prefix_long, prefix_short, prefix) in Self::prefixes() {
            let is_metric = prefix.is_metric();
            let is_binary = prefix.is_binary();

            if input.starts_with(prefix_long)
                && self
                    .units
                    .iter()
                    .filter(|(_, info)| {
                        info.kind == UnitKind::Long
                            && (is_metric && info.metric_prefixes
                                || is_binary && info.binary_prefixes)
                    })
                    .any(|(name, _)| name == &input[prefix_long.len()..])
            {
                return PrefixParserResult::UnitIdentifier(
                    prefix.clone(),
                    input[prefix_long.len()..].into(),
                );
            }

            if input.starts_with(prefix_short)
                && self
                    .units
                    .iter()
                    .filter(|(_, info)| {
                        info.kind == UnitKind::Short
                            && (is_metric && info.metric_prefixes
                                || is_binary && info.binary_prefixes)
                    })
                    .any(|(name, _)| name == &input[prefix_short.len()..])
            {
                return PrefixParserResult::UnitIdentifier(
                    prefix.clone(),
                    input[prefix_short.len()..].into(),
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
        prefix_parser
            .add_unit("meter", UnitKind::Long, true, false)
            .unwrap();
        prefix_parser
            .add_unit("m", UnitKind::Short, true, false)
            .unwrap();

        prefix_parser
            .add_unit("byte", UnitKind::Long, true, true)
            .unwrap();
        prefix_parser
            .add_unit("B", UnitKind::Short, true, true)
            .unwrap();

        prefix_parser
            .add_unit("me", UnitKind::Short, false, false)
            .unwrap();

        assert_eq!(
            prefix_parser.parse("meter"),
            PrefixParserResult::UnitIdentifier(Prefix::none(), "meter".into())
        );
        assert_eq!(
            prefix_parser.parse("m"),
            PrefixParserResult::UnitIdentifier(Prefix::none(), "m".into())
        );
        assert_eq!(
            prefix_parser.parse("byte"),
            PrefixParserResult::UnitIdentifier(Prefix::none(), "byte".into())
        );
        assert_eq!(
            prefix_parser.parse("B"),
            PrefixParserResult::UnitIdentifier(Prefix::none(), "B".into())
        );
        assert_eq!(
            prefix_parser.parse("me"),
            PrefixParserResult::UnitIdentifier(Prefix::none(), "me".into())
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
            prefix_parser.parse("kilobyte"),
            PrefixParserResult::UnitIdentifier(Prefix::kilo(), "byte".into())
        );
        assert_eq!(
            prefix_parser.parse("kibibyte"),
            PrefixParserResult::UnitIdentifier(Prefix::kibi(), "byte".into())
        );
        assert_eq!(
            prefix_parser.parse("mebibyte"),
            PrefixParserResult::UnitIdentifier(Prefix::mebi(), "byte".into())
        );

        assert_eq!(
            prefix_parser.parse("km"),
            PrefixParserResult::UnitIdentifier(Prefix::kilo(), "m".into())
        );
        assert_eq!(
            prefix_parser.parse("mm"),
            PrefixParserResult::UnitIdentifier(Prefix::milli(), "m".into())
        );
        assert_eq!(
            prefix_parser.parse("kB"),
            PrefixParserResult::UnitIdentifier(Prefix::kilo(), "B".into())
        );
        assert_eq!(
            prefix_parser.parse("MB"),
            PrefixParserResult::UnitIdentifier(Prefix::mega(), "B".into())
        );
        assert_eq!(
            prefix_parser.parse("KiB"),
            PrefixParserResult::UnitIdentifier(Prefix::kibi(), "B".into())
        );
        assert_eq!(
            prefix_parser.parse("MiB"),
            PrefixParserResult::UnitIdentifier(Prefix::mebi(), "B".into())
        );

        assert_eq!(
            prefix_parser.parse("kilom"),
            PrefixParserResult::Identifier("kilom".into())
        );
        assert_eq!(
            prefix_parser.parse("kilome"),
            PrefixParserResult::Identifier("kilome".into())
        );
        assert_eq!(
            prefix_parser.parse("kme"),
            PrefixParserResult::Identifier("kme".into())
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

        assert_eq!(
            prefix_parser.parse("kibimeter"),
            PrefixParserResult::Identifier("kibimeter".into())
        );
        assert_eq!(
            prefix_parser.parse("Kim"),
            PrefixParserResult::Identifier("Kim".into())
        );
    }
}
