use std::collections::HashMap;

use std::sync::OnceLock;

use crate::span::Span;
use crate::{name_resolution::NameResolutionError, prefix::Prefix};

static PREFIXES: OnceLock<Vec<(&'static str, &'static str, Prefix)>> = OnceLock::new();

#[derive(Debug, Clone, PartialEq)]
pub enum PrefixParserResult {
    Identifier(String),
    /// Span, prefix, unit name in source (e.g. 'm'), full unit name (e.g. 'meter')
    UnitIdentifier(Span, Prefix, String, String),
}

type Result<T> = std::result::Result<T, NameResolutionError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AcceptsPrefix {
    pub short: bool,
    pub long: bool,
}

impl AcceptsPrefix {
    pub fn only_long() -> Self {
        Self {
            long: true,
            short: false,
        }
    }

    pub fn only_short() -> Self {
        Self {
            long: false,
            short: true,
        }
    }

    pub fn both() -> Self {
        Self {
            long: true,
            short: true,
        }
    }

    pub fn none() -> Self {
        Self {
            long: false,
            short: false,
        }
    }
}

#[derive(Debug, Clone)]
struct UnitInfo {
    definition_span: Span,
    accepts_prefix: AcceptsPrefix,
    metric_prefixes: bool,
    binary_prefixes: bool,
    full_name: String,
}

#[derive(Debug, Clone)]
pub struct PrefixParser {
    units: HashMap<String, UnitInfo>,
    // This is the exact same information as in the "units" hashmap, only faster to iterate over.
    // TODO: maybe use an external crate for this (e.g. indexmap?)
    units_vec: Vec<(String, UnitInfo)>,

    other_identifiers: HashMap<String, Span>,

    reserved_identifiers: &'static [&'static str],
}

impl PrefixParser {
    pub fn new() -> Self {
        Self {
            units: HashMap::new(),
            units_vec: Vec::new(),
            other_identifiers: HashMap::new(),
            reserved_identifiers: &["_", "ans"],
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
                ("micro", "µ", Prefix::Metric(-6)),
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

    fn identifier_clash_error(
        &self,
        name: &str,
        conflict_span: Span,
        original_span: Span,
    ) -> NameResolutionError {
        NameResolutionError::IdentifierClash {
            conflicting_identifier: name.to_string(),
            conflict_span,
            original_span,
        }
    }

    fn ensure_name_is_available(
        &self,
        name: &str,
        conflict_span: Span,
        clash_with_other_identifiers: bool,
    ) -> Result<()> {
        if self.reserved_identifiers.contains(&name) {
            return Err(NameResolutionError::ReservedIdentifier(conflict_span));
        }

        if clash_with_other_identifiers {
            if let Some(original_span) = self.other_identifiers.get(name) {
                return Err(self.identifier_clash_error(name, conflict_span, *original_span));
            }
        }

        match self.parse(name) {
            PrefixParserResult::Identifier(_) => Ok(()),
            PrefixParserResult::UnitIdentifier(original_span, _, _, _) => {
                Err(self.identifier_clash_error(name, conflict_span, original_span))
            }
        }
    }

    pub fn add_unit(
        &mut self,
        unit_name: &str,
        accepts_prefix: AcceptsPrefix,
        metric: bool,
        binary: bool,
        full_name: &str,
        definition_span: Span,
    ) -> Result<()> {
        self.ensure_name_is_available(unit_name, definition_span, true)?;

        for (prefix_long, prefix_short, prefix) in Self::prefixes() {
            if !(prefix.is_metric() && metric || prefix.is_binary() && binary) {
                continue;
            }

            if accepts_prefix.long {
                self.ensure_name_is_available(
                    &format!("{}{}", prefix_long, unit_name),
                    definition_span,
                    true,
                )?;
            }
            if accepts_prefix.short {
                self.ensure_name_is_available(
                    &format!("{}{}", prefix_short, unit_name),
                    definition_span,
                    true,
                )?;
            }
        }

        let unit_info = UnitInfo {
            definition_span,
            accepts_prefix,
            metric_prefixes: metric,
            binary_prefixes: binary,
            full_name: full_name.into(),
        };
        self.units.insert(unit_name.into(), unit_info.clone());
        self.units_vec.push((unit_name.into(), unit_info));

        Ok(())
    }

    pub fn add_other_identifier(&mut self, identifier: &str, definition_span: Span) -> Result<()> {
        self.ensure_name_is_available(identifier, definition_span, false)?;

        self.other_identifiers
            .insert(identifier.into(), definition_span);
        Ok(())
    }

    pub fn parse(&self, input: &str) -> PrefixParserResult {
        if let Some(info) = self.units.get(input) {
            return PrefixParserResult::UnitIdentifier(
                info.definition_span,
                Prefix::none(),
                input.into(),
                info.full_name.clone(),
            );
        }

        for (unit_name, info) in &self.units_vec {
            if !input.ends_with(unit_name.as_str()) {
                continue;
            }

            for (prefix_long, prefix_short, prefix) in Self::prefixes() {
                let is_metric = prefix.is_metric();
                let is_binary = prefix.is_binary();

                if info.accepts_prefix.long
                    && (is_metric && info.metric_prefixes || is_binary && info.binary_prefixes)
                    && input.starts_with(prefix_long)
                    && &input[prefix_long.len()..] == unit_name
                {
                    return PrefixParserResult::UnitIdentifier(
                        info.definition_span,
                        *prefix,
                        unit_name.to_string(),
                        info.full_name.clone(),
                    );
                }

                if info.accepts_prefix.short
                    && (is_metric && info.metric_prefixes || is_binary && info.binary_prefixes)
                    && input.starts_with(prefix_short)
                    && &input[prefix_short.len()..] == unit_name
                {
                    return PrefixParserResult::UnitIdentifier(
                        info.definition_span,
                        *prefix,
                        unit_name.to_string(),
                        info.full_name.clone(),
                    );
                }
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
            .add_unit(
                "meter",
                AcceptsPrefix::only_long(),
                true,
                false,
                "meter",
                Span::dummy(),
            )
            .unwrap();
        prefix_parser
            .add_unit(
                "m",
                AcceptsPrefix::only_short(),
                true,
                false,
                "meter",
                Span::dummy(),
            )
            .unwrap();

        prefix_parser
            .add_unit(
                "byte",
                AcceptsPrefix::only_long(),
                true,
                true,
                "byte",
                Span::dummy(),
            )
            .unwrap();
        prefix_parser
            .add_unit(
                "B",
                AcceptsPrefix::only_short(),
                true,
                true,
                "byte",
                Span::dummy(),
            )
            .unwrap();

        prefix_parser
            .add_unit(
                "me",
                AcceptsPrefix::only_short(),
                false,
                false,
                "me",
                Span::dummy(),
            )
            .unwrap();

        assert_eq!(
            prefix_parser.parse("meter"),
            PrefixParserResult::UnitIdentifier(
                Span::dummy(),
                Prefix::none(),
                "meter".into(),
                "meter".into()
            )
        );
        assert_eq!(
            prefix_parser.parse("m"),
            PrefixParserResult::UnitIdentifier(
                Span::dummy(),
                Prefix::none(),
                "m".into(),
                "meter".into()
            )
        );
        assert_eq!(
            prefix_parser.parse("byte"),
            PrefixParserResult::UnitIdentifier(
                Span::dummy(),
                Prefix::none(),
                "byte".into(),
                "byte".into()
            )
        );
        assert_eq!(
            prefix_parser.parse("B"),
            PrefixParserResult::UnitIdentifier(
                Span::dummy(),
                Prefix::none(),
                "B".into(),
                "byte".into()
            )
        );
        assert_eq!(
            prefix_parser.parse("me"),
            PrefixParserResult::UnitIdentifier(
                Span::dummy(),
                Prefix::none(),
                "me".into(),
                "me".into()
            )
        );

        assert_eq!(
            prefix_parser.parse("kilometer"),
            PrefixParserResult::UnitIdentifier(
                Span::dummy(),
                Prefix::kilo(),
                "meter".into(),
                "meter".into()
            )
        );
        assert_eq!(
            prefix_parser.parse("millimeter"),
            PrefixParserResult::UnitIdentifier(
                Span::dummy(),
                Prefix::milli(),
                "meter".into(),
                "meter".into()
            )
        );
        assert_eq!(
            prefix_parser.parse("kilobyte"),
            PrefixParserResult::UnitIdentifier(
                Span::dummy(),
                Prefix::kilo(),
                "byte".into(),
                "byte".into()
            )
        );
        assert_eq!(
            prefix_parser.parse("kibibyte"),
            PrefixParserResult::UnitIdentifier(
                Span::dummy(),
                Prefix::kibi(),
                "byte".into(),
                "byte".into()
            )
        );
        assert_eq!(
            prefix_parser.parse("mebibyte"),
            PrefixParserResult::UnitIdentifier(
                Span::dummy(),
                Prefix::mebi(),
                "byte".into(),
                "byte".into()
            )
        );

        assert_eq!(
            prefix_parser.parse("km"),
            PrefixParserResult::UnitIdentifier(
                Span::dummy(),
                Prefix::kilo(),
                "m".into(),
                "meter".into()
            )
        );
        assert_eq!(
            prefix_parser.parse("mm"),
            PrefixParserResult::UnitIdentifier(
                Span::dummy(),
                Prefix::milli(),
                "m".into(),
                "meter".into()
            )
        );
        assert_eq!(
            prefix_parser.parse("kB"),
            PrefixParserResult::UnitIdentifier(
                Span::dummy(),
                Prefix::kilo(),
                "B".into(),
                "byte".into()
            )
        );
        assert_eq!(
            prefix_parser.parse("MB"),
            PrefixParserResult::UnitIdentifier(
                Span::dummy(),
                Prefix::mega(),
                "B".into(),
                "byte".into()
            )
        );
        assert_eq!(
            prefix_parser.parse("KiB"),
            PrefixParserResult::UnitIdentifier(
                Span::dummy(),
                Prefix::kibi(),
                "B".into(),
                "byte".into()
            )
        );
        assert_eq!(
            prefix_parser.parse("MiB"),
            PrefixParserResult::UnitIdentifier(
                Span::dummy(),
                Prefix::mebi(),
                "B".into(),
                "byte".into()
            )
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
