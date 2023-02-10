use once_cell::sync::OnceCell;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Prefix {
    Decimal(i32),
}

static PREFIXES: OnceCell<Vec<(&'static str, &'static str, Prefix)>> = OnceCell::new();

pub fn prefixes() -> &'static [(&'static str, &'static str, Prefix)] {
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

    &prefixes
}
