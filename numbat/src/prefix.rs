use compact_str::{CompactString, format_compact};

use crate::number::Number;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Prefix {
    /// Represents a metric/decimal prefix symbolizing 10^n
    Metric(i32),
    /// Represents a binary prefix symbolizing 2^n
    Binary(i32),
}

impl Prefix {
    pub fn factor(&self) -> Number {
        match self {
            Prefix::Metric(exp) => Number::from_f64(10.0f64.powi(*exp)),
            Prefix::Binary(exp) => Number::from_f64(2.0f64.powi(*exp)),
        }
    }

    pub fn none() -> Self {
        Prefix::Metric(0)
    }

    #[cfg(test)]
    pub fn micro() -> Self {
        Prefix::Metric(-6)
    }

    #[cfg(test)]
    pub fn milli() -> Self {
        Prefix::Metric(-3)
    }

    #[cfg(test)]
    pub fn centi() -> Self {
        Prefix::Metric(-2)
    }

    #[cfg(test)]
    pub fn deci() -> Self {
        Prefix::Metric(-1)
    }

    #[cfg(test)]
    pub fn deca() -> Self {
        Prefix::Metric(1)
    }

    #[cfg(test)]
    pub fn hecto() -> Self {
        Prefix::Metric(2)
    }

    #[cfg(test)]
    pub fn kilo() -> Self {
        Prefix::Metric(3)
    }

    #[cfg(test)]
    pub fn mega() -> Self {
        Prefix::Metric(6)
    }

    #[cfg(test)]
    pub fn giga() -> Self {
        Prefix::Metric(9)
    }

    #[cfg(test)]
    pub fn tera() -> Self {
        Prefix::Metric(12)
    }

    #[cfg(test)]
    pub fn kibi() -> Self {
        Prefix::Binary(10)
    }

    #[cfg(test)]
    pub fn mebi() -> Self {
        Prefix::Binary(20)
    }

    #[cfg(test)]
    pub fn gibi() -> Self {
        Prefix::Binary(30)
    }

    pub fn is_none(&self) -> bool {
        match self {
            Prefix::Metric(0) => true,
            Prefix::Binary(0) => true,
            Prefix::Metric(_) => false,
            Prefix::Binary(_) => false,
        }
    }

    pub fn is_metric(&self) -> bool {
        matches!(self, Prefix::Metric(_))
    }

    pub fn is_binary(&self) -> bool {
        matches!(self, Prefix::Binary(_))
    }

    pub fn as_string_short(&self) -> CompactString {
        CompactString::const_new(match self {
            Prefix::Metric(-30) => "q",
            Prefix::Metric(-27) => "r",
            Prefix::Metric(-24) => "y",
            Prefix::Metric(-21) => "z",
            Prefix::Metric(-18) => "a",
            Prefix::Metric(-15) => "f",
            Prefix::Metric(-12) => "p",
            Prefix::Metric(-9) => "n",
            Prefix::Metric(-6) => "µ",
            Prefix::Metric(-3) => "m",
            Prefix::Metric(-2) => "c",
            Prefix::Metric(-1) => "d",
            Prefix::Metric(0) => "",
            Prefix::Metric(1) => "da",
            Prefix::Metric(2) => "h",
            Prefix::Metric(3) => "k",
            Prefix::Metric(6) => "M",
            Prefix::Metric(9) => "G",
            Prefix::Metric(12) => "T",
            Prefix::Metric(15) => "P",
            Prefix::Metric(18) => "E",
            Prefix::Metric(21) => "Z",
            Prefix::Metric(24) => "Y",
            Prefix::Metric(27) => "R",
            Prefix::Metric(30) => "Q",

            Prefix::Metric(n) => return format_compact!("<prefix 10^{n}>"),

            Prefix::Binary(0) => "",
            Prefix::Binary(10) => "Ki",
            Prefix::Binary(20) => "Mi",
            Prefix::Binary(30) => "Gi",
            Prefix::Binary(40) => "Ti",
            Prefix::Binary(50) => "Pi",
            Prefix::Binary(60) => "Ei",
            Prefix::Binary(70) => "Zi",
            Prefix::Binary(80) => "Yi",
            Prefix::Binary(90) => "Ri",
            Prefix::Binary(100) => "Qi",

            Prefix::Binary(n) => return format_compact!("<prefix 2^{n}>"),
        })
    }

    pub fn as_string_long(&self) -> CompactString {
        CompactString::const_new(match self {
            Prefix::Metric(-30) => "quecto",
            Prefix::Metric(-27) => "ronto",
            Prefix::Metric(-24) => "yocto",
            Prefix::Metric(-21) => "zepto",
            Prefix::Metric(-18) => "atto",
            Prefix::Metric(-15) => "femto",
            Prefix::Metric(-12) => "pico",
            Prefix::Metric(-9) => "nano",
            Prefix::Metric(-6) => "micro",
            Prefix::Metric(-3) => "milli",
            Prefix::Metric(-2) => "centi",
            Prefix::Metric(-1) => "deci",
            Prefix::Metric(0) => "",
            Prefix::Metric(1) => "deca",
            Prefix::Metric(2) => "hecto",
            Prefix::Metric(3) => "kilo",
            Prefix::Metric(6) => "mega",
            Prefix::Metric(9) => "giga",
            Prefix::Metric(12) => "tera",
            Prefix::Metric(15) => "peta",
            Prefix::Metric(18) => "exa",
            Prefix::Metric(21) => "zetta",
            Prefix::Metric(24) => "yotta",
            Prefix::Metric(27) => "ronna",
            Prefix::Metric(30) => "quetta",

            Prefix::Metric(n) => return format_compact!("<prefix 10^{n}>"),

            Prefix::Binary(0) => "",
            Prefix::Binary(10) => "kibi",
            Prefix::Binary(20) => "mebi",
            Prefix::Binary(30) => "gibi",
            Prefix::Binary(40) => "tebi",
            Prefix::Binary(50) => "pebi",
            Prefix::Binary(60) => "exbi",
            Prefix::Binary(70) => "zebi",
            Prefix::Binary(80) => "yobi",
            Prefix::Binary(90) => "robi",
            Prefix::Binary(100) => "quebi",

            Prefix::Binary(n) => return format_compact!("<prefix 2^{n}>"),
        })
    }
}
