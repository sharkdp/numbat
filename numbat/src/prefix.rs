use compact_str::{format_compact, CompactString};

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
        match self {
            Prefix::Metric(-30) => CompactString::const_new("q"),
            Prefix::Metric(-27) => CompactString::const_new("r"),
            Prefix::Metric(-24) => CompactString::const_new("y"),
            Prefix::Metric(-21) => CompactString::const_new("z"),
            Prefix::Metric(-18) => CompactString::const_new("a"),
            Prefix::Metric(-15) => CompactString::const_new("f"),
            Prefix::Metric(-12) => CompactString::const_new("p"),
            Prefix::Metric(-9) => CompactString::const_new("n"),
            Prefix::Metric(-6) => CompactString::const_new("µ"),
            Prefix::Metric(-3) => CompactString::const_new("m"),
            Prefix::Metric(-2) => CompactString::const_new("c"),
            Prefix::Metric(-1) => CompactString::const_new("d"),
            Prefix::Metric(0) => CompactString::const_new(""),
            Prefix::Metric(1) => CompactString::const_new("da"),
            Prefix::Metric(2) => CompactString::const_new("h"),
            Prefix::Metric(3) => CompactString::const_new("k"),
            Prefix::Metric(6) => CompactString::const_new("M"),
            Prefix::Metric(9) => CompactString::const_new("G"),
            Prefix::Metric(12) => CompactString::const_new("T"),
            Prefix::Metric(15) => CompactString::const_new("P"),
            Prefix::Metric(18) => CompactString::const_new("E"),
            Prefix::Metric(21) => CompactString::const_new("Z"),
            Prefix::Metric(24) => CompactString::const_new("Y"),
            Prefix::Metric(27) => CompactString::const_new("R"),
            Prefix::Metric(30) => CompactString::const_new("Q"),

            Prefix::Metric(n) => format_compact!("<prefix 10^{n}>"),

            Prefix::Binary(0) => CompactString::const_new(""),
            Prefix::Binary(10) => CompactString::const_new("Ki"),
            Prefix::Binary(20) => CompactString::const_new("Mi"),
            Prefix::Binary(30) => CompactString::const_new("Gi"),
            Prefix::Binary(40) => CompactString::const_new("Ti"),
            Prefix::Binary(50) => CompactString::const_new("Pi"),
            Prefix::Binary(60) => CompactString::const_new("Ei"),
            Prefix::Binary(70) => CompactString::const_new("Zi"),
            Prefix::Binary(80) => CompactString::const_new("Yi"),

            Prefix::Binary(n) => format_compact!("<prefix 2^{n}>"),
        }
    }

    pub fn as_string_long(&self) -> CompactString {
        match self {
            Prefix::Metric(-30) => CompactString::const_new("quecto"),
            Prefix::Metric(-27) => CompactString::const_new("ronto"),
            Prefix::Metric(-24) => CompactString::const_new("yocto"),
            Prefix::Metric(-21) => CompactString::const_new("zepto"),
            Prefix::Metric(-18) => CompactString::const_new("atto"),
            Prefix::Metric(-15) => CompactString::const_new("femto"),
            Prefix::Metric(-12) => CompactString::const_new("pico"),
            Prefix::Metric(-9) => CompactString::const_new("nano"),
            Prefix::Metric(-6) => CompactString::const_new("micro"),
            Prefix::Metric(-3) => CompactString::const_new("milli"),
            Prefix::Metric(-2) => CompactString::const_new("centi"),
            Prefix::Metric(-1) => CompactString::const_new("deci"),
            Prefix::Metric(0) => CompactString::const_new(""),
            Prefix::Metric(1) => CompactString::const_new("deca"),
            Prefix::Metric(2) => CompactString::const_new("hecto"),
            Prefix::Metric(3) => CompactString::const_new("kilo"),
            Prefix::Metric(6) => CompactString::const_new("mega"),
            Prefix::Metric(9) => CompactString::const_new("giga"),
            Prefix::Metric(12) => CompactString::const_new("tera"),
            Prefix::Metric(15) => CompactString::const_new("peta"),
            Prefix::Metric(18) => CompactString::const_new("exa"),
            Prefix::Metric(21) => CompactString::const_new("zetta"),
            Prefix::Metric(24) => CompactString::const_new("yotta"),
            Prefix::Metric(27) => CompactString::const_new("ronna"),
            Prefix::Metric(30) => CompactString::const_new("quetta"),

            Prefix::Metric(n) => format_compact!("<prefix 10^{n}>"),

            Prefix::Binary(0) => CompactString::const_new(""),
            Prefix::Binary(10) => CompactString::const_new("kibi"),
            Prefix::Binary(20) => CompactString::const_new("mebi"),
            Prefix::Binary(30) => CompactString::const_new("gibi"),
            Prefix::Binary(40) => CompactString::const_new("tebi"),
            Prefix::Binary(50) => CompactString::const_new("pebi"),
            Prefix::Binary(60) => CompactString::const_new("exbi"),
            Prefix::Binary(70) => CompactString::const_new("zebi"),
            Prefix::Binary(80) => CompactString::const_new("yobi"),

            Prefix::Binary(n) => format_compact!("<prefix 2^{n}>"),
        }
    }
}
