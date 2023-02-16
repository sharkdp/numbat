use std::fmt::Display;

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
    pub fn kilo() -> Self {
        Prefix::Metric(3)
    }

    #[cfg(test)]
    pub fn mega() -> Self {
        Prefix::Metric(6)
    }

    #[cfg(test)]
    pub fn kibi() -> Self {
        Prefix::Binary(10)
    }

    #[cfg(test)]
    pub fn mebi() -> Self {
        Prefix::Binary(20)
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
}

impl Display for Prefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Prefix::Metric(-30) => write!(f, "quecto"),
            Prefix::Metric(-27) => write!(f, "ronto"),
            Prefix::Metric(-24) => write!(f, "yocto"),
            Prefix::Metric(-21) => write!(f, "zepto"),
            Prefix::Metric(-18) => write!(f, "atto"),
            Prefix::Metric(-15) => write!(f, "femto"),
            Prefix::Metric(-12) => write!(f, "pico"),
            Prefix::Metric(-9) => write!(f, "nano"),
            Prefix::Metric(-6) => write!(f, "micro"),
            Prefix::Metric(-3) => write!(f, "milli"),
            Prefix::Metric(-2) => write!(f, "centi"),
            Prefix::Metric(-1) => write!(f, "deci"),
            Prefix::Metric(0) => write!(f, ""),
            Prefix::Metric(1) => write!(f, "deca"),
            Prefix::Metric(2) => write!(f, "hecto"),
            Prefix::Metric(3) => write!(f, "kilo"),
            Prefix::Metric(6) => write!(f, "mega"),
            Prefix::Metric(9) => write!(f, "giga"),
            Prefix::Metric(12) => write!(f, "tera"),
            Prefix::Metric(15) => write!(f, "peta"),
            Prefix::Metric(18) => write!(f, "exa"),
            Prefix::Metric(21) => write!(f, "zetta"),
            Prefix::Metric(24) => write!(f, "yotta"),
            Prefix::Metric(27) => write!(f, "ronna"),
            Prefix::Metric(30) => write!(f, "quetta"),

            Prefix::Metric(n) => write!(f, "<prefix 10^{}>", n),

            Prefix::Binary(0) => write!(f, ""),
            Prefix::Binary(10) => write!(f, "kibi"),
            Prefix::Binary(20) => write!(f, "mebi"),
            Prefix::Binary(30) => write!(f, "gibi"),
            Prefix::Binary(40) => write!(f, "tebi"),
            Prefix::Binary(50) => write!(f, "pebi"),
            Prefix::Binary(60) => write!(f, "exbi"),
            Prefix::Binary(70) => write!(f, "zebi"),
            Prefix::Binary(80) => write!(f, "yobi"),

            Prefix::Binary(n) => write!(f, "<prefix 2^{}>", n),
        }
    }
}
