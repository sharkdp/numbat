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

    pub fn as_string_short(&self) -> String {
        match self {
            Prefix::Metric(-30) => "q".into(),
            Prefix::Metric(-27) => "r".into(),
            Prefix::Metric(-24) => "y".into(),
            Prefix::Metric(-21) => "z".into(),
            Prefix::Metric(-18) => "a".into(),
            Prefix::Metric(-15) => "f".into(),
            Prefix::Metric(-12) => "p".into(),
            Prefix::Metric(-9) => "n".into(),
            Prefix::Metric(-6) => "µ".into(),
            Prefix::Metric(-3) => "m".into(),
            Prefix::Metric(-2) => "c".into(),
            Prefix::Metric(-1) => "d".into(),
            Prefix::Metric(0) => "".into(),
            Prefix::Metric(1) => "da".into(),
            Prefix::Metric(2) => "h".into(),
            Prefix::Metric(3) => "k".into(),
            Prefix::Metric(6) => "M".into(),
            Prefix::Metric(9) => "G".into(),
            Prefix::Metric(12) => "T".into(),
            Prefix::Metric(15) => "P".into(),
            Prefix::Metric(18) => "E".into(),
            Prefix::Metric(21) => "Z".into(),
            Prefix::Metric(24) => "Y".into(),
            Prefix::Metric(27) => "R".into(),
            Prefix::Metric(30) => "Q".into(),

            Prefix::Metric(n) => format!("<prefix 10^{n}>"),

            Prefix::Binary(0) => "".into(),
            Prefix::Binary(10) => "Ki".into(),
            Prefix::Binary(20) => "Mi".into(),
            Prefix::Binary(30) => "Gi".into(),
            Prefix::Binary(40) => "Ti".into(),
            Prefix::Binary(50) => "Pi".into(),
            Prefix::Binary(60) => "Ei".into(),
            Prefix::Binary(70) => "Zi".into(),
            Prefix::Binary(80) => "Yi".into(),

            Prefix::Binary(n) => format!("<prefix 2^{n}>"),
        }
    }

    pub fn as_string_long(&self) -> String {
        match self {
            Prefix::Metric(-30) => "quecto".into(),
            Prefix::Metric(-27) => "ronto".into(),
            Prefix::Metric(-24) => "yocto".into(),
            Prefix::Metric(-21) => "zepto".into(),
            Prefix::Metric(-18) => "atto".into(),
            Prefix::Metric(-15) => "femto".into(),
            Prefix::Metric(-12) => "pico".into(),
            Prefix::Metric(-9) => "nano".into(),
            Prefix::Metric(-6) => "micro".into(),
            Prefix::Metric(-3) => "milli".into(),
            Prefix::Metric(-2) => "centi".into(),
            Prefix::Metric(-1) => "deci".into(),
            Prefix::Metric(0) => "".into(),
            Prefix::Metric(1) => "deca".into(),
            Prefix::Metric(2) => "hecto".into(),
            Prefix::Metric(3) => "kilo".into(),
            Prefix::Metric(6) => "mega".into(),
            Prefix::Metric(9) => "giga".into(),
            Prefix::Metric(12) => "tera".into(),
            Prefix::Metric(15) => "peta".into(),
            Prefix::Metric(18) => "exa".into(),
            Prefix::Metric(21) => "zetta".into(),
            Prefix::Metric(24) => "yotta".into(),
            Prefix::Metric(27) => "ronna".into(),
            Prefix::Metric(30) => "quetta".into(),

            Prefix::Metric(n) => format!("<prefix 10^{n}>"),

            Prefix::Binary(0) => "".into(),
            Prefix::Binary(10) => "kibi".into(),
            Prefix::Binary(20) => "mebi".into(),
            Prefix::Binary(30) => "gibi".into(),
            Prefix::Binary(40) => "tebi".into(),
            Prefix::Binary(50) => "pebi".into(),
            Prefix::Binary(60) => "exbi".into(),
            Prefix::Binary(70) => "zebi".into(),
            Prefix::Binary(80) => "yobi".into(),

            Prefix::Binary(n) => format!("<prefix 2^{n}>"),
        }
    }
}
