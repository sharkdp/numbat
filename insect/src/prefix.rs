use std::fmt::Display;

use crate::number::Number;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Prefix {
    Decimal(i32),
}

impl Prefix {
    pub fn factor(&self) -> Number {
        match self {
            Prefix::Decimal(exp) => Number::from_f64(10.0f64.powi(*exp)),
        }
    }
}

impl Display for Prefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Prefix::Decimal(-3) => write!(f, "milli"),
            Prefix::Decimal(-2) => write!(f, "centi"),
            Prefix::Decimal(-1) => write!(f, "deci"),
            Prefix::Decimal(0) => write!(f, ""),
            Prefix::Decimal(3) => write!(f, "kilo"),
            Prefix::Decimal(6) => write!(f, "mega"),
            _ => write!(f, "<prefix>"), // TODO
        }
    }
}
