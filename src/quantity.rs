use crate::number::Number;
use crate::unit::Unit;

use thiserror::Error;

#[derive(Clone, Error, Debug, PartialEq, Eq)]
pub enum UnitError {}

pub type Result<T> = std::result::Result<T, UnitError>;

#[derive(Debug, Clone, PartialEq)]
pub struct Quantity {
    value: Number,
    unit: Unit,
}

impl Quantity {
    pub fn scalar(value: f64) -> Quantity {
        Quantity {
            value: Number::from_f64(value),
            unit: Unit::scalar(),
        }
    }

    pub fn unit(unit: Unit) -> Quantity {
        Quantity {
            value: Number::from_f64(1.0),
            unit,
        }
    }

    pub fn is_zero(&self) -> bool {
        self.value.to_f64() == 0.0
    }
}

impl From<&Number> for Quantity {
    fn from(n: &Number) -> Self {
        Quantity::scalar(n.to_f64())
    }
}

// TODO: do we want to implement this for references instead of values?
impl std::ops::Add for Quantity {
    type Output = Result<Quantity>;

    fn add(self, rhs: Self) -> Self::Output {
        Ok(Quantity {
            value: self.value + rhs.value,
            unit: Unit::scalar(),
        })
    }
}

impl std::ops::Sub for Quantity {
    type Output = Result<Quantity>;

    fn sub(self, rhs: Self) -> Self::Output {
        Ok(Quantity {
            value: self.value - rhs.value,
            unit: Unit::scalar(),
        })
    }
}

impl std::ops::Mul for Quantity {
    type Output = Result<Quantity>;

    fn mul(self, rhs: Self) -> Self::Output {
        Ok(Quantity {
            value: self.value * rhs.value,
            unit: self.unit.multiply(rhs.unit),
        })
    }
}

impl std::ops::Div for Quantity {
    type Output = Result<Quantity>;

    fn div(self, rhs: Self) -> Self::Output {
        Ok(Quantity {
            value: self.value / rhs.value,
            unit: self.unit.divide(rhs.unit),
        })
    }
}

impl std::ops::Neg for Quantity {
    type Output = Quantity;

    fn neg(self) -> Self::Output {
        Quantity {
            value: -self.value,
            unit: self.unit,
        }
    }
}

impl std::fmt::Display for Quantity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO: we do not always want to canonicalize
        let mut unit_canonicalized = self.unit.clone();
        unit_canonicalized.canonicalize();
        write!(f, "{:.6} {}", self.value.to_f64(), unit_canonicalized)
    }
}
