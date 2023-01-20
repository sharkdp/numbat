use crate::arithmetic::{Power, Rational};
use crate::number::Number;
use crate::unit::Unit;

use num_traits::FromPrimitive;
use thiserror::Error;

#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum ConversionError {
    #[error("Conversion error: unit '{0}' can not be converted to '{1}'")]
    IncompatibleUnits(Unit, Unit),
}

pub type Result<T> = std::result::Result<T, ConversionError>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Quantity {
    value: Number,
    unit: Unit,
}

impl Quantity {
    pub fn new(value: Number, unit: Unit) -> Self {
        Quantity { value, unit }
    }

    pub fn from_scalar(value: f64) -> Quantity {
        Quantity::new(Number::from_f64(value), Unit::scalar())
    }

    pub fn from_unit(unit: Unit) -> Quantity {
        Quantity::new(Number::from_f64(1.0), unit)
    }

    pub fn unit(&self) -> &Unit {
        &self.unit
    }

    pub fn is_zero(&self) -> bool {
        self.value.to_f64() == 0.0
    }

    pub fn convert_to(&self, unit: &Unit) -> Result<Quantity> {
        if &self.unit == unit || self.is_zero() {
            Ok(Quantity::new(self.value.clone(), unit.clone()))
        } else {
            Err(ConversionError::IncompatibleUnits(
                self.unit.clone(),
                unit.clone(),
            ))
        }
    }

    pub fn as_scalar(&self) -> Result<Number> {
        Ok(self.convert_to(&Unit::scalar())?.value)
    }

    pub fn unsafe_value(&self) -> &Number {
        &self.value
    }

    pub fn power(self, exp: Quantity) -> Result<Self> {
        let exponent_as_scalar = exp.as_scalar()?.to_f64();
        Ok(Quantity::new(
            Number::from_f64(self.value.to_f64().powf(exponent_as_scalar)),
            self.unit
                .power(Rational::from_f64(exponent_as_scalar).unwrap()), // TODO: error handling; can this really handle rational exponents?
        ))
    }
}

impl From<&Number> for Quantity {
    fn from(n: &Number) -> Self {
        Quantity::from_scalar(n.to_f64())
    }
}

// TODO(minor): do we want to implement this for references instead of values?
impl std::ops::Add for Quantity {
    type Output = Result<Quantity>;

    fn add(self, rhs: Self) -> Self::Output {
        rhs.convert_to(&self.unit)?;
        Ok(Quantity {
            value: self.value + rhs.value,
            unit: self.unit,
        })
    }
}

impl std::ops::Sub for Quantity {
    type Output = Result<Quantity>;

    fn sub(self, rhs: Self) -> Self::Output {
        rhs.convert_to(&self.unit)?;
        Ok(Quantity {
            value: self.value - rhs.value,
            unit: self.unit,
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

#[test]
fn test_convert() {
    let q = Quantity::new(Number::from_f64(2.0), Unit::new_standard("meter"));
    assert!(q.convert_to(&Unit::new_standard("meter")).is_ok());

    assert!(q.convert_to(&Unit::new_standard("second")).is_err());
    assert!(q.convert_to(&Unit::scalar()).is_err());
}
