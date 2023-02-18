use std::fmt::{Display, Write};

use num_rational::Ratio;
use num_traits::{ToPrimitive, Zero};

use crate::{
    arithmetic::{Exponent, Power, Rational},
    number::Number,
    prefix::Prefix,
    product::{Canonicalize, Product},
};

pub type ConversionFactor = Number;

/// A unit can either be a base/fundamental unit or it is derived from one.
/// In the latter case, a conversion factor to the base unit has to be specified.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnitKind {
    Base,
    Derived(ConversionFactor, Unit),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnitIdentifier {
    name: String,
    unit_type: UnitKind,
}

impl UnitIdentifier {
    fn corresponding_base_unit(&self) -> Unit {
        match &self.unit_type {
            UnitKind::Base => Unit::new_base(&self.name),
            UnitKind::Derived(_, base_unit) => base_unit.clone(),
        }
    }

    fn conversion_factor(&self) -> Number {
        match &self.unit_type {
            UnitKind::Base => Number::from_f64(1.0),
            UnitKind::Derived(factor, _) => factor.clone(),
        }
    }
}

impl PartialOrd for UnitIdentifier {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.name.partial_cmp(&other.name)
    }
}

impl Ord for UnitIdentifier {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.name.cmp(&other.name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnitFactor {
    pub prefix: Prefix,
    pub unit_id: UnitIdentifier,
    pub exponent: Exponent,
}

impl Canonicalize for UnitFactor {
    type MergeKey = (Prefix, UnitIdentifier);

    fn merge_key(&self) -> Self::MergeKey {
        (self.prefix.clone(), self.unit_id.clone())
    }

    fn merge(self, other: Self) -> Self {
        UnitFactor {
            prefix: self.prefix,
            unit_id: self.unit_id,
            exponent: self.exponent + other.exponent,
        }
    }

    fn is_trivial(&self) -> bool {
        self.exponent == Rational::zero()
    }
}

impl Power for UnitFactor {
    fn power(self, e: Exponent) -> Self {
        UnitFactor {
            prefix: self.prefix,
            unit_id: self.unit_id,
            exponent: self.exponent * e,
        }
    }
}

pub type Unit = Product<UnitFactor, false>;

impl Unit {
    pub fn scalar() -> Self {
        Self::unity()
    }

    pub fn new_base(name: &str) -> Self {
        Unit::from_factor(UnitFactor {
            prefix: Prefix::none(),
            unit_id: UnitIdentifier {
                name: name.into(),
                unit_type: UnitKind::Base,
            },
            exponent: Rational::from_integer(1),
        })
    }

    pub fn new_derived(name: &str, factor: ConversionFactor, base_unit: Unit) -> Self {
        Unit::from_factor(UnitFactor {
            prefix: Prefix::none(),
            unit_id: UnitIdentifier {
                name: name.into(),
                unit_type: UnitKind::Derived(factor, base_unit),
            },
            exponent: Rational::from_integer(1),
        })
    }

    pub fn with_prefix(self, prefix: Prefix) -> Self {
        let mut factors: Vec<_> = self.into_iter().collect();
        assert!(!factors.is_empty());
        assert!(factors[0].prefix == Prefix::none());
        factors[0].prefix = prefix;
        Self::from_factors(factors)
    }

    pub fn to_base_unit_representation(&self) -> (Self, ConversionFactor) {
        let base_unit_representation = self
            .iter()
            .map(
                |UnitFactor {
                     prefix: _,
                     unit_id: base_unit,
                     exponent,
                 }| { base_unit.corresponding_base_unit().power(*exponent) },
            )
            .product();

        let factor = self
            .iter()
            .map(
                |UnitFactor {
                     prefix,
                     unit_id: base_unit,
                     exponent,
                 }| {
                    (prefix.factor() * base_unit.conversion_factor())
                        .pow(&Number::from_f64(exponent.to_f64().unwrap()))
                },
            ) // TODO: reduce wrapping/unwrapping; do we want to use exponent.to_f64?
            .product();

        (base_unit_representation, factor)
    }

    #[cfg(test)]
    fn meter() -> Self {
        Self::new_base("meter")
    }

    #[cfg(test)]
    fn second() -> Self {
        Self::new_base("second")
    }

    #[cfg(test)]
    fn mile() -> Self {
        Self::new_derived("mile", Number::from_f64(1609.344), Self::meter())
    }

    #[cfg(test)]
    fn hour() -> Self {
        Self::new_derived("hour", Number::from_f64(3600.0), Self::second())
    }
}

impl Display for Unit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = String::new();
        for &UnitFactor {
            prefix,
            unit_id: ref base_unit,
            exponent,
        } in self.iter()
        {
            result.push_str(&format!("{}", prefix));
            result.push_str(&base_unit.name);

            if exponent == Ratio::from_integer(5) {
                result.push('⁵');
            } else if exponent == Ratio::from_integer(4) {
                result.push('⁴');
            } else if exponent == Ratio::from_integer(3) {
                result.push('³');
            } else if exponent == Ratio::from_integer(2) {
                result.push('²');
            } else if exponent == Ratio::from_integer(1) {
            } else if exponent == Ratio::from_integer(-1) {
                result.push('⁻');
                result.push('¹');
            } else if exponent == Ratio::from_integer(-2) {
                result.push('⁻');
                result.push('²');
            } else if exponent == Ratio::from_integer(-3) {
                result.push('⁻');
                result.push('³');
            } else if exponent == Ratio::from_integer(-4) {
                result.push('⁻');
                result.push('⁴');
            } else if exponent == Ratio::from_integer(-5) {
                result.push('⁻');
                result.push('⁵');
            } else {
                write!(result, "^{}", exponent).unwrap();
            };
            result.push('·');
        }

        write!(f, "{}", result.trim_end_matches('·'))
    }
}

#[cfg(test)]
mod tests {
    use approx::assert_relative_eq;

    use super::*;

    #[test]
    fn division() {
        let meter_per_second = Unit::from_factors([
            UnitFactor {
                prefix: Prefix::none(),
                unit_id: UnitIdentifier {
                    name: "meter".into(),
                    unit_type: UnitKind::Base,
                },
                exponent: Rational::from_integer(1),
            },
            UnitFactor {
                prefix: Prefix::none(),
                unit_id: UnitIdentifier {
                    name: "second".into(),
                    unit_type: UnitKind::Base,
                },
                exponent: Rational::from_integer(-1),
            },
        ]);

        assert_eq!(Unit::meter() / Unit::second(), meter_per_second);
    }

    #[test]
    fn canonicalized() {
        {
            let unit = Unit::meter() * Unit::second() * Unit::meter();
            assert_eq!(
                unit.canonicalized(),
                Unit::meter() * Unit::meter() * Unit::second()
            );
        }
    }

    #[test]
    fn with_prefix() {
        let millimeter = Unit::meter().with_prefix(Prefix::milli());
        assert_eq!(
            millimeter,
            Unit::from_factors([UnitFactor {
                prefix: Prefix::Metric(-3),
                unit_id: UnitIdentifier {
                    name: "meter".into(),
                    unit_type: UnitKind::Base,
                },
                exponent: Rational::from_integer(1),
            }])
        );
    }

    #[test]
    fn to_base_unit_representation() {
        let mile_per_hour = Unit::mile() / Unit::hour();
        let (base_unit_representation, conversion_factor) =
            mile_per_hour.to_base_unit_representation();
        assert_eq!(base_unit_representation, Unit::meter() / Unit::second());
        assert_relative_eq!(
            conversion_factor.to_f64(),
            1609.344 / 3600.0,
            epsilon = 1e-6
        );
    }
}
