use std::fmt::Display;

use itertools::Itertools;
use num_traits::{ToPrimitive, Zero};

use crate::{
    arithmetic::{pretty_exponent, Exponent, Power, Rational},
    number::Number,
    prefix::Prefix,
    product::{Canonicalize, Product},
};

pub type ConversionFactor = Number;

/// A unit can either be a base/fundamental unit or it is derived from another unit.
/// In the latter case, a conversion factor to the defining unit has to be specified.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnitKind {
    Base,
    Derived(ConversionFactor, Unit),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnitIdentifier {
    pub name: String,
    pub canonical_name: String,
    kind: UnitKind,
}

#[derive(Clone)]
pub struct BaseUnitAndFactor(pub Unit, pub Number);

impl std::iter::Product for BaseUnitAndFactor {
    fn product<I: Iterator<Item = Self>>(iter: I) -> Self {
        let (fst, snd) = iter.tee();
        BaseUnitAndFactor(fst.map(|i| i.0).product(), snd.map(|i| i.1).product())
    }
}

impl UnitIdentifier {
    pub fn is_base(&self) -> bool {
        matches!(self.kind, UnitKind::Base)
    }

    pub fn base_unit_and_factor(&self) -> BaseUnitAndFactor {
        match &self.kind {
            UnitKind::Base => BaseUnitAndFactor(
                Unit::new_base(&self.name, &self.canonical_name),
                Number::from_f64(1.0),
            ),
            UnitKind::Derived(factor, defining_unit) => {
                let BaseUnitAndFactor(base_unit, defining_unit_factor) = defining_unit
                    .iter()
                    .map(
                        |UnitFactor {
                             unit_id,
                             prefix,
                             exponent,
                         }| {
                            let BaseUnitAndFactor(base_unit, base_unit_factor) =
                                unit_id.base_unit_and_factor();

                            BaseUnitAndFactor(
                                base_unit.power(*exponent),
                                (prefix.factor() * base_unit_factor)
                                    .pow(&Number::from_f64(exponent.to_f64().unwrap())),
                            )
                        },
                    )
                    .product();

                BaseUnitAndFactor(base_unit, *factor * defining_unit_factor)
            }
        }
    }

    pub fn sort_key(&self) -> Vec<(String, Exponent)> {
        use num_integer::Integer;

        // TODO: this is more or less a hack. instead of properly sorting by physical
        // dimension, we sort by the name of the corresponding base unit(s).
        match &self.kind {
            UnitKind::Base => vec![(self.name.clone(), Exponent::from_integer(1))],
            UnitKind::Derived(_, defining_unit) => {
                let base_unit = defining_unit.to_base_unit_representation().0;
                let mut key: Vec<_> = base_unit
                    .canonicalized()
                    .iter()
                    .flat_map(|f| {
                        let mut k = f.unit_id.sort_key();
                        debug_assert!(k.len() == 1);
                        k[0].1 = f.exponent;
                        k
                    })
                    .collect();

                if !key.is_empty() {
                    // Normalize the sign of the exponents. This is useful to consider
                    // 's' and 'Hz' for merging.
                    if key[0].1 < 0.into() {
                        key.iter_mut().for_each(|p| p.1 = -p.1);
                    }

                    // Multiply by the product of all divisors to make all exponents
                    // integers. This is needed for the next step.
                    let factor: i128 = key.iter().map(|p| p.1.numer()).product();

                    key.iter_mut().for_each(|p| p.1 *= factor);

                    // Now divide every factor by the greatest common divisor. This is
                    // useful to consider g·m² and g²·m⁴ for merging (but not g·m² and g·m³).
                    debug_assert!(key[0].1.is_integer());
                    let mut common_divisor: i128 = key[0].1.to_integer();
                    for p in &key[1..] {
                        debug_assert!(p.1.is_integer());
                        common_divisor = common_divisor.gcd(&p.1.to_integer());
                    }

                    key.iter_mut().for_each(|p| p.1 /= common_divisor);
                }

                key
            }
        }
    }
}

impl PartialOrd for UnitIdentifier {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.sort_key().partial_cmp(&other.sort_key())
    }
}

impl Ord for UnitIdentifier {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.sort_key().cmp(&other.sort_key())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnitFactor {
    pub unit_id: UnitIdentifier,
    pub prefix: Prefix,
    pub exponent: Exponent,
}

impl Canonicalize for UnitFactor {
    type MergeKey = (Prefix, UnitIdentifier);

    fn merge_key(&self) -> Self::MergeKey {
        (self.prefix, self.unit_id.clone())
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

impl Display for UnitFactor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}",
            self.prefix.as_string_short(),
            self.unit_id.canonical_name,
            pretty_exponent(&self.exponent)
        )
    }
}

pub type Unit = Product<UnitFactor, false>;

impl Unit {
    pub fn scalar() -> Self {
        Self::unity()
    }

    pub fn is_scalar(&self) -> bool {
        self == &Self::scalar()
    }

    pub fn new_base(name: &str, canonical_name: &str) -> Self {
        Unit::from_factor(UnitFactor {
            prefix: Prefix::none(),
            unit_id: UnitIdentifier {
                name: name.into(),
                canonical_name: canonical_name.into(),
                kind: UnitKind::Base,
            },
            exponent: Rational::from_integer(1),
        })
    }

    pub fn new_derived(
        name: &str,
        canonical_name: &str,
        factor: ConversionFactor,
        base_unit: Unit,
    ) -> Self {
        Unit::from_factor(UnitFactor {
            prefix: Prefix::none(),
            unit_id: UnitIdentifier {
                name: name.into(),
                canonical_name: canonical_name.into(),
                kind: UnitKind::Derived(factor, base_unit),
            },
            exponent: Rational::from_integer(1),
        })
    }

    pub fn with_prefix(self, prefix: Prefix) -> Self {
        let mut factors: Vec<_> = self.into_iter().collect();
        debug_assert!(!factors.is_empty());
        debug_assert!(factors[0].prefix == Prefix::none());
        factors[0].prefix = prefix;
        Self::from_factors(factors)
    }

    pub fn to_base_unit_representation(&self) -> (Self, ConversionFactor) {
        // TODO: reduce wrapping/unwrapping and duplication.

        let base_unit_representation = self
            .iter()
            .map(
                |UnitFactor {
                     prefix: _,
                     unit_id: base_unit,
                     exponent,
                 }| { base_unit.base_unit_and_factor().0.power(*exponent) },
            )
            .product::<Self>()
            .canonicalized();

        let factor = self
            .iter()
            .map(
                |UnitFactor {
                     prefix,
                     unit_id: base_unit,
                     exponent,
                 }| {
                    (prefix.factor() * base_unit.base_unit_and_factor().1)
                        .pow(&Number::from_f64(exponent.to_f64().unwrap())) // TODO do we want to use exponent.to_f64?
                },
            )
            .product();

        (base_unit_representation, factor)
    }

    #[cfg(test)]
    pub fn meter() -> Self {
        Self::new_base("meter", "m")
    }

    #[cfg(test)]
    pub fn centimeter() -> Self {
        Self::new_base("meter", "m").with_prefix(Prefix::centi())
    }

    #[cfg(test)]
    pub fn millimeter() -> Self {
        Self::new_base("meter", "m").with_prefix(Prefix::milli())
    }

    #[cfg(test)]
    pub fn kilometer() -> Self {
        Self::new_base("meter", "m").with_prefix(Prefix::kilo())
    }

    #[cfg(test)]
    pub fn second() -> Self {
        Self::new_base("second", "s")
    }

    #[cfg(test)]
    pub fn gram() -> Self {
        Self::new_base("gram", "g")
    }

    #[cfg(test)]
    pub fn kilogram() -> Self {
        Self::gram().with_prefix(Prefix::kilo())
    }

    #[cfg(test)]
    pub fn kelvin() -> Self {
        Self::new_base("kelvin", "K")
    }

    #[cfg(test)]
    pub fn radian() -> Self {
        Self::new_base("radian", "rad")
    }

    #[cfg(test)]
    pub fn degree() -> Self {
        Self::new_derived(
            "degree",
            "°",
            Number::from_f64(std::f64::consts::PI / 180.0),
            Self::radian(),
        )
    }

    #[cfg(test)]
    pub fn percent() -> Self {
        Self::new_derived("percent", "%", Number::from_f64(1e-2), Self::scalar())
    }

    #[cfg(test)]
    pub fn hertz() -> Self {
        Self::new_derived(
            "hertz",
            "Hz",
            Number::from_f64(1.0),
            Unit::second().powi(-1),
        )
    }

    #[cfg(test)]
    pub fn newton() -> Self {
        Self::new_derived(
            "newton",
            "N",
            Number::from_f64(1.0),
            Unit::kilogram() * Unit::meter() / Unit::second().powi(2),
        )
    }

    #[cfg(test)]
    pub fn minute() -> Self {
        Self::new_derived("minute", "min", Number::from_f64(60.0), Self::second())
    }

    #[cfg(test)]
    pub fn hour() -> Self {
        Self::new_derived("hour", "h", Number::from_f64(60.0), Self::minute())
    }

    #[cfg(test)]
    pub fn kph() -> Self {
        Self::new_derived(
            "kilometer_per_hour",
            "kph",
            Number::from_f64(1.0),
            Self::kilometer() / Self::hour(),
        )
    }

    #[cfg(test)]
    pub fn inch() -> Self {
        Self::new_derived("inch", "in", Number::from_f64(0.0254), Self::meter())
    }

    #[cfg(test)]
    pub fn foot() -> Self {
        Self::new_derived("foot", "ft", Number::from_f64(12.0), Self::inch())
    }

    #[cfg(test)]
    pub fn yard() -> Self {
        Self::new_derived("yard", "yd", Number::from_f64(3.0), Self::foot())
    }

    #[cfg(test)]
    pub fn mile() -> Self {
        Self::new_derived("mile", "mi", Number::from_f64(1760.0), Self::yard())
    }

    #[cfg(test)]
    pub fn bit() -> Self {
        Self::new_base("bit", "B")
    }

    #[cfg(test)]
    pub fn byte() -> Self {
        Self::new_derived("byte", "B", Number::from_f64(8.0), Self::bit())
    }
}

impl Display for Unit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.as_string(|f| f.exponent, "·", "/"))
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
                    canonical_name: "m".into(),
                    kind: UnitKind::Base,
                },
                exponent: Rational::from_integer(1),
            },
            UnitFactor {
                prefix: Prefix::none(),
                unit_id: UnitIdentifier {
                    name: "second".into(),
                    canonical_name: "s".into(),
                    kind: UnitKind::Base,
                },
                exponent: Rational::from_integer(-1),
            },
        ]);

        assert_eq!(Unit::meter() / Unit::second(), meter_per_second);
    }

    #[test]
    fn canonicalization() {
        let assert_same_representation = |lhs: Unit, rhs: Unit| {
            // we collect the unit factors into a vector here instead of directly comaring the units.
            // Otherwise the tests would always succeed because the PartialEq implementation on units
            // performs canonicalization.
            assert_eq!(
                lhs.into_iter().collect::<Vec<_>>(),
                rhs.into_iter().collect::<Vec<_>>()
            );
        };

        {
            let unit = Unit::meter() * Unit::second() * Unit::meter() * Unit::second().powi(2);
            assert_same_representation(
                unit.canonicalized(),
                Unit::meter().powi(2) * Unit::second().powi(3),
            );
        }
        {
            let unit = Unit::meter() * Unit::second() * Unit::meter() * Unit::hertz();
            assert_same_representation(
                unit.canonicalized(),
                Unit::meter().powi(2) * Unit::second() * Unit::hertz(),
            );
        }
        {
            let unit = Unit::meter() * Unit::second() * Unit::millimeter();
            assert_same_representation(
                unit.canonicalized(),
                Unit::millimeter() * Unit::meter() * Unit::second(),
            );
        }
        {
            let unit = Unit::meter() * Unit::second() * Unit::meter() * Unit::second().powi(-1);
            assert_same_representation(unit.canonicalized(), Unit::meter().powi(2));
        }
        {
            let unit =
                Unit::meter().powi(-1) * Unit::second() * Unit::meter() * Unit::second().powi(-1);
            assert_same_representation(unit.canonicalized(), Unit::scalar());
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
                    canonical_name: "m".into(),
                    kind: UnitKind::Base,
                },
                exponent: Rational::from_integer(1),
            }])
        );
    }

    #[test]
    fn to_base_unit_representation_basic() {
        let hour = Unit::hour();
        let (base_unit_representation, conversion_factor) = hour.to_base_unit_representation();
        assert_eq!(base_unit_representation, Unit::second());
        assert_relative_eq!(conversion_factor.to_f64(), 3600.0, epsilon = 1e-6);
    }

    #[test]
    fn to_base_unit_representation_percent() {
        let percent = Unit::percent();
        let (base_unit_representation, conversion_factor) = percent.to_base_unit_representation();
        assert_eq!(base_unit_representation, Unit::scalar());
        assert_eq!(conversion_factor.to_f64(), 0.01);
    }

    #[test]
    fn to_base_unit_representation_combined() {
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

    #[test]
    fn to_string() {
        assert_eq!(Unit::meter().to_string(), "m");
        assert_eq!(Unit::meter().powi(2).to_string(), "m²");
        assert_eq!(Unit::meter().powi(3).to_string(), "m³");
        assert_eq!(Unit::meter().powi(4).to_string(), "m⁴");
        assert_eq!(Unit::meter().powi(8).to_string(), "m^8");

        assert_eq!(Unit::meter().powi(-1).to_string(), "m⁻¹");
        assert_eq!(Unit::meter().powi(-4).to_string(), "m⁻⁴");
        assert_eq!(Unit::meter().powi(-8).to_string(), "m^(-8)");

        assert_eq!(
            (Unit::meter() * Unit::meter() * Unit::second())
                .canonicalized()
                .to_string(),
            "m²·s"
        );
        assert_eq!(
            (Unit::meter() * Unit::second() * Unit::second())
                .canonicalized()
                .to_string(),
            "m·s²"
        );

        assert_eq!(
            (Unit::meter() / Unit::second()).canonicalized().to_string(),
            "m/s"
        );
        assert_eq!(
            (Unit::meter() / (Unit::second() * Unit::second()))
                .canonicalized()
                .to_string(),
            "m/s²"
        );

        assert_eq!(
            (Unit::kilometer() * Unit::second() * Unit::second())
                .canonicalized()
                .to_string(),
            "km·s²"
        );
        assert_eq!(
            (Unit::meter() / (Unit::second() * Unit::second() * Unit::kilogram()))
                .canonicalized()
                .to_string(),
            "m/(kg·s²)"
        );
        assert_eq!(
            (Unit::meter() * Unit::second().with_prefix(Prefix::milli()) * Unit::second())
                .canonicalized()
                .to_string(),
            "m·ms·s"
        );

        assert_eq!(Unit::meter().with_prefix(Prefix::micro()).to_string(), "µm");
        assert_eq!(Unit::meter().with_prefix(Prefix::milli()).to_string(), "mm");
        assert_eq!(Unit::meter().with_prefix(Prefix::centi()).to_string(), "cm");
        assert_eq!(Unit::meter().with_prefix(Prefix::deci()).to_string(), "dm");
        assert_eq!(Unit::meter().with_prefix(Prefix::hecto()).to_string(), "hm");
        assert_eq!(Unit::meter().with_prefix(Prefix::kilo()).to_string(), "km");
        assert_eq!(Unit::second().with_prefix(Prefix::mega()).to_string(), "Ms");
        assert_eq!(Unit::second().with_prefix(Prefix::giga()).to_string(), "Gs");
        assert_eq!(Unit::second().with_prefix(Prefix::tera()).to_string(), "Ts");
        assert_eq!(
            Unit::second()
                .with_prefix(Prefix::tera())
                .powi(2)
                .to_string(),
            "Ts²"
        );
        assert_eq!(Unit::byte().with_prefix(Prefix::kibi()).to_string(), "KiB");
        assert_eq!(Unit::byte().with_prefix(Prefix::mebi()).to_string(), "MiB");
        assert_eq!(Unit::byte().with_prefix(Prefix::gibi()).to_string(), "GiB");
        assert_eq!(
            Unit::byte().with_prefix(Prefix::gibi()).powi(2).to_string(),
            "GiB²"
        );
    }
}
