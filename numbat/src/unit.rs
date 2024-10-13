use std::{fmt::Display, ops::Div};

use compact_str::{CompactString, ToCompactString};
use itertools::Itertools;
use num_traits::{ToPrimitive, Zero};

use crate::{
    arithmetic::{pretty_exponent, Exponent, Power, Rational},
    number::Number,
    prefix::Prefix,
    prefix_parser::AcceptsPrefix,
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
pub struct CanonicalName {
    pub name: CompactString,
    pub accepts_prefix: AcceptsPrefix,
}

impl CanonicalName {
    pub fn new(name: &str, accepts_prefix: AcceptsPrefix) -> Self {
        Self {
            name: name.into(),
            accepts_prefix,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnitIdentifier {
    pub name: CompactString,
    pub canonical_name: CanonicalName,
    kind: UnitKind,
}

#[derive(Clone, Debug)]
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

    pub fn unit_and_factor(&self) -> BaseUnitAndFactor {
        match &self.kind {
            UnitKind::Base => BaseUnitAndFactor(
                Unit::new_base(self.name.to_compact_string(), self.canonical_name.clone()),
                Number::from_f64(1.0),
            ),
            UnitKind::Derived(factor, defining_unit) => {
                BaseUnitAndFactor(defining_unit.clone(), *factor)
            }
        }
    }

    pub fn base_unit_and_factor(&self) -> BaseUnitAndFactor {
        match &self.kind {
            UnitKind::Base => BaseUnitAndFactor(
                Unit::new_base(self.name.to_compact_string(), self.canonical_name.clone()),
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

    pub fn sort_key(&self) -> Vec<(CompactString, Exponent)> {
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
                    let factor: i128 = key.iter().map(|p| p.1.denom()).product();

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
        Some(self.cmp(other))
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
        let prefix = if self.unit_id.canonical_name.accepts_prefix.short {
            self.prefix.as_string_short()
        } else {
            self.prefix.as_string_long()
        };

        write!(
            f,
            "{}{}{}",
            prefix,
            self.unit_id.canonical_name.name,
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

    pub fn new_base(name: CompactString, canonical_name: CanonicalName) -> Self {
        Unit::from_factor(UnitFactor {
            prefix: Prefix::none(),
            unit_id: UnitIdentifier {
                name,
                canonical_name,
                kind: UnitKind::Base,
            },
            exponent: Rational::from_integer(1),
        })
    }

    pub fn new_derived(
        name: CompactString,
        canonical_name: CanonicalName,
        factor: ConversionFactor,
        base_unit: Unit,
    ) -> Self {
        Unit::from_factor(UnitFactor {
            prefix: Prefix::none(),
            unit_id: UnitIdentifier {
                name,
                canonical_name,
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
        // TODO: reduce wrapping/unwrapping
        let mut base_unit_representation = Product::unity();
        let mut factor = Number::from_f64(1.0);

        for UnitFactor {
            unit_id: base_unit,
            prefix,
            exponent,
        } in self.iter()
        {
            base_unit_representation =
                base_unit_representation * base_unit.base_unit_and_factor().0.power(*exponent);
            factor = factor
                * (prefix.factor() * base_unit.base_unit_and_factor().1)
                    // TODO do we want to use exponent.to_f64?
                    .pow(&Number::from_f64(exponent.to_f64().unwrap()));
        }

        base_unit_representation.canonicalize();

        (base_unit_representation, factor)
    }

    #[cfg(test)]
    pub fn meter() -> Self {
        Self::new_base(
            CompactString::const_new("meter"),
            CanonicalName::new("m", AcceptsPrefix::only_short()),
        )
    }

    #[cfg(test)]
    pub fn centimeter() -> Self {
        Self::new_base(
            CompactString::const_new("meter"),
            CanonicalName::new("m", AcceptsPrefix::only_short()),
        )
        .with_prefix(Prefix::centi())
    }

    #[cfg(test)]
    pub fn millimeter() -> Self {
        Self::new_base(
            CompactString::const_new("meter"),
            CanonicalName::new("m", AcceptsPrefix::only_short()),
        )
        .with_prefix(Prefix::milli())
    }

    #[cfg(test)]
    pub fn kilometer() -> Self {
        Self::new_base(
            CompactString::const_new("meter"),
            CanonicalName::new("m", AcceptsPrefix::only_short()),
        )
        .with_prefix(Prefix::kilo())
    }

    #[cfg(test)]
    pub fn second() -> Self {
        Self::new_base(
            CompactString::const_new("second"),
            CanonicalName::new("s", AcceptsPrefix::only_short()),
        )
    }

    #[cfg(test)]
    pub fn gram() -> Self {
        Self::new_base(
            CompactString::const_new("gram"),
            CanonicalName::new("g", AcceptsPrefix::only_short()),
        )
    }

    #[cfg(test)]
    pub fn kilogram() -> Self {
        Self::gram().with_prefix(Prefix::kilo())
    }

    #[cfg(test)]
    pub fn kelvin() -> Self {
        Self::new_base(
            CompactString::const_new("kelvin"),
            CanonicalName::new("K", AcceptsPrefix::only_short()),
        )
    }

    #[cfg(test)]
    pub fn radian() -> Self {
        Self::new_derived(
            CompactString::const_new("radian"),
            CanonicalName::new("rad", AcceptsPrefix::only_long()),
            Number::from_f64(1.0),
            Self::meter() / Self::meter(),
        )
    }

    #[cfg(test)]
    pub fn degree() -> Self {
        Self::new_derived(
            CompactString::const_new("degree"),
            CanonicalName::new("°", AcceptsPrefix::none()),
            Number::from_f64(std::f64::consts::PI / 180.0),
            Self::radian(),
        )
    }

    #[cfg(test)]
    pub fn percent() -> Self {
        Self::new_derived(
            CompactString::const_new("percent"),
            CanonicalName::new("%", AcceptsPrefix::none()),
            Number::from_f64(1e-2),
            Self::scalar(),
        )
    }

    #[cfg(test)]
    pub fn hertz() -> Self {
        Self::new_derived(
            CompactString::const_new("hertz"),
            CanonicalName::new("Hz", AcceptsPrefix::only_short()),
            Number::from_f64(1.0),
            Unit::second().powi(-1),
        )
    }

    #[cfg(test)]
    pub fn newton() -> Self {
        Self::new_derived(
            CompactString::const_new("newton"),
            CanonicalName::new("N", AcceptsPrefix::only_short()),
            Number::from_f64(1.0),
            Unit::kilogram() * Unit::meter() / Unit::second().powi(2),
        )
    }

    #[cfg(test)]
    pub fn minute() -> Self {
        Self::new_derived(
            CompactString::const_new("minute"),
            CanonicalName::new("min", AcceptsPrefix::none()),
            Number::from_f64(60.0),
            Self::second(),
        )
    }

    #[cfg(test)]
    pub fn hour() -> Self {
        Self::new_derived(
            CompactString::const_new("hour"),
            CanonicalName::new("h", AcceptsPrefix::none()),
            Number::from_f64(60.0),
            Self::minute(),
        )
    }

    #[cfg(test)]
    pub fn kph() -> Self {
        Self::new_derived(
            CompactString::const_new("kilometer_per_hour"),
            CanonicalName::new("kph", AcceptsPrefix::none()),
            Number::from_f64(1.0),
            Self::kilometer() / Self::hour(),
        )
    }

    #[cfg(test)]
    pub fn inch() -> Self {
        Self::new_derived(
            CompactString::const_new("inch"),
            CanonicalName::new("in", AcceptsPrefix::none()),
            Number::from_f64(0.0254),
            Self::meter(),
        )
    }

    #[cfg(test)]
    pub fn gallon() -> Self {
        Self::new_derived(
            CompactString::const_new("gallon"),
            CanonicalName::new("gal", AcceptsPrefix::none()),
            Number::from_f64(231.0),
            Self::inch().powi(3),
        )
    }

    #[cfg(test)]
    pub fn foot() -> Self {
        Self::new_derived(
            CompactString::const_new("foot"),
            CanonicalName::new("ft", AcceptsPrefix::none()),
            Number::from_f64(12.0),
            Self::inch(),
        )
    }

    #[cfg(test)]
    pub fn yard() -> Self {
        Self::new_derived(
            CompactString::const_new("yard"),
            CanonicalName::new("yd", AcceptsPrefix::none()),
            Number::from_f64(3.0),
            Self::foot(),
        )
    }

    #[cfg(test)]
    pub fn mile() -> Self {
        Self::new_derived(
            CompactString::const_new("mile"),
            CanonicalName::new("mi", AcceptsPrefix::none()),
            Number::from_f64(1760.0),
            Self::yard(),
        )
    }

    #[cfg(test)]
    pub fn bit() -> Self {
        Self::new_base(
            CompactString::const_new("bit"),
            CanonicalName::new("bit", AcceptsPrefix::only_long()),
        )
    }

    #[cfg(test)]
    pub fn byte() -> Self {
        Self::new_derived(
            CompactString::const_new("byte"),
            CanonicalName::new("B", AcceptsPrefix::only_short()),
            Number::from_f64(8.0),
            Self::bit(),
        )
    }
}

impl Display for Unit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.as_string(|f| f.exponent, '·', '/', false))
    }
}

/// This function attempts to solves the equation a = C · b^alpha, where
/// C is a constant and alpha is a rational exponent. If there is a solution,
/// `Some(alpha)` is returned. If not, `None` is returned.
///
/// Examples:
/// - is_multiple_of(m², m)       = Some(2)
/// - is_multiple_of(m, m³)       = Some(1/3)
/// - is_multiple_of(m³, m²)      = Some(3/2)
/// - is_multiple_of(m²·s², m·s)  = Some(2)
///
/// - is_multiple_of(m, km)       = Some(1)
/// - is_multiple_of(m, inch)     = Some(1)
/// - is_multiple_of(m², inch)    = Some(2)
///
/// - is_multiple_of(m, s)        = None
/// - is_multiple_of(m, m·s)      = None
/// - is_multiple_of(m·s², m²·s²) = None
///
pub fn is_multiple_of(a: &Unit, b: &Unit) -> Option<Exponent> {
    let a_base = a.to_base_unit_representation().0;
    let b_base = b.to_base_unit_representation().0;

    if (a_base.clone().div(b_base.clone()))
        .canonicalized()
        .is_scalar()
    {
        return Some(Exponent::from_integer(1));
    }

    if a_base.is_scalar() {
        return None;
    }

    let a_first = a_base
        .iter()
        .next()
        .expect("At least one factor in non-scalar unit");

    if let Some(b_corresponding_factor) = b_base.iter().find(|fb| fb.unit_id == a_first.unit_id) {
        let alpha = a_first.exponent / b_corresponding_factor.exponent;

        // Make sure that this is also correct for all other factors:
        if a_base.div(b_base.power(alpha)).is_scalar() {
            Some(alpha)
        } else {
            None
        }
    } else {
        None
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
                    canonical_name: CanonicalName::new("m", AcceptsPrefix::only_short()),
                    kind: UnitKind::Base,
                },
                exponent: Rational::from_integer(1),
            },
            UnitFactor {
                prefix: Prefix::none(),
                unit_id: UnitIdentifier {
                    name: "second".into(),
                    canonical_name: CanonicalName::new("s", AcceptsPrefix::only_short()),
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
                    canonical_name: CanonicalName::new("m", AcceptsPrefix::only_short()),
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

    #[test]
    fn is_multiple_of_basic() {
        assert_eq!(
            is_multiple_of(&Unit::scalar(), &Unit::scalar()),
            Some(Exponent::from_integer(1))
        );

        assert_eq!(is_multiple_of(&Unit::scalar(), &Unit::meter()), None);
        assert_eq!(is_multiple_of(&Unit::meter(), &Unit::scalar()), None);

        assert_eq!(
            is_multiple_of(&Unit::meter(), &Unit::meter()),
            Some(Exponent::new(1, 1))
        );
        assert_eq!(
            is_multiple_of(&Unit::meter(), &Unit::meter().powi(3)),
            Some(Exponent::new(1, 3))
        );
        assert_eq!(
            is_multiple_of(&Unit::meter().powi(2), &Unit::meter()),
            Some(Exponent::new(2, 1))
        );
        assert_eq!(
            is_multiple_of(&Unit::meter().powi(2), &Unit::meter().powi(3)),
            Some(Exponent::new(2, 3))
        );

        assert_eq!(
            is_multiple_of(&(Unit::meter() * Unit::second()), &Unit::meter()),
            None
        );
        assert_eq!(
            is_multiple_of(&Unit::meter(), &(Unit::meter() * Unit::second())),
            None
        );

        assert_eq!(
            is_multiple_of(
                &(Unit::meter() * Unit::second()),
                &(Unit::meter() * Unit::second())
            ),
            Some(Exponent::new(1, 1))
        );
        assert_eq!(
            is_multiple_of(
                &(Unit::meter() * Unit::second()),
                &(Unit::meter() * Unit::second()).powi(2)
            ),
            Some(Exponent::new(1, 2))
        );
        assert_eq!(
            is_multiple_of(
                &(Unit::meter() * Unit::second()).powi(3),
                &(Unit::meter() * Unit::second()).powi(2)
            ),
            Some(Exponent::new(3, 2))
        );
        assert_eq!(
            is_multiple_of(
                &(Unit::meter() * Unit::second()),
                &(Unit::meter().powi(2) * Unit::second().powi(4))
            ),
            None
        );
        assert_eq!(
            is_multiple_of(
                &(Unit::meter() * Unit::second().powi(2)),
                &(Unit::meter().powi(2) * Unit::second().powi(4))
            ),
            Some(Exponent::new(1, 2))
        );
    }

    #[test]
    fn is_multiple_of_with_factor() {
        assert_eq!(
            is_multiple_of(&Unit::scalar(), &Unit::radian()),
            Some(Exponent::new(1, 1))
        );
        assert_eq!(
            is_multiple_of(&Unit::radian(), &Unit::scalar()),
            Some(Exponent::new(1, 1))
        );

        assert_eq!(
            is_multiple_of(&Unit::meter(), &Unit::inch()),
            Some(Exponent::new(1, 1))
        );
        assert_eq!(
            is_multiple_of(&Unit::meter(), &Unit::inch().powi(2)),
            Some(Exponent::new(1, 2))
        );

        assert_eq!(
            is_multiple_of(&Unit::meter(), &Unit::kilometer()),
            Some(Exponent::new(1, 1))
        );
        assert_eq!(
            is_multiple_of(&Unit::kilometer(), &Unit::meter()),
            Some(Exponent::new(1, 1))
        );
    }
}
