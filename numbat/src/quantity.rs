use crate::arithmetic::{Exponent, Power, Rational};
use crate::number::Number;
use crate::pretty_print::PrettyPrint;
use crate::unit::{is_multiple_of, Unit, UnitFactor};

use itertools::Itertools;
use num_rational::Ratio;
use num_traits::{FromPrimitive, Zero};
use pretty_dtoa::FmtFloatConfig;
use thiserror::Error;

#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum QuantityError {
    #[error("Conversion error: unit '{0}' can not be converted to '{1}'")]
    IncompatibleUnits(Unit, Unit), // TODO: this can currently be triggered if there are multiple base units for the same dimension (no way to convert between them)

    #[error("Non-rational exponent")]
    NonRationalExponent,
}

pub type Result<T> = std::result::Result<T, QuantityError>;

#[derive(Debug, Clone)]
pub struct Quantity {
    value: Number,
    unit: Unit,
    can_simplify: bool,
}

impl Quantity {
    pub fn new(value: Number, unit: Unit) -> Self {
        Quantity {
            value,
            unit,
            can_simplify: true,
        }
    }

    pub fn new_f64(value: f64, unit: Unit) -> Self {
        Quantity {
            value: Number::from_f64(value),
            unit,
            can_simplify: true,
        }
    }

    pub fn no_simplify(mut self) -> Self {
        self.can_simplify = false;
        self
    }

    pub fn from_scalar(value: f64) -> Quantity {
        Quantity::new_f64(value, Unit::scalar())
    }

    pub fn from_unit(unit: Unit) -> Quantity {
        Quantity::new_f64(1.0, unit)
    }

    pub fn unit(&self) -> &Unit {
        &self.unit
    }

    pub fn is_zero(&self) -> bool {
        self.value.to_f64() == 0.0
    }

    pub fn abs(self) -> Self {
        Quantity::new(self.value.abs(), self.unit)
    }

    pub fn to_base_unit_representation(&self) -> Quantity {
        let (unit, factor) = self.unit.to_base_unit_representation();
        Quantity::new(self.value * factor, unit)
    }

    pub fn convert_to(&self, target_unit: &Unit) -> Result<Quantity> {
        if &self.unit == target_unit || self.unsafe_value().to_f64().is_zero() {
            Ok(Quantity::new(self.value, target_unit.clone()))
        } else {
            // Remove common unit factors to reduce unnecessary conversion procedures
            // For example: when converting from km/hour to mile/hour, there is no need
            // to also perform the hour->second conversion, which would be needed, as
            // we go back to base units for now. Removing common factors is just one
            // heuristic, but it would be better to solve this in a more general way.
            // For more details on this problem, see [1].
            //
            // [1] https://github.com/sharkdp/numbat/issues/118.
            let mut common_unit_factors = Unit::scalar();
            let target_unit_canonicalized = target_unit.canonicalized();

            for factor in self.unit.canonicalized().iter() {
                if let Some(other_factor) = target_unit_canonicalized
                    .iter()
                    .find(|&f| factor.prefix == f.prefix && factor.unit_id == f.unit_id)
                {
                    if factor.exponent > Ratio::zero() && other_factor.exponent > Ratio::zero() {
                        common_unit_factors = common_unit_factors
                            * Unit::from_factor(UnitFactor {
                                exponent: std::cmp::min(factor.exponent, other_factor.exponent),
                                ..factor.clone()
                            });
                    } else if factor.exponent < Ratio::zero()
                        && other_factor.exponent < Ratio::zero()
                    {
                        common_unit_factors = common_unit_factors
                            * Unit::from_factor(UnitFactor {
                                exponent: std::cmp::max(factor.exponent, other_factor.exponent),
                                ..factor.clone()
                            });
                    }
                }
            }

            let target_unit_reduced =
                (target_unit.clone() / common_unit_factors.clone()).canonicalized();
            let own_unit_reduced =
                (self.unit.clone() / common_unit_factors.clone()).canonicalized();

            let (target_base_unit_representation, factor) =
                target_unit_reduced.to_base_unit_representation();

            let quantity_base_unit_representation = (self.clone()
                / Quantity::from_unit(common_unit_factors))
            .to_base_unit_representation();
            let own_base_unit_representation = own_unit_reduced.to_base_unit_representation().0;

            if own_base_unit_representation == target_base_unit_representation {
                Ok(Quantity::new(
                    *quantity_base_unit_representation.unsafe_value() / factor,
                    target_unit.clone(),
                ))
            } else {
                // TODO: can this even be triggered? replace by an assertion?
                Err(QuantityError::IncompatibleUnits(
                    self.unit.clone(),
                    target_unit.clone(),
                ))
            }
        }
    }

    pub fn full_simplify(&self) -> Self {
        if !self.can_simplify {
            return self.clone();
        }

        // Heuristic 1
        if let Ok(scalar_result) = self.convert_to(&Unit::scalar()) {
            return scalar_result;
        }

        // Heuristic 2
        let unit = self.unit.canonicalized();
        if unit.iter().count() > 1 {
            for factor in unit.iter() {
                let mut factor = factor.clone();
                factor.exponent = Exponent::from_integer(1);
                let factor_unit = Unit::from_factor(factor);

                if let Some(alpha) = is_multiple_of(&unit, &factor_unit) {
                    if alpha.is_integer() {
                        let simplified_unit = factor_unit.power(alpha);
                        if let Ok(q) = self.convert_to(&simplified_unit) {
                            return q;
                        }
                    }
                }
            }
        }

        // Heuristic 3
        let removed_exponent = |u: &UnitFactor| {
            let base_unit = u.unit_id.base_unit_and_factor().0;
            if let Some(first_factor) = base_unit.into_iter().next() {
                first_factor.exponent
            } else {
                Ratio::from_integer(1)
            }
        };

        let mut factor = Number::from_f64(1.0);
        let mut simplified_unit = Unit::scalar();

        for (_, group) in &self
            .unit
            .canonicalized()
            .iter()
            .group_by(|f| f.unit_id.sort_key())
        {
            let group_as_unit = Unit::from_factors(group.cloned());
            let group_representative = group_as_unit
                .iter()
                .max_by(|&f1, &f2| {
                    // prefer base units over non-base. if multiple base units, prefer
                    // those with a larger exponent
                    (f1.unit_id.is_base().cmp(&f2.unit_id.is_base()))
                        .then(f1.exponent.cmp(&f2.exponent))
                })
                .expect("At least one unit factor in the group");

            let target_unit = if group_as_unit.to_base_unit_representation().0.is_scalar() {
                // If the group-representative is convertible to a scalar,
                // use 'scalar' as the target unit instead. This allows us
                // to simplify, for example, '3% · kg' to '0.03 kg'.
                Unit::scalar()
            } else {
                let exponent = group_as_unit
                    .iter()
                    .map(|f| {
                        f.exponent * removed_exponent(f) / removed_exponent(group_representative)
                    })
                    .sum();
                Unit::from_factor(UnitFactor {
                    exponent,
                    ..group_representative.clone()
                })
            };

            let converted = Quantity::from_unit(group_as_unit)
                .convert_to(&target_unit)
                .unwrap();

            simplified_unit = simplified_unit * target_unit;
            factor = factor * converted.value;
        }

        simplified_unit.canonicalize();

        Quantity::new(self.value * factor, simplified_unit)
    }

    pub fn as_scalar(&self) -> Result<Number> {
        Ok(self.convert_to(&Unit::scalar())?.value)
    }

    pub fn unsafe_value(&self) -> &Number {
        &self.value
    }

    pub fn power(self, exp: Quantity) -> Result<Self> {
        let exponent_as_scalar = exp.as_scalar()?.to_f64();
        Ok(Quantity::new_f64(
            self.value.to_f64().powf(exponent_as_scalar),
            self.unit.power(
                Rational::from_f64(exponent_as_scalar).ok_or(QuantityError::NonRationalExponent)?,
            ),
        ))
    }

    pub fn checked_div(self, other: Self) -> Option<Self> {
        if other.is_zero() {
            None
        } else {
            Some(self / other)
        }
    }
}

impl From<&Number> for Quantity {
    fn from(n: &Number) -> Self {
        Quantity::from_scalar(n.to_f64())
    }
}

impl std::ops::Add for &Quantity {
    type Output = Result<Quantity>;

    fn add(self, rhs: Self) -> Self::Output {
        if self.is_zero() {
            Ok(rhs.clone())
        } else if rhs.is_zero() {
            Ok(self.clone())
        } else {
            Ok(Quantity::new(
                self.value + rhs.convert_to(&self.unit)?.value,
                self.unit.clone(),
            ))
        }
    }
}

impl std::ops::Sub for &Quantity {
    type Output = Result<Quantity>;

    fn sub(self, rhs: Self) -> Self::Output {
        if self.is_zero() {
            Ok(-rhs.clone())
        } else if rhs.is_zero() {
            Ok(self.clone())
        } else {
            Ok(Quantity::new(
                self.value - rhs.convert_to(&self.unit)?.value,
                self.unit.clone(),
            ))
        }
    }
}

impl std::ops::Mul for Quantity {
    type Output = Quantity;

    fn mul(self, rhs: Self) -> Self::Output {
        Quantity::new(self.value * rhs.value, self.unit * rhs.unit)
    }
}

impl std::ops::Div for Quantity {
    type Output = Quantity;

    fn div(self, rhs: Self) -> Self::Output {
        Quantity::new(self.value / rhs.value, self.unit / rhs.unit)
    }
}

impl std::ops::Neg for Quantity {
    type Output = Quantity;

    fn neg(self) -> Self::Output {
        Quantity::new(-self.value, self.unit)
    }
}

impl PartialEq for Quantity {
    fn eq(&self, other: &Self) -> bool {
        if let Ok(other_converted) = other.convert_to(self.unit()) {
            self.value == other_converted.value
        } else {
            false
        }
    }
}

impl Eq for Quantity {}

impl PartialOrd for Quantity {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let other_converted = other.convert_to(self.unit()).ok()?;
        self.value.partial_cmp(&other_converted.value)
    }
}

impl PrettyPrint for Quantity {
    fn pretty_print(&self) -> crate::markup::Markup {
        self.pretty_print_with_options(None)
    }
}

impl Quantity {
    /// Pretty prints with the given options.
    /// If options is None, default options will be used.
    fn pretty_print_with_options(&self, options: Option<FmtFloatConfig>) -> crate::markup::Markup {
        use crate::markup;

        let formatted_number = self.unsafe_value().pretty_print_with_options(options);

        let unit_str = format!("{}", self.unit());

        markup::value(formatted_number)
            + if unit_str == "°" || unit_str == "′" || unit_str == "″" || unit_str.is_empty() {
                markup::empty()
            } else {
                markup::space()
            }
            + markup::unit(unit_str)
    }

    /// Pretty prints with the given precision. Disables e (scientific) notation.
    /// Prints without fractional part if precision is 0.
    pub fn pretty_print_with_precision(&self, precision: i8) -> crate::markup::Markup {
        let options = FmtFloatConfig::default()
            .min_decimal_digits(precision)
            .max_decimal_digits(precision)
            .add_point_zero(false)
            .force_no_e_notation()
            .round();
        self.pretty_print_with_options(Some(options))
    }

    pub fn unsafe_value_as_string(&self) -> String {
        self.unsafe_value().to_string()
    }
}

impl std::fmt::Display for Quantity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use crate::markup::{Formatter, PlainTextFormatter};

        let markup = self.pretty_print();
        let formatter = PlainTextFormatter {};
        write!(f, "{}", formatter.format(&markup, false).trim())
    }
}

#[cfg(test)]
mod tests {
    use crate::{prefix::Prefix, prefix_parser::AcceptsPrefix, unit::CanonicalName};

    use super::*;

    #[test]
    fn conversion_trivial() {
        let meter = Unit::meter();
        let second = Unit::second();

        let length = Quantity::new_f64(2.0, meter.clone());

        assert!(length.convert_to(&meter).is_ok());

        assert!(length.convert_to(&second).is_err());
        assert!(length.convert_to(&Unit::scalar()).is_err());
    }

    #[test]
    fn conversion_basic() {
        use approx::assert_relative_eq;

        let meter = Unit::meter();
        let foot = Unit::new_derived(
            "foot",
            CanonicalName::new("ft", AcceptsPrefix::none()),
            Number::from_f64(0.3048),
            meter.clone(),
        );

        let length = Quantity::new_f64(2.0, meter.clone());

        let length_in_foot = length.convert_to(&foot).expect("conversion succeeds");
        assert_eq!(length_in_foot.unsafe_value().to_f64(), 2.0 / 0.3048);

        let length_converted_back_to_meter = length_in_foot
            .convert_to(&meter)
            .expect("conversion succeeds");
        assert_relative_eq!(
            length_converted_back_to_meter.unsafe_value().to_f64(),
            2.0,
            epsilon = 1e-6
        );
    }

    #[test]
    fn prefixes() {
        use crate::prefix::Prefix;

        use approx::assert_relative_eq;

        let meter = Unit::meter();
        let centimeter = Unit::meter().with_prefix(Prefix::centi());

        let length = Quantity::new_f64(2.5, meter.clone());
        {
            let length_in_centimeter = length.convert_to(&centimeter).expect("conversion succeeds");
            assert_relative_eq!(
                length_in_centimeter.unsafe_value().to_f64(),
                250.0,
                epsilon = 1e-6
            );

            let length_converted_back_to_meter = length_in_centimeter
                .convert_to(&meter)
                .expect("conversion succeeds");
            assert_relative_eq!(
                length_converted_back_to_meter.unsafe_value().to_f64(),
                2.5,
                epsilon = 1e-6
            );
        }
        {
            let volume = length
                .power(Quantity::from_scalar(3.0))
                .expect("exponent is scalar");

            let volume_in_centimeter3 = volume
                .convert_to(&centimeter.powi(3))
                .expect("conversion succeeds");
            assert_relative_eq!(
                volume_in_centimeter3.unsafe_value().to_f64(),
                15_625_000.0,
                epsilon = 1e-6
            );
        }
    }

    #[test]
    fn abs() {
        assert_eq!(
            Quantity::new_f64(0.0, Unit::scalar()).abs(),
            Quantity::new_f64(0.0, Unit::scalar())
        );

        assert_eq!(
            Quantity::new_f64(1.0, Unit::scalar()).abs(),
            Quantity::new_f64(1.0, Unit::scalar())
        );

        assert_eq!(
            Quantity::new_f64(-1.0, Unit::scalar()).abs(),
            Quantity::new_f64(1.0, Unit::scalar())
        );
    }

    #[test]
    fn full_simplify_basic() {
        let q = Quantity::new_f64(2.0, Unit::meter() / Unit::second());
        assert_eq!(q.full_simplify(), q);
    }

    #[test]
    fn full_simplify_convertible_to_scalar() {
        {
            let q = Quantity::new_f64(2.0, Unit::meter() / Unit::millimeter());
            assert_eq!(q.full_simplify(), Quantity::from_scalar(2000.0));
        }
        {
            let q = Quantity::new_f64(2.0, Unit::kilometer() / Unit::millimeter());
            assert_eq!(q.full_simplify(), Quantity::from_scalar(2000000.0));
        }
        {
            let q = Quantity::new_f64(2.0, Unit::meter() / Unit::centimeter() * Unit::second());
            assert_eq!(
                q.full_simplify(),
                Quantity::new_f64(2.0 * 100.0, Unit::second())
            );
        }
        {
            let q = Quantity::new_f64(1.0, Unit::kph() / (Unit::kilometer() / Unit::hour()));
            assert_eq!(q.full_simplify(), Quantity::from_scalar(1.0));
        }
    }

    #[test]
    fn full_simplify_unit_rearrangements() {
        {
            let q = Quantity::new_f64(2.0, Unit::meter() * Unit::second() * Unit::meter());
            let expected = Quantity::new_f64(2.0, Unit::meter().powi(2) * Unit::second());
            assert_eq!(q.full_simplify(), expected);
        }
        {
            let q = Quantity::new_f64(2.0, Unit::kilometer() / Unit::millimeter());
            assert_eq!(q.full_simplify(), Quantity::from_scalar(2000000.0));
        }
        {
            let q = Quantity::new_f64(1.0, Unit::meter() * Unit::gram() / Unit::centimeter());
            assert_eq!(q.full_simplify(), Quantity::new_f64(100.0, Unit::gram()));
        }
    }

    #[test]
    fn full_simplify_scalarlike_units() {
        {
            let q = Quantity::new_f64(3.0, Unit::percent() * Unit::kilogram());
            assert_eq!(q.full_simplify(), Quantity::new_f64(0.03, Unit::kilogram()));
        }
    }

    #[test]
    fn full_simplify_complex() {
        {
            let q = Quantity::new_f64(5.0, Unit::second() * Unit::millimeter() / Unit::meter());
            let expected = Quantity::new_f64(0.005, Unit::second());
            assert_eq!(q.full_simplify(), expected);
        }
        {
            let q = Quantity::new_f64(
                5.0,
                Unit::bit().with_prefix(Prefix::mega()) / Unit::second() * Unit::hour(),
            );
            let expected = Quantity::new_f64(18000.0, Unit::bit().with_prefix(Prefix::mega()));
            assert_eq!(q.full_simplify(), expected);
        }
        {
            let q = Quantity::new_f64(5.0, Unit::centimeter() * Unit::meter());
            let expected = Quantity::new_f64(500.0, Unit::centimeter().powi(2));
            assert_eq!(q.full_simplify(), expected);
        }
        {
            let q = Quantity::new_f64(5.0, Unit::meter() * Unit::centimeter());
            let expected = Quantity::new_f64(500.0, Unit::centimeter().powi(2));
            assert_eq!(q.full_simplify(), expected);
        }
        {
            let q = Quantity::new_f64(1.0, Unit::hertz() / Unit::second());
            let expected = Quantity::new_f64(1.0, Unit::second().powi(-2));
            assert_eq!(q.full_simplify(), expected);
        }
        {
            let q = Quantity::new_f64(1.0, Unit::gallon() / Unit::inch());
            let expected = Quantity::new_f64(231.0, Unit::inch().powi(2));
            assert_eq!(q.full_simplify(), expected);
        }
        {
            let q = Quantity::new_f64(1.0, Unit::gallon() / Unit::inch().powi(2));
            let expected = Quantity::new_f64(231.0, Unit::inch());
            assert_eq!(q.full_simplify(), expected);
        }
    }

    #[test]
    fn si_compliant_pretty_printing() {
        //  See: https://en.wikipedia.org/wiki/International_System_of_Units
        //        -> Unit symbols and the value of quantities

        // The value of a quantity is written as a number followed by a space
        // (representing a multiplication sign) and a unit symbol; e.g., 2.21 kg,
        // 7.3×10² m², 22 K.
        assert_eq!(
            Quantity::new_f64(2.21, Unit::kilogram()).to_string(),
            "2.21 kg"
        );
        assert_eq!(Quantity::new_f64(22.0, Unit::kelvin()).to_string(), "22 K");

        // Exceptions are the symbols for plane angular degrees, minutes, and
        // seconds (°, ′, and ″), which are placed immediately after the
        // number with no intervening space.
        assert_eq!(Quantity::new_f64(90.0, Unit::degree()).to_string(), "90°");

        // A prefix is part of the unit, and its symbol is prepended to the
        // unit symbol without a separator (e.g., k in km, M in MPa, G in GHz).
        // Compound prefixes are not allowed.
        assert_eq!(
            Quantity::new_f64(1.0, Unit::hertz().with_prefix(Prefix::giga())).to_string(),
            "1 GHz"
        );

        // Symbols for derived units formed by multiplication are joined with a
        // centre dot (·) or a non-breaking space; e.g., N·m or N m.
        assert_eq!(
            Quantity::new_f64(1.0, Unit::newton() * Unit::meter()).to_string(),
            "1 N·m"
        );

        // Symbols for derived units formed by division are joined with a solidus
        // (/), or given as a negative exponent. E.g., the "metre per second" can
        // be written m/s, m s^(−1), m·s^(−1), or m/s. Only one solidus should
        // be used; e.g., kg/(m·s²) and kg·m^(−1)·s^(−2) are acceptable, but
        // kg/m/s² is ambiguous and unacceptable.
        assert_eq!(
            Quantity::new_f64(1.0, Unit::meter() / Unit::meter()).to_string(),
            "1 m/m"
        );
        assert_eq!(
            Quantity::new_f64(
                1.0,
                Unit::kilogram() / (Unit::meter() * Unit::second().powi(2))
            )
            .to_string(),
            "1 kg/(m·s²)"
        );
    }
}
