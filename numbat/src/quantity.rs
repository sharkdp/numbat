use crate::arithmetic::{Exponent, Power, Rational};
use crate::number::Number;
use crate::pretty_print::FormatOptions;
use crate::unit::{Unit, UnitFactor, is_multiple_of};
use crate::unit_registry::UnitRegistry;

use compact_str::{CompactString, ToCompactString, format_compact};
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
    /// If set, the quantity is displayed as `(value/target) × target`
    /// instead of `value unit`. This is used for conversions like `6 hours -> 45 min`
    /// which should display as `8 × 45 min` instead of `360 min`.
    conversion_target: Option<Box<Quantity>>,
}

impl Quantity {
    pub fn new(value: Number, unit: Unit) -> Self {
        Quantity {
            value,
            unit,
            can_simplify: true,
            conversion_target: None,
        }
    }

    pub fn new_f64(value: f64, unit: Unit) -> Self {
        Quantity {
            value: Number::from_f64(value),
            unit,
            can_simplify: true,
            conversion_target: None,
        }
    }

    pub fn no_simplify(mut self) -> Self {
        self.can_simplify = false;
        self
    }

    pub fn with_conversion_target(mut self, target: Quantity) -> Self {
        // Only set the target if its value is not 1
        if target.value != Number::from_f64(1.0) {
            self.conversion_target = Some(Box::new(target));
        }
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

    pub fn can_simplify(&self) -> bool {
        self.can_simplify
    }

    pub fn is_zero(&self) -> bool {
        self.value == Number::from_f64(0.0)
    }

    pub fn abs(self) -> Self {
        Quantity::new(self.value.abs(), self.unit)
    }

    pub fn to_base_unit_representation(&self) -> Quantity {
        let (unit, factor) = self.unit.to_base_unit_representation();
        Quantity::new(self.value * factor, unit)
    }

    pub fn convert_to(&self, target_unit: &Unit) -> Result<Quantity> {
        if &self.unit == target_unit || self.is_zero() {
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

                if let Some(alpha) = is_multiple_of(&unit, &factor_unit)
                    && alpha.is_integer()
                {
                    let simplified_unit = factor_unit.power(alpha);
                    if let Ok(q) = self.convert_to(&simplified_unit) {
                        return q;
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
            .chunk_by(|f| f.unit_id.sort_key())
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

    /// Like `full_simplify`, but also tries to simplify to registered derived units.
    /// For example, J/s -> W, Pa·m² -> N, etc.
    ///
    /// The `get_unit` closure should return the Unit for a given unit name,
    /// typically by looking it up in the VM's constants.
    pub fn full_simplify_with_registry<F>(&self, registry: &UnitRegistry, get_unit: F) -> Self
    where
        F: Fn(&str) -> Option<Unit>,
    {
        // First apply the standard simplification
        let simplified = self.full_simplify();

        if !simplified.can_simplify {
            return simplified;
        }

        let current_complexity = simplified.unit.iter().count();

        // Only try registry lookup if we have more than one factor
        if current_complexity <= 1 {
            return simplified;
        }

        let (base_unit_repr, source_factor) = simplified.unit.to_base_unit_representation();

        // Check if the base representation is a single base unit with an integer exponent,
        // and the conversion factor is 1 (i.e., the unit is definitionally equivalent).
        // For example, N/Pa has base repr m² with factor 1, so we simplify to m².
        // But Wh/W has base repr s with factor 3600, so we don't simplify to s.
        if base_unit_repr.iter().count() == 1
            && let Some(factor) = base_unit_repr.iter().next()
            && factor.exponent.is_integer()
            && (source_factor.to_f64() - 1.0).abs() < 1e-9
            && let Ok(converted) = simplified.convert_to(&base_unit_repr)
        {
            return converted;
        }

        // Look up matching unit names from the registry
        let matching_names = registry.get_matching_unit_names(&simplified.unit);

        // Try each matching unit and pick the simplest one we can convert to
        let mut best: Option<Quantity> = None;

        for name in matching_names {
            if let Some(target_unit) = get_unit(&name) {
                let target_complexity = target_unit.iter().count();

                // Only consider if it's simpler
                if target_complexity < current_complexity {
                    // Check if the source unit and target unit have the same
                    // conversion factor to base units. This ensures we only
                    // simplify when the units are definitionally equivalent
                    // (e.g., J/s = W) and not just dimensionally equivalent
                    // (e.g., Wh/W ≠ century, even though both are time).
                    let (_, target_factor) = target_unit.to_base_unit_representation();

                    // Only simplify if conversion factors are equal (within tolerance)
                    let factors_match = (source_factor.to_f64() - target_factor.to_f64()).abs()
                        < 1e-9 * source_factor.to_f64().abs().max(1.0);

                    if factors_match && let Ok(converted) = simplified.convert_to(&target_unit) {
                        // Can't get simpler than 1 factor, return immediately
                        if target_complexity == 1 {
                            return converted;
                        }

                        let dominated = match &best {
                            None => true,
                            Some(best_q) => target_complexity < best_q.unit.iter().count(),
                        };
                        if dominated {
                            best = Some(converted);
                        }
                    }
                }
            }
        }

        best.unwrap_or(simplified)
    }

    pub fn as_scalar(&self) -> Result<Number> {
        Ok(self.convert_to(&Unit::scalar())?.value)
    }

    pub fn unsafe_value(&self) -> &Number {
        &self.value
    }

    pub fn checked_power(self, exp: Quantity) -> Result<Option<Self>> {
        let exponent = exp.as_scalar()?;
        // Unit dimension calculation requires a real exponent
        let exponent_for_unit = exponent.try_as_real()
            .ok_or(QuantityError::NonRationalExponent)?;
        if exponent_for_unit < 0.0 && self.is_zero() {
            Ok(None)
        } else {
            Ok(Some(Quantity::new(
                self.value.pow(&exponent),
                self.unit.power(
                    Rational::from_f64(exponent_for_unit)
                        .ok_or(QuantityError::NonRationalExponent)?,
                ),
            )))
        }
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
        Quantity::new(*n, Unit::scalar())
    }
}

impl std::ops::Add for &Quantity {
    type Output = Result<Quantity>;

    fn add(self, rhs: Self) -> Self::Output {
        if self.is_zero() {
            Ok(rhs.clone())
        } else if rhs.is_zero() {
            Ok(self.clone())
        } else if self.unit == rhs.unit {
            Ok(Quantity::new(self.value + rhs.value, self.unit.clone()))
        } else {
            // Use the smaller unit to ensure commutativity: a + b == b + a
            let result_unit = self.unit.smaller_unit(&rhs.unit);
            Ok(Quantity::new(
                self.convert_to(result_unit)?.value + rhs.convert_to(result_unit)?.value,
                result_unit.clone(),
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
        } else if self.unit == rhs.unit {
            Ok(Quantity::new(self.value - rhs.value, self.unit.clone()))
        } else {
            // Use the smaller unit to ensure anticommutativity: a - b == -(b - a)
            let result_unit = self.unit.smaller_unit(&rhs.unit);
            Ok(Quantity::new(
                self.convert_to(result_unit)?.value - rhs.convert_to(result_unit)?.value,
                result_unit.clone(),
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

impl PartialOrd for Quantity {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let other_converted = other.convert_to(self.unit()).ok()?;
        // Complex numbers are not ordered
        if !self.value.is_real() || !other_converted.value.is_real() {
            return None;
        }
        self.value.re.partial_cmp(&other_converted.value.re)
    }
}

impl Quantity {
    pub fn pretty_print(&self) -> crate::markup::Markup {
        self.pretty_print_with(&FormatOptions::default())
    }

    pub fn pretty_print_with(&self, format_options: &FormatOptions) -> crate::markup::Markup {
        self.pretty_print_internal(format_options, None)
    }
}

pub(crate) enum QuantityOrdering {
    IncompatibleUnits,
    NanOperand,
    ComplexOperand,
    Ok(std::cmp::Ordering),
}

impl Quantity {
    /// partial_cmp that encodes whether comparison fails because its arguments have
    /// incompatible units, because one of them is NaN, or because they are complex
    pub(crate) fn partial_cmp_preserve_nan(&self, other: &Self) -> QuantityOrdering {
        if !self.value.is_real() || !other.value.is_real() {
            return QuantityOrdering::ComplexOperand;
        }

        if self.value.re.is_nan() || other.value.re.is_nan() {
            return QuantityOrdering::NanOperand;
        }

        let Ok(other_converted) = other.convert_to(self.unit()) else {
            return QuantityOrdering::IncompatibleUnits;
        };

        let cmp = self
            .value
            .re
            .partial_cmp(&other_converted.value.re)
            .expect("unexpectedly got a None partial_cmp from non-NaN arguments");

        QuantityOrdering::Ok(cmp)
    }

    /// Pretty prints with the given format options and optional dtoa config override.
    fn pretty_print_internal(
        &self,
        format_options: &FormatOptions,
        dtoa_config: Option<FmtFloatConfig>,
    ) -> crate::markup::Markup {
        use crate::markup;

        let unit_str = format_compact!("{}", self.unit());

        // If there's a conversion target, display as `coefficient × target`
        // e.g., `6 hours -> 45 min` displays as `8 × 45 min`
        if let Some(ref target) = self.conversion_target {
            let coefficient = self.value / target.value;
            let formatted_coefficient =
                coefficient.pretty_print_with_dtoa_config(format_options, dtoa_config);
            let formatted_target = target.pretty_print_internal(format_options, dtoa_config);

            return markup::value(formatted_coefficient)
                + markup::space()
                + markup::operator("×")
                + markup::space()
                + formatted_target;
        }

        let formatted_number = self
            .unsafe_value()
            .pretty_print_with_dtoa_config(format_options, dtoa_config);

        // Wrap complex numbers in parens when there's a unit
        let is_complex = !self.value.is_real();
        let has_unit = !unit_str.is_empty() && unit_str != "°" && unit_str != "′" && unit_str != "″";

        if is_complex && has_unit {
            markup::operator("(")
                + markup::value(formatted_number)
                + markup::operator(")")
                + markup::space()
                + markup::unit(unit_str)
        } else {
            markup::value(formatted_number)
                + if unit_str == "°" || unit_str == "′" || unit_str == "″" || unit_str.is_empty() {
                    markup::empty()
                } else {
                    markup::space()
                }
                + markup::unit(unit_str)
        }
    }

    /// Pretty prints with the given precision. Disables e (scientific) notation.
    /// Prints without fractional part if precision is 0.
    pub fn pretty_print_with_precision(&self, precision: i8) -> crate::markup::Markup {
        let dtoa_config = FmtFloatConfig::default()
            .min_decimal_digits(precision)
            .max_decimal_digits(precision)
            .add_point_zero(false)
            .force_no_e_notation()
            .round();
        self.pretty_print_internal(&FormatOptions::default(), Some(dtoa_config))
    }

    pub fn unsafe_value_as_string(&self) -> CompactString {
        self.unsafe_value().to_compact_string()
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
    use compact_str::CompactString;

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
            CompactString::const_new("foot"),
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
                .checked_power(Quantity::from_scalar(3.0))
                .expect("exponent is scalar")
                .expect("no zero to negative power");

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
