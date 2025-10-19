use crate::{quantity::Quantity, span::Span, value::Value};
use compact_str::{CompactString, format_compact};
use std::fmt::Display;
use thiserror::Error;

#[derive(Debug, Clone, Error, PartialEq)]
pub struct AssertEq2Error {
    pub span_lhs: Span,
    pub lhs: Value,
    pub span_rhs: Span,
    pub rhs: Value,
}

impl Display for AssertEq2Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let optional_message = if format!("{}", self.lhs) == format!("{}", self.rhs) {
            "\nNote: The two printed values appear to be the same, this may be due to floating point precision errors.\n      \
            For dimension types you may want to test approximate equality instead: assert_eq(q1, q2, ε)."
        } else {
            ""
        };

        write!(
            f,
            "Assertion failed because the following two values are not the same:\n  {}\n  {}{}",
            self.lhs, self.rhs, optional_message
        )
    }
}

#[derive(Debug, Clone, Error, PartialEq)]
pub struct AssertEq3Error {
    pub span_lhs: Span,
    pub lhs_original: Quantity,
    pub lhs_converted: Quantity,
    pub span_rhs: Span,
    pub rhs_original: Quantity,
    pub rhs_converted: Quantity,
    pub eps: Quantity,
    pub diff_abs: Quantity,
}

impl AssertEq3Error {
    /// Returns the precision of epsilon clamped to i8 for compatibility with pretty_dtoa.
    fn eps_precision(&self) -> i8 {
        let (_, eps_precision) = get_float_part_lengths(&self.eps.unsafe_value_as_string());
        // i8 should fit inside usize so this should be safe
        let eps_precision = eps_precision.min(i8::MAX.try_into().unwrap());
        let eps_precision: i8 = eps_precision.try_into().unwrap();
        eps_precision
    }

    /// Returns the input quantity pretty printed with precision matching epsilon.
    pub fn fmt_quantity(&self, quantity: &Quantity) -> String {
        quantity
            .pretty_print_with_precision(self.eps_precision())
            .to_string()
    }

    /// Returns the comparand quantities formatted as strings for pretty error message display.
    pub fn fmt_comparands(&self) -> (CompactString, CompactString) {
        let (lhs_converted_len, _) =
            get_float_part_lengths(&self.lhs_converted.unsafe_value_as_string());
        let (rhs_converted_len, _) =
            get_float_part_lengths(&self.rhs_converted.unsafe_value_as_string());

        let width = core::cmp::Ord::max(lhs_converted_len, rhs_converted_len);

        let lhs = self.fmt_comparand(&self.lhs_converted, &self.lhs_original, width);
        let rhs = self.fmt_comparand(&self.rhs_converted, &self.rhs_original, width);

        (lhs, rhs)
    }

    fn fmt_comparand(
        &self,
        converted: &Quantity,
        original: &Quantity,
        width: usize,
    ) -> CompactString {
        let pretty_converted_str = left_pad_integer_part(
            converted
                .pretty_print_with_precision(self.eps_precision())
                .to_string()
                .as_str(),
            width,
        );

        if converted.unit() == original.unit() {
            pretty_converted_str
        } else {
            format_compact!("{} ({})", pretty_converted_str, original)
        }
    }
}

impl Display for AssertEq3Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (lhs, rhs) = self.fmt_comparands();
        let diff_abs = self.fmt_quantity(&self.diff_abs);
        let eps = self.fmt_quantity(&self.eps);

        write!(
            f,
            "Assertion failed because the following two quantities differ by {}, which is more than {}:\n  {}\n  {}",
            diff_abs, eps, lhs, rhs
        )
    }
}

/// Returns the integer (including negative sign if present) and fractional parts of the input number.
/// The input number should be a float plainly formatted as a string as with `to_string`.
fn get_float_parts(number: &str) -> (&str, &str) {
    let parts: Vec<&str> = number.split('.').collect();
    let integer_part = parts[0];
    let fractional_part = if parts.len() > 1 { parts[1] } else { "" };
    (integer_part, fractional_part)
}

/// Returns the lengths of the integer (including negative sign if present) and fractional parts of the input number.
/// The input number should be a float plainly formatted as a string as with `to_string`.
fn get_float_part_lengths(number: &str) -> (usize, usize) {
    let (integer_part, fractional_part) = get_float_parts(number);
    (integer_part.len(), fractional_part.len())
}

/// Returns the input number padded with 0s until the integer part width (number of characters) is exactly integer_part_width
/// The input number should be a float plainly formatted as a string as with `to_string`.
fn left_pad_integer_part(number: &str, integer_part_width: usize) -> CompactString {
    let (integer_part, fractional_part) = get_float_parts(number);
    let integer_part_len = integer_part.len();
    let is_negative = integer_part.starts_with('-');

    let integer_part_abs = if is_negative {
        // Strip negative sign if present
        &integer_part[1..]
    } else {
        integer_part
    };

    // Calculate padding needed
    let padding_needed = integer_part_width.saturating_sub(integer_part_len);

    // Pad integer part with 0s
    let integer_part_abs_padded = format_compact!(
        "{:0>width$}",
        integer_part_abs,
        width = padding_needed + integer_part_abs.len()
    );

    // Combine the padded integer part and the fractional part
    let padded_str = if fractional_part.is_empty() {
        integer_part_abs_padded
    } else {
        format_compact!("{}.{}", integer_part_abs_padded, fractional_part)
    };

    // Add the negative sign if necessary
    if is_negative {
        format_compact!("-{}", padded_str)
    } else {
        padded_str
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    mod get_float_parts {
        use super::*;
        #[test]
        fn positive_integer() {
            let input = 123f64.to_string();
            let result = get_float_parts(&input);
            assert_eq!(result, ("123", ""));
        }
        #[test]
        fn negative_integer() {
            let input = (-123f64).to_string();
            let result = get_float_parts(&input);
            assert_eq!(result, ("-123", ""));
        }
        #[test]
        fn integer_with_zero_fractional_part() {
            let input = (-123.000f64).to_string();
            let result = get_float_parts(&input);
            assert_eq!(result, ("-123", ""));
        }

        #[test]
        fn positive_number_with_decimal() {
            let input = (123.456f64).to_string();
            let result = get_float_parts(&input);
            assert_eq!(result, ("123", "456"));
        }

        #[test]
        fn negative_number_with_decimal() {
            let input = (-123.456f64).to_string();
            let result = get_float_parts(&input);
            assert_eq!(result, ("-123", "456"));
        }
    }
    mod left_pad {
        use super::*;

        #[test]
        fn positive_number_with_decimal() {
            let result = left_pad_integer_part("123.456", 5);
            assert_eq!(result, "00123.456");
        }

        #[test]
        fn negative_number_with_decimal() {
            let result = left_pad_integer_part("-123.456", 5);
            assert_eq!(result, "-0123.456");
        }

        #[test]
        fn positive_number_without_decimal() {
            let result = left_pad_integer_part("123", 5);
            assert_eq!(result, "00123");
        }

        #[test]
        fn negative_number_without_decimal() {
            let result = left_pad_integer_part("-123", 5);
            assert_eq!(result, "-0123");
        }

        #[test]
        fn number_already_wider_than_width() {
            let result = left_pad_integer_part("12345.6789", 4);
            assert_eq!(result, "12345.6789");
        }

        #[test]
        fn zero_padded() {
            let result = left_pad_integer_part("0.123", 5);
            assert_eq!(result, "00000.123");
        }

        #[test]
        fn negative_zero_padded() {
            let result = left_pad_integer_part("-0.123", 5);
            assert_eq!(result, "-0000.123");
        }
    }
}
