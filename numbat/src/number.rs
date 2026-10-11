use std::fmt::Display;

use compact_str::{CompactString, ToCompactString, format_compact};
use num_traits::ToPrimitive;
use pretty_dtoa::FmtFloatConfig;

use crate::pretty_print::FormatOptions;

#[derive(Debug, Clone, Copy)]
pub struct Number {
    pub re: f64,
    pub im: f64,
}

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        self.re == other.re && self.im == other.im
    }
}

impl Eq for Number {}

impl Number {
    pub fn new(re: f64, im: f64) -> Self {
        Number { re, im }
    }

    pub fn from_f64(n: f64) -> Self {
        Number { re: n, im: 0.0 }
    }

    pub fn to_f64(self) -> f64 {
        debug_assert!(
            self.is_real(),
            "to_f64 called on complex number: {} + {}i",
            self.re,
            self.im
        );
        self.re
    }

    pub fn try_as_real(self) -> Option<f64> {
        if self.is_real() {
            Some(self.re)
        } else {
            None
        }
    }

    pub fn is_real(self) -> bool {
        self.im == 0.0
    }

    pub fn re(self) -> f64 {
        self.re
    }

    pub fn im(self) -> f64 {
        self.im
    }

    pub fn pow(self, other: &Number) -> Self {
        // Real fast path
        if self.is_real() && other.is_real() {
            let result = self.re.powf(other.re);
            if !result.is_nan() || self.re.is_nan() || other.re.is_nan() {
                return Number::from_f64(result);
            }
            // Fall through to complex path (e.g. (-1)^0.5)
        }

        // Complex path: z^w = exp(w * ln(z))
        let z = self;
        let w = *other;

        // ln(z) = ln|z| + i*arg(z)
        let abs_z = z.re.hypot(z.im);
        if abs_z == 0.0 {
            // 0^w = 0 for positive real part of w
            return Number::from_f64(0.0);
        }
        let ln_abs_z = abs_z.ln();
        let arg_z = z.im.atan2(z.re);

        // w * ln(z)
        let ln_z_re = ln_abs_z;
        let ln_z_im = arg_z;

        let product_re = w.re * ln_z_re - w.im * ln_z_im;
        let product_im = w.re * ln_z_im + w.im * ln_z_re;

        // exp(product)
        let exp_re = product_re.exp();
        let result_re = exp_re * product_im.cos();
        let result_im = exp_re * product_im.sin();

        // Clean up tiny imaginary parts from floating point errors
        if result_im.abs() < 1e-14 * result_re.abs().max(1.0) {
            Number::from_f64(result_re)
        } else if result_re.abs() < 1e-14 * result_im.abs().max(1.0) {
            Number::new(0.0, result_im)
        } else {
            Number::new(result_re, result_im)
        }
    }

    pub fn abs(self) -> Self {
        if self.is_real() {
            Number::from_f64(self.re.abs())
        } else {
            Number::from_f64(self.re.hypot(self.im))
        }
    }

    pub fn conj(self) -> Self {
        Number::new(self.re, -self.im)
    }

    pub fn arg(self) -> Self {
        Number::from_f64(self.im.atan2(self.re))
    }

    pub fn is_integer(self) -> bool {
        self.is_real() && self.re.trunc() == self.re
    }

    /// Pretty prints with default format options.
    pub fn pretty_print(self) -> CompactString {
        self.pretty_print_with(&FormatOptions::default())
    }

    /// Pretty prints with the given format options.
    pub fn pretty_print_with(self, options: &FormatOptions) -> CompactString {
        self.pretty_print_with_dtoa_config(options, None)
    }

    /// Format a single real f64 value with the given format options and optional dtoa config.
    fn format_real(
        value: f64,
        options: &FormatOptions,
        dtoa_config: Option<FmtFloatConfig>,
    ) -> CompactString {
        // 64-bit floats can accurately represent integers up to 2^53 [1],
        // which is approximately 9.0 × 10^15.
        //
        // [1] https://stackoverflow.com/a/43656339
        //
        // Skip special format handling for integers if dtoa_config is provided.
        let is_integer = value.trunc() == value;
        if dtoa_config.is_none() && is_integer && value.abs() < (2.0_f64).powi(53) {
            use num_format::{CustomFormat, Grouping, ToFormattedString};

            let threshold = options.digit_grouping_threshold as f64;
            let use_grouping = !options.digit_separator.is_empty()
                && value.abs() >= 10.0_f64.powf(threshold - 1.0);

            let format = CustomFormat::builder()
                .grouping(if use_grouping {
                    Grouping::Standard
                } else {
                    Grouping::Posix
                })
                .minus_sign("-")
                .separator(&options.digit_separator)
                .build()
                .unwrap();

            // TODO: this is pretty wasteful. formatted numbers should be small enough
            // to fit in a CompactString without first going to the heap
            value
                .to_i64()
                .expect("small enough integers are representable as i64")
                .to_formatted_string(&format)
                .to_compact_string()
        } else {
            use pretty_dtoa::dtoa;

            let config = if let Some(config) = dtoa_config {
                config
            } else {
                FmtFloatConfig::default()
                    .max_significant_digits(options.significant_digits as u8)
                    .add_point_zero(false)
                    .lower_e_break(-6)
                    .upper_e_break(6)
                    .round()
            };

            let formatted_number = dtoa(value, config);

            if formatted_number.contains('.') && !formatted_number.contains('e') {
                let formatted_number = if config.max_sig_digits.is_some() {
                    formatted_number.trim_end_matches('0')
                } else {
                    &formatted_number
                };

                if formatted_number.ends_with('.') {
                    format_compact!("{formatted_number}0")
                } else {
                    formatted_number.to_compact_string()
                }
            } else if formatted_number.contains('e') && !formatted_number.contains("e-") {
                formatted_number.replace('e', "e+").to_compact_string()
            } else {
                formatted_number.to_compact_string()
            }
        }
    }

    /// Pretty prints with the given format options and optional dtoa config override.
    /// If dtoa_config is Some, it overrides the default float formatting and skips integer handling.
    pub fn pretty_print_with_dtoa_config(
        self,
        options: &FormatOptions,
        dtoa_config: Option<FmtFloatConfig>,
    ) -> CompactString {
        if self.is_real() {
            Self::format_real(self.re, options, dtoa_config)
        } else if self.re == 0.0 {
            // Pure imaginary
            format_imaginary_part(self.im, options, dtoa_config)
        } else {
            // Full complex: "a + bi" or "a - bi"
            let real_str = Self::format_real(self.re, options, dtoa_config);
            if self.im > 0.0 {
                let im_str = format_imaginary_part(self.im, options, dtoa_config);
                format_compact!("{real_str} + {im_str}")
            } else {
                let im_str = format_imaginary_part(-self.im, options, dtoa_config);
                format_compact!("{real_str} - {im_str}")
            }
        }
    }
}

/// Format the imaginary part as "i", "-i", "2i", etc.
/// `value` should be the absolute value of the imaginary coefficient when called
/// from `pretty_print_with_dtoa_config`, or the signed value for pure imaginary.
fn format_imaginary_part(
    value: f64,
    options: &FormatOptions,
    dtoa_config: Option<FmtFloatConfig>,
) -> CompactString {
    if value == 1.0 {
        CompactString::const_new("i")
    } else if value == -1.0 {
        CompactString::const_new("-i")
    } else {
        let formatted = Number::format_real(value, options, dtoa_config);
        format_compact!("{formatted}i")
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_real() {
            self.re.fmt(f)
        } else if self.re == 0.0 {
            if self.im == 1.0 {
                write!(f, "i")
            } else if self.im == -1.0 {
                write!(f, "-i")
            } else {
                write!(f, "{}i", self.im)
            }
        } else if self.im > 0.0 {
            if self.im == 1.0 {
                write!(f, "{} + i", self.re)
            } else {
                write!(f, "{} + {}i", self.re, self.im)
            }
        } else if self.im == -1.0 {
            write!(f, "{} - i", self.re)
        } else {
            write!(f, "{} - {}i", self.re, -self.im)
        }
    }
}

impl std::ops::Add for Number {
    type Output = Number;

    fn add(self, rhs: Self) -> Self::Output {
        Number::new(self.re + rhs.re, self.im + rhs.im)
    }
}

impl std::ops::Sub for Number {
    type Output = Number;

    fn sub(self, rhs: Self) -> Self::Output {
        let im = self.im - rhs.im;
        // Canonicalize -0.0 to 0.0 in the imaginary part
        Number::new(self.re - rhs.re, if im == 0.0 && im.is_sign_negative() { 0.0 } else { im })
    }
}

impl std::ops::Mul for Number {
    type Output = Number;

    fn mul(self, rhs: Self) -> Self::Output {
        // Fast path for real multiplication
        if self.im == 0.0 && rhs.im == 0.0 {
            return Number::from_f64(self.re * rhs.re);
        }
        // (a+bi)(c+di) = (ac-bd) + (ad+bc)i
        Number::new(
            self.re * rhs.re - self.im * rhs.im,
            self.re * rhs.im + self.im * rhs.re,
        )
    }
}

impl std::ops::Div for Number {
    type Output = Number;

    fn div(self, rhs: Self) -> Self::Output {
        // Fast path for real division
        if self.im == 0.0 && rhs.im == 0.0 {
            return Number::from_f64(self.re / rhs.re);
        }
        // (a+bi)/(c+di) = ((ac+bd) + (bc-ad)i) / (c²+d²)
        let denom = rhs.re * rhs.re + rhs.im * rhs.im;
        Number::new(
            (self.re * rhs.re + self.im * rhs.im) / denom,
            (self.im * rhs.re - self.re * rhs.im) / denom,
        )
    }
}

impl std::ops::Neg for Number {
    type Output = Number;

    fn neg(self) -> Self::Output {
        // For real numbers, preserve im=0.0 (avoid IEEE 754 negative zero -0.0
        // which would cause atan2(-0.0, x) to return -π instead of π).
        if self.im == 0.0 {
            Number::new(-self.re, 0.0)
        } else {
            Number::new(-self.re, -self.im)
        }
    }
}

impl std::iter::Product for Number {
    fn product<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(Number::from_f64(1.0), |acc, n| acc * n)
    }
}

#[test]
fn test_pretty_print() {
    assert_eq!(Number::from_f64(1.).pretty_print(), "1");
    assert_eq!(Number::from_f64(100.).pretty_print(), "100");
    assert_eq!(Number::from_f64(1.234).pretty_print(), "1.234");
    assert_eq!(Number::from_f64(12345.6).pretty_print(), "12345.6");
    assert_eq!(Number::from_f64(1.234e50).pretty_print(), "1.234e+50");
    assert_eq!(Number::from_f64(-1.234e50).pretty_print(), "-1.234e+50");
    assert_eq!(Number::from_f64(1.234e-50).pretty_print(), "1.234e-50");
    assert_eq!(Number::from_f64(-1.234e-50).pretty_print(), "-1.234e-50");

    assert_eq!(Number::from_f64(1234.).pretty_print(), "1234");
    assert_eq!(Number::from_f64(12345.).pretty_print(), "12345");
    assert_eq!(Number::from_f64(123456.).pretty_print(), "123_456");
    assert_eq!(
        Number::from_f64(1234567890.).pretty_print(),
        "1_234_567_890"
    );
    assert_eq!(
        Number::from_f64(1234567890000000.).pretty_print(),
        "1_234_567_890_000_000"
    );
    assert_eq!(
        Number::from_f64(12345678900000000.).pretty_print(),
        "1.23457e+16"
    );
    // 2^53 - 1 is the largest integer that can be unambiguously
    // represented as f64. At 2^53 and beyond, adjacent integers
    // round to the same f64 value, so we switch to scientific
    // notation to avoid displaying a falsely precise result.
    assert_eq!(
        Number::from_f64(9_007_199_254_740_991.).pretty_print(),
        "9_007_199_254_740_991"
    );
    assert_eq!(
        Number::from_f64(9_007_199_254_740_992.).pretty_print(),
        "9.0072e+15"
    );

    // UNIX timestamps in µs (e.g. 2026-01-27) are well within the
    // exact integer range and should display without scientific notation.
    assert_eq!(
        Number::from_f64(1_769_519_170_818_991.).pretty_print(),
        "1_769_519_170_818_991"
    );

    assert_eq!(Number::from_f64(1.23456789).pretty_print(), "1.23457");
    assert_eq!(
        Number::from_f64(1234567890000.1).pretty_print(),
        "1.23457e+12"
    );

    assert_eq!(Number::from_f64(100.00001).pretty_print(), "100.0");

    assert_eq!(Number::from_f64(0.00001).pretty_print(), "0.00001");
    assert_eq!(Number::from_f64(0.000001).pretty_print(), "0.000001");
    assert_eq!(Number::from_f64(0.0000001).pretty_print(), "1.0e-7");

    // Test with no digit separator
    let no_sep = FormatOptions {
        digit_separator: "".to_string(),
        ..FormatOptions::default()
    };
    assert_eq!(
        Number::from_f64(1234567890.).pretty_print_with(&no_sep),
        "1234567890"
    );

    // Test with comma separator
    let comma_sep = FormatOptions {
        digit_separator: ",".to_string(),
        ..FormatOptions::default()
    };
    assert_eq!(
        Number::from_f64(1234567890.).pretty_print_with(&comma_sep),
        "1,234,567,890"
    );
}

#[test]
fn test_abs() {
    assert_eq!(Number::from_f64(0.0).abs(), Number::from_f64(0.0));
    assert_eq!(Number::from_f64(1.0).abs(), Number::from_f64(1.0));
    assert_eq!(Number::from_f64(-1.0).abs(), Number::from_f64(1.0));
}

#[test]
fn test_complex_arithmetic() {
    let a = Number::new(3.0, 2.0);
    let b = Number::new(1.0, 4.0);

    // Addition
    let sum = a + b;
    assert_eq!(sum.re, 4.0);
    assert_eq!(sum.im, 6.0);

    // Subtraction
    let diff = a - b;
    assert_eq!(diff.re, 2.0);
    assert_eq!(diff.im, -2.0);

    // Multiplication: (3+2i)(1+4i) = (3-8) + (12+2)i = -5 + 14i
    let prod = a * b;
    assert_eq!(prod.re, -5.0);
    assert_eq!(prod.im, 14.0);

    // i * i = -1
    let i = Number::new(0.0, 1.0);
    let i_squared = i * i;
    assert!((i_squared.re - (-1.0)).abs() < 1e-15);
    assert!(i_squared.im.abs() < 1e-15);
}

#[test]
fn test_complex_pow() {
    // sqrt(-1) = i
    let neg1 = Number::from_f64(-1.0);
    let half = Number::from_f64(0.5);
    let result = neg1.pow(&half);
    assert!(result.re.abs() < 1e-14);
    assert!((result.im - 1.0).abs() < 1e-14);
}

#[test]
fn test_complex_pretty_print() {
    assert_eq!(Number::new(0.0, 1.0).pretty_print(), "i");
    assert_eq!(Number::new(0.0, -1.0).pretty_print(), "-i");
    assert_eq!(Number::new(0.0, 2.0).pretty_print(), "2i");
    assert_eq!(Number::new(0.0, -2.0).pretty_print(), "-2i");
    assert_eq!(Number::new(3.0, 2.0).pretty_print(), "3 + 2i");
    assert_eq!(Number::new(3.0, -2.0).pretty_print(), "3 - 2i");
    assert_eq!(Number::new(3.0, 1.0).pretty_print(), "3 + i");
    assert_eq!(Number::new(3.0, -1.0).pretty_print(), "3 - i");
}
