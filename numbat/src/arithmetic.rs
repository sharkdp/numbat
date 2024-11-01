use compact_str::{format_compact, CompactString};
use num_rational::Ratio;
use num_traits::Signed;

pub type Rational = Ratio<i128>;
pub type Exponent = Rational;

pub trait Power {
    fn power(self, e: Exponent) -> Self;

    fn invert(self) -> Self
    where
        Self: Sized,
    {
        self.power(Exponent::from_integer(-1))
    }
}

pub fn pretty_exponent(e: &Exponent) -> CompactString {
    CompactString::const_new(if e == &Ratio::from_integer(5) {
        "⁵"
    } else if e == &Ratio::from_integer(4) {
        "⁴"
    } else if e == &Ratio::from_integer(3) {
        "³"
    } else if e == &Ratio::from_integer(2) {
        "²"
    }
    // 1 handled by ugly_exponent
    else if e == &Ratio::from_integer(-1) {
        "⁻¹"
    } else if e == &Ratio::from_integer(-2) {
        "⁻²"
    } else if e == &Ratio::from_integer(-3) {
        "⁻³"
    } else if e == &Ratio::from_integer(-4) {
        "⁻⁴"
    } else if e == &Ratio::from_integer(-5) {
        "⁻⁵"
    } else {
        return ugly_exponent(e);
    })
}

pub fn ugly_exponent(e: &Exponent) -> CompactString {
    if e == &Ratio::from_integer(1) {
        CompactString::const_new("")
    } else if e.is_positive() && e.is_integer() {
        format_compact!("^{e}")
    } else {
        format_compact!("^({e})")
    }
}

// pub trait
