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
    if e == &Ratio::from_integer(5) {
        CompactString::const_new("⁵")
    } else if e == &Ratio::from_integer(4) {
        CompactString::const_new("⁴")
    } else if e == &Ratio::from_integer(3) {
        CompactString::const_new("³")
    } else if e == &Ratio::from_integer(2) {
        CompactString::const_new("²")
    } else if e == &Ratio::from_integer(1) {
        CompactString::const_new("")
    } else if e == &Ratio::from_integer(-1) {
        CompactString::const_new("⁻¹")
    } else if e == &Ratio::from_integer(-2) {
        CompactString::const_new("⁻²")
    } else if e == &Ratio::from_integer(-3) {
        CompactString::const_new("⁻³")
    } else if e == &Ratio::from_integer(-4) {
        CompactString::const_new("⁻⁴")
    } else if e == &Ratio::from_integer(-5) {
        CompactString::const_new("⁻⁵")
    } else if e.is_positive() && e.is_integer() {
        format_compact!("^{e}")
    } else {
        format_compact!("^({e})")
    }
}
