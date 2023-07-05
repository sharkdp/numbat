use num_rational::{Ratio, Rational64};
use num_traits::Signed;

pub type Rational = Rational64;
pub type Exponent = Rational;

pub trait Power {
    fn power(self, e: Exponent) -> Self;
}

pub fn pretty_exponent(e: &Exponent) -> String {
    if e == &Ratio::from_integer(5) {
        "⁵".into()
    } else if e == &Ratio::from_integer(4) {
        "⁴".into()
    } else if e == &Ratio::from_integer(3) {
        "³".into()
    } else if e == &Ratio::from_integer(2) {
        "²".into()
    } else if e == &Ratio::from_integer(1) {
        "".into()
    } else if e == &Ratio::from_integer(-1) {
        "⁻¹".into()
    } else if e == &Ratio::from_integer(-2) {
        "⁻²".into()
    } else if e == &Ratio::from_integer(-3) {
        "⁻³".into()
    } else if e == &Ratio::from_integer(-4) {
        "⁻⁴".into()
    } else if e == &Ratio::from_integer(-5) {
        "⁻⁵".into()
    } else {
        if e.is_positive() {
            format!("^{}", e)
        } else {
            format!("^({})", e)
        }
    }
}
