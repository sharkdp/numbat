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
    } else if e.is_positive() && e.is_integer() {
        format!("^{e}")
    } else {
        format!("^({e})")
    }
}
