use num_rational::Rational64;

pub type Rational = Rational64;
pub type Exponent = Rational;

pub trait Power {
    fn power(self, e: Exponent) -> Self;
}
