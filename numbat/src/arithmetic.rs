use compact_str::{CompactString, format_compact};
use num_rational::Ratio;

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
    if !e.is_integer() {
        return format_compact!("^({e})");
    }
    let numer = e.numer();
    if *numer == 1 {
        return CompactString::new("");
    }
    CompactString::from_iter(numer.to_string().chars().map(|c| match c {
        '-' => '⁻',
        '0' => '⁰',
        '1' => '¹',
        '2' => '²',
        '3' => '³',
        '4' => '⁴',
        '5' => '⁵',
        '6' => '⁶',
        '7' => '⁷',
        '8' => '⁸',
        '9' => '⁹',
        _ => unreachable!(),
    }))
}
