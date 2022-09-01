pub type Exponent = i32;

pub trait Power {
    fn power(self, e: Exponent) -> Self;
}
