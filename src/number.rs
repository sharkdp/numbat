#[derive(Debug, Clone, PartialEq)]
pub struct Number(pub f64);

impl Eq for Number {}

impl Number {
    pub fn from_f64(n: f64) -> Self {
        Number(n)
    }

    pub fn to_f64(&self) -> f64 {
        let Number(n) = *self;
        n
    }
}

impl std::ops::Add for Number {
    type Output = Number;

    fn add(self, rhs: Self) -> Self::Output {
        Number(self.0 + rhs.0)
    }
}

impl std::ops::Sub for Number {
    type Output = Number;

    fn sub(self, rhs: Self) -> Self::Output {
        Number(self.0 - rhs.0)
    }
}

impl std::ops::Mul for Number {
    type Output = Number;

    fn mul(self, rhs: Self) -> Self::Output {
        Number(self.0 * rhs.0)
    }
}

impl std::ops::Div for Number {
    type Output = Number;

    fn div(self, rhs: Self) -> Self::Output {
        Number(self.0 / rhs.0)
    }
}

impl std::ops::Neg for Number {
    type Output = Number;

    fn neg(self) -> Self::Output {
        Number(-self.0)
    }
}
