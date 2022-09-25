use crate::arithmetic::{Exponent, Power, Rational};
use itertools::Itertools;

pub trait Canonicalize {
    type MergeKey: PartialEq;

    fn merge_key(&self) -> Self::MergeKey;
    fn merge(self, other: Self) -> Self;
    fn is_trivial(&self) -> bool;
}

#[derive(Debug, Clone)]
pub struct Product<Factor, const CANONICALIZE: bool = false> {
    factors: Vec<Factor>,
}

impl<Factor: Clone + Ord + Canonicalize, const CANONICALIZE: bool> Product<Factor, CANONICALIZE> {
    pub fn unity() -> Self {
        Self::from_factors([])
    }

    pub fn from_factors(factors: impl IntoIterator<Item = Factor>) -> Self {
        Self::from_vec(factors.into_iter().collect())
    }

    pub fn from_factor(factor: Factor) -> Self {
        Self {
            factors: vec![factor],
        }
    }

    fn from_vec(factors: Vec<Factor>) -> Self {
        let mut product = Self { factors };
        product.automated_canonicalize();
        product
    }

    pub fn multiply(mut self, mut other: Self) -> Self {
        self.factors.append(&mut other.factors);
        Self::from_vec(self.factors)
    }

    pub fn iter(&self) -> ProductIter<Factor> {
        ProductIter {
            inner: self.factors.iter(),
        }
    }

    #[cfg(test)]
    fn into_vec(self) -> Vec<Factor> {
        self.factors
    }

    fn automated_canonicalize(&mut self) {
        if CANONICALIZE {
            self.canonicalize();
        }
    }

    pub fn canonicalize(&mut self) {
        self.factors.sort_unstable();

        self.factors = self
            .factors
            .iter()
            .cloned()
            .group_by(|f1| f1.merge_key())
            .into_iter()
            .map(|(_, group)| {
                group
                    .reduce(|acc, item| acc.merge(item))
                    .expect("non zero group")
            })
            .filter(|factor| !factor.is_trivial())
            .collect();
    }

    fn canonicalized(&self) -> Self {
        let mut result = self.clone();
        result.canonicalize();
        result
    }
}

impl<Factor: Power + Clone + Canonicalize + Ord, const CANONICALIZE: bool> Power
    for Product<Factor, CANONICALIZE>
{
    fn power(self, exp: Exponent) -> Self {
        Product::from_factors(self.factors.into_iter().map(|f| f.power(exp)))
    }
}

impl<Factor: Power + Clone + Canonicalize + Ord, const CANONICALIZE: bool>
    Product<Factor, CANONICALIZE>
{
    pub fn invert(self) -> Self {
        self.power(Rational::from_integer(-1))
    }

    pub fn divide(self, other: Self) -> Self {
        let mut result = self.multiply(other.invert());
        result.automated_canonicalize();
        result
    }
}

impl<Factor: Clone + Ord + PartialEq + Canonicalize, const CANONICALIZE: bool> PartialEq
    for Product<Factor, CANONICALIZE>
{
    fn eq(&self, other: &Self) -> bool {
        self.canonicalized().factors == other.canonicalized().factors
    }
}

impl<Factor: Clone + Ord + Canonicalize + Eq, const CANONICALIZE: bool> Eq
    for Product<Factor, CANONICALIZE>
{
}

impl<Factor, const CANONICALIZE: bool> IntoIterator for Product<Factor, CANONICALIZE> {
    type IntoIter = ProductIntoIter<Factor>;
    type Item = Factor;

    fn into_iter(self) -> Self::IntoIter {
        ProductIntoIter {
            inner: self.factors.into_iter(),
        }
    }
}

pub struct ProductIter<'a, Factor> {
    inner: std::slice::Iter<'a, Factor>,
}

impl<'a, Factor> Iterator for ProductIter<'a, Factor> {
    type Item = &'a Factor;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

pub struct ProductIntoIter<Factor> {
    inner: std::vec::IntoIter<Factor>,
}

impl<Factor> Iterator for ProductIntoIter<Factor> {
    type Item = Factor;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

#[cfg(test)]
impl Canonicalize for i32 {
    type MergeKey = ();

    fn merge_key(&self) -> Self::MergeKey {
        // merge everything
    }

    fn merge(self, other: Self) -> Self {
        self * other
    }

    fn is_trivial(&self) -> bool {
        *self == 1
    }
}

#[test]
fn test_multiply() {
    let product1 = Product::<i32>::from_factors([5, 2, 3]);
    let product2 = Product::<i32>::from_factors([6, 8]);
    let result = product1.multiply(product2);
    assert_eq!(
        result.into_iter().collect::<Vec<_>>().as_slice(),
        [5, 2, 3, 6, 8]
    );
}

#[test]
fn test_multiply_canonicalize() {
    let product1 = Product::<TestUnit, true>::from_factors([
        TestUnit("meter".into(), Rational::from_integer(1)),
        TestUnit("second".into(), Rational::from_integer(1)),
    ]);
    let product2 = Product::from_factor(TestUnit("meter".into(), Rational::from_integer(2)));
    let result = product1.multiply(product2);
    assert_eq!(
        result.into_vec(),
        &[
            TestUnit("meter".into(), Rational::from_integer(3)),
            TestUnit("second".into(), Rational::from_integer(1))
        ]
    );
}

#[cfg(test)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct TestUnit(String, Exponent);

#[cfg(test)]
impl Canonicalize for TestUnit {
    type MergeKey = String;

    fn merge_key(&self) -> Self::MergeKey {
        self.0.clone()
    }

    fn merge(self, other: Self) -> Self {
        TestUnit(self.0, self.1 + other.1)
    }

    fn is_trivial(&self) -> bool {
        use num_traits::Zero;
        self.1 == Rational::zero()
    }
}

#[cfg(test)]
impl Power for TestUnit {
    fn power(self, e: Exponent) -> Self {
        TestUnit(self.0, self.1 * e)
    }
}

#[test]
fn test_power() {
    let product = Product::<TestUnit>::from_factors([
        TestUnit("meter".into(), Rational::from_integer(1)),
        TestUnit("second".into(), Rational::from_integer(-2)),
    ]);
    let result = product.power(Rational::from_integer(3));
    assert_eq!(
        result.into_vec(),
        &[
            TestUnit("meter".into(), Rational::from_integer(3)),
            TestUnit("second".into(), Rational::from_integer(-6))
        ]
    );
}

#[test]
fn test_divide() {
    let product1 = Product::<TestUnit>::from_factors([
        TestUnit("meter".into(), Rational::from_integer(1)),
        TestUnit("second".into(), Rational::from_integer(1)),
    ]);
    let product2 = Product::from_factor(TestUnit("second".into(), Rational::from_integer(1)));
    let result = product1.divide(product2);
    assert_eq!(
        result.into_vec(),
        &[
            TestUnit("meter".into(), Rational::from_integer(1)),
            TestUnit("second".into(), Rational::from_integer(1)),
            TestUnit("second".into(), Rational::from_integer(-1))
        ]
    );
}

#[test]
fn test_iter() {
    let product = Product::<i32>::from_factors([5, 2, 3]);
    let mut iter = product.iter();
    assert_eq!(iter.next(), Some(&5));
    assert_eq!(iter.next(), Some(&2));
    assert_eq!(iter.next(), Some(&3));
    assert_eq!(iter.next(), None);
    assert_eq!(iter.next(), None);
}

#[test]
fn test_canonicalize() {
    let mut product = Product::<i32>::from_factors([5, 2, 3]);
    product.canonicalize();
    assert_eq!(product.into_iter().collect::<Vec<_>>().as_slice(), [30]);
}
