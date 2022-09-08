use crate::arithmetic::{Exponent, Power};
use itertools::Itertools;

pub trait Canonicalize {
    type MergeKey: PartialEq;

    fn merge_key(&self) -> Self::MergeKey;
    fn merge(self, other: Self) -> Self;
    fn is_trivial(&self) -> bool;
}

#[derive(Debug, Clone)]
pub struct Product<Factor, const CANONICALIZE: bool = false> {
    pub factors: Vec<Factor>, // TODO: make this private
}

impl<Factor: Clone + Ord + Canonicalize, const CANONICALIZE: bool> Product<Factor, CANONICALIZE> {
    pub fn unity() -> Self {
        Self::from_factors([])
    }

    pub fn from_factors(factors: impl IntoIterator<Item = Factor>) -> Self {
        Self::from_vec(factors.into_iter().collect())
    }

    fn from_vec(factors: Vec<Factor>) -> Self {
        let mut product = Self { factors };
        product.canonicalize();
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

    pub fn sort_unstable(&mut self) {
        self.factors.sort_unstable();
    }

    fn canonicalize(&mut self) {
        if CANONICALIZE {
            self.sort_unstable();

            // Merge adjacent
            let mut new_factors = vec![];
            for (_, group) in &self.factors.iter().cloned().group_by(|f1| f1.merge_key()) {
                let merged = group.reduce(|acc, item| acc.merge(item)).unwrap();
                if !merged.is_trivial() {
                    new_factors.push(merged);
                }
            }
            self.factors = new_factors;
        }
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
        self.power(-1)
    }

    pub fn divide(self, other: Self) -> Self {
        let mut result = self.multiply(other.invert());
        result.canonicalize();
        result
    }
}

impl<Factor: PartialEq, const CANONICALIZE: bool> PartialEq for Product<Factor, CANONICALIZE> {
    fn eq(&self, other: &Self) -> bool {
        self.factors == other.factors // TODO: do we need to canonicalize first?
    }
}

impl<Factor: PartialEq, const CANONICALIZE: bool> Eq for Product<Factor, CANONICALIZE> {}

pub struct ProductIter<'a, Factor> {
    inner: std::slice::Iter<'a, Factor>,
}

impl<'a, Factor> Iterator for ProductIter<'a, Factor> {
    type Item = &'a Factor;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

#[cfg(test)]
impl Canonicalize for i32 {
    type MergeKey = i32;

    fn merge_key(&self) -> Self::MergeKey {
        *self
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
        result.iter().cloned().collect::<Vec<_>>().as_slice(),
        [5, 2, 3, 6, 8]
    );
}

#[test]
fn test_multiply_canonicalize() {
    let product1 = Product::<TestUnit, true>::from_factors([
        TestUnit("meter".into(), 1),
        TestUnit("second".into(), 1),
    ]);
    let product2 = Product::from_factors([TestUnit("meter".into(), 2)]);
    let result = product1.multiply(product2);
    assert_eq!(
        result.into_vec(),
        &[TestUnit("meter".into(), 3), TestUnit("second".into(), 1)]
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
        self.1 == 0
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
        TestUnit("meter".into(), 1),
        TestUnit("second".into(), -2),
    ]);
    let result = product.power(3);
    assert_eq!(
        result.into_vec(),
        &[TestUnit("meter".into(), 3), TestUnit("second".into(), -6)]
    );
}

#[test]
fn test_divide() {
    let product1 = Product::<TestUnit>::from_factors([
        TestUnit("meter".into(), 1),
        TestUnit("second".into(), 1),
    ]);
    let product2 = Product::from_factors([TestUnit("second".into(), 1)]);
    let result = product1.divide(product2);
    assert_eq!(
        result.into_vec(),
        &[
            TestUnit("meter".into(), 1),
            TestUnit("second".into(), 1),
            TestUnit("second".into(), -1)
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
fn test_sort_unstable() {
    let mut product = Product::<i32>::from_factors([5, 2, 3]);
    product.sort_unstable();
    assert_eq!(
        product.iter().cloned().collect::<Vec<_>>().as_slice(),
        [2, 3, 5]
    );
}
