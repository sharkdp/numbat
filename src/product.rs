use crate::arithmetic::{Exponent, Power};

pub struct Product<Factor> {
    factors: Vec<Factor>,
}

impl<Factor> Product<Factor> {
    pub fn from_factors(factors: impl IntoIterator<Item = Factor>) -> Self {
        Self {
            factors: factors.into_iter().collect(),
        }
    }

    pub fn multiply(mut self, mut other: Product<Factor>) -> Product<Factor> {
        self.factors.append(&mut other.factors);
        Product::<Factor> {
            factors: self.factors,
        }
    }

    pub fn iter(&self) -> ProductIter<Factor> {
        ProductIter {
            inner: self.factors.iter(),
        }
    }

    pub fn sort_by_key<K, F>(&mut self, f: F)
    where
        F: FnMut(&Factor) -> K,
        K: Ord,
    {
        self.factors.sort_by_key(f);
    }

    #[cfg(test)]
    fn into_vec(self) -> Vec<Factor> {
        self.factors
    }
}

impl<Factor: Ord> Product<Factor> {
    pub fn sort(&mut self) {
        self.factors.sort();
    }
}

impl<Factor: Power> Power for Product<Factor> {
    fn power(self, exp: Exponent) -> Self {
        Product::from_factors(self.factors.into_iter().map(|f| f.power(exp)))
    }
}

impl<Factor: Power> Product<Factor> {
    pub fn invert(self) -> Self {
        self.power(-1)
    }

    pub fn divide(self, other: Product<Factor>) -> Product<Factor> {
        self.multiply(other.invert())
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

#[cfg(test)]
#[derive(Debug, PartialEq)]
struct TestUnit(String, Exponent);

#[cfg(test)]
impl Power for TestUnit {
    fn power(self, e: Exponent) -> Self {
        TestUnit(self.0, self.1 * e)
    }
}

#[test]
fn test_power() {
    let product =
        Product::from_factors([TestUnit("meter".into(), 1), TestUnit("second".into(), -2)]);
    let result = product.power(3);
    assert_eq!(
        result.into_vec(),
        &[TestUnit("meter".into(), 3), TestUnit("second".into(), -6)]
    );
}

#[test]
fn test_divide() {
    let product1 =
        Product::from_factors([TestUnit("meter".into(), 1), TestUnit("second".into(), 1)]);
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
fn test_sort() {
    let mut product = Product::<i32>::from_factors([5, 2, 3]);
    product.sort();
    assert_eq!(
        product.iter().cloned().collect::<Vec<_>>().as_slice(),
        [2, 3, 5]
    );
}

#[test]
fn test_sort_by_key() {
    let mut product = Product::<_>::from_factors([(1.0, 2), (2.0, 1), (3.0, -3)]);
    product.sort_by_key(|(_, second)| *second);
    assert_eq!(
        product
            .iter()
            .map(|(first, _)| first)
            .cloned()
            .collect::<Vec<_>>()
            .as_slice(),
        [3.0, 2.0, 1.0]
    );
}
