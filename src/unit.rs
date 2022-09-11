use std::fmt::{Display, Write};

use crate::{
    arithmetic::{Exponent, Power},
    number::Number,
    product::{Canonicalize, Product},
};

pub type ConversionFactor = Number;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnitType {
    Standard,
    NonStandard(ConversionFactor, Unit),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BaseUnit {
    name: String,
    unit_type: UnitType,
}

impl PartialOrd for BaseUnit {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.name.partial_cmp(&other.name)
    }
}

impl Ord for BaseUnit {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.name.cmp(&other.name)
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct UnitFactor(pub BaseUnit, pub Exponent);

impl Canonicalize for UnitFactor {
    type MergeKey = BaseUnit;

    fn merge_key(&self) -> Self::MergeKey {
        self.0.clone()
    }

    fn merge(self, other: Self) -> Self {
        UnitFactor(self.0, self.1 + other.1)
    }

    fn is_trivial(&self) -> bool {
        self.1 == 0
    }
}

impl Power for UnitFactor {
    fn power(self, e: Exponent) -> Self {
        UnitFactor(self.0, self.1 * e)
    }
}

pub type Unit = Product<UnitFactor, false>;

impl Unit {
    pub fn scalar() -> Self {
        Self::unity()
    }

    pub fn new_standard(name: &str) -> Self {
        Unit::from_factor(UnitFactor(
            BaseUnit {
                name: name.into(),
                unit_type: UnitType::Standard,
            },
            1,
        ))
    }

    pub fn new_non_standard(name: &str, factor: ConversionFactor, standard_unit: Unit) -> Self {
        Unit::from_factor(UnitFactor(
            BaseUnit {
                name: name.into(),
                unit_type: UnitType::NonStandard(factor, standard_unit),
            },
            1,
        ))
    }
}

impl Display for Unit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = String::new();
        for &UnitFactor(ref base_unit, exp) in self.iter() {
            result.push_str(&base_unit.name);

            match exp {
                1 => {}
                2 => result.push('²'),
                3 => result.push('³'),
                e => write!(result, "^{}", e).unwrap(),
            }
            result.push('·');
        }

        write!(f, "{}", result.trim_end_matches('·'))
    }
}

#[test]
fn unit_basic() {
    let meter = Unit::from_factor(UnitFactor(
        BaseUnit {
            name: "meter".into(),
            unit_type: UnitType::Standard,
        },
        1,
    ));
    let second = Unit::from_factor(UnitFactor(
        BaseUnit {
            name: "second".into(),
            unit_type: UnitType::Standard,
        },
        1,
    ));

    let meter_per_second = Unit::from_factors([
        UnitFactor(
            BaseUnit {
                name: "meter".into(),
                unit_type: UnitType::Standard,
            },
            1,
        ),
        UnitFactor(
            BaseUnit {
                name: "second".into(),
                unit_type: UnitType::Standard,
            },
            -1,
        ),
    ]);

    assert_eq!(meter.divide(second), meter_per_second);
}
