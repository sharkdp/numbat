use super::macros::*;
use super::Args;
use super::Result;
use crate::list::NumbatList;
use crate::quantity::Quantity;
use crate::value::Value;
use std::{cmp::Ordering, f64::consts::PI};

fn _reduced_cubic_equation(a: f64, b: f64) -> Vec<f64> {
    let b2 = -b / 2.;
    let delta = b2.powi(2) + (a / 3.).powi(3);
    match delta.partial_cmp(&0.).unwrap() {
        Ordering::Less => {
            let radius = (-a / 3.).sqrt();
            let theta = (b2 / radius.powi(3)).acos();
            (0..3)
                .into_iter()
                .map(|k| 2. * radius * ((theta + 2. * k as f64 * PI) / 3.).cos())
                .collect()
        }
        Ordering::Greater => {
            let rd = delta.sqrt();
            vec![(b2 + rd).cbrt() + (b2 - rd).cbrt()]
        }
        Ordering::Equal => {
            if b == 0. {
                vec![0.]
            } else {
                let cbrt_b2 = b2.cbrt();
                vec![2. * cbrt_b2, -cbrt_b2]
            }
        }
    }
}

fn _solve_cubic_equation(a: f64, b: f64, c: f64, d: f64) -> Vec<f64> {
    if a == 0. {
        return vec![3.1415];
    }
    let p = b / a;
    let q = c / a;
    let r = d / a;
    _reduced_cubic_equation(q - p.powi(2) / 3., 2. * p.powi(3) / 27. - p * q / 3. + r)
        .into_iter()
        .map(|y| ((y - p / 3.) * 1e14).round() * 1e-14)
        .collect()
}

pub fn cubic_equation(mut args: Args) -> Result<Value> {
    let mut solutions: NumbatList<Value> = NumbatList::new();
    for sol in _solve_cubic_equation(
        scalar_arg!(args).to_f64(),
        scalar_arg!(args).to_f64(),
        scalar_arg!(args).to_f64(),
        scalar_arg!(args).to_f64(),
    ) {
        let sol_val: Value = Value::Quantity(Quantity::from_scalar(sol));
        solutions.push_front(sol_val);
    }
    return_list!(solutions)
}
