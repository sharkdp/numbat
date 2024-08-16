use std::{collections::HashMap, error::Error, fmt};

use crate::arithmetic::{pretty_exponent, Exponent, Rational};
use crate::registry::{BaseRepresentation, BaseRepresentationFactor};
use crate::span::Span;

use itertools::Itertools;
use num_traits::Zero;
use unicode_width::UnicodeWidthStr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IncompatibleDimensionsError {
    pub span_operation: Span,
    pub operation: String,
    pub span_expected: Span,
    pub expected_name: &'static str,
    pub expected_type: BaseRepresentation,
    pub expected_dimensions: Vec<String>,
    pub span_actual: Span,
    pub actual_name: &'static str,
    pub actual_name_for_fix: &'static str,
    pub actual_type: BaseRepresentation,
    pub actual_dimensions: Vec<String>,
}

fn pad(a: &str, b: &str) -> (String, String) {
    let max_length = a.width().max(b.width());

    (format!("{a: <max_length$}"), format!("{b: <max_length$}"))
}

fn suggested_fix(
    expected_type: &BaseRepresentation,
    actual_type: &BaseRepresentation,
    expression_to_change: &str,
) -> Option<String> {
    // Heuristic 1: if actual_type == 1 / expected_type, suggest
    // to invert the 'actual' expression:
    if actual_type == &expected_type.clone().invert() {
        return Some(format!("invert the {expression_to_change}"));
    }

    // Heuristic 2: compute the "missing" factor between the expected
    // and the actual type. Suggest to multiply / divide with the
    // appropriate delta.
    let delta_type = expected_type.clone() / actual_type.clone();

    let num_factors = delta_type.iter().count();
    if num_factors > 1 {
        return None; // Do not suggest fixes with complicated dimensions
    }

    let exponent_sum: Rational = delta_type.iter().map(|a| a.1).sum();

    let (action, delta_type) = if exponent_sum >= Rational::zero() {
        ("multiply", delta_type)
    } else {
        ("divide", delta_type.invert())
    };

    Some(format!(
        "{action} the {expression_to_change} by a `{delta_type}` factor"
    ))
}

impl fmt::Display for IncompatibleDimensionsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let have_common_factors = self
            .expected_type
            .iter()
            .any(|f| self.actual_type.iter().map(|f| &f.0).contains(&f.0));

        let (mut expected_result_string, mut actual_result_string) = if !have_common_factors
            || (self.expected_type.iter().count() == 1 && self.actual_type.iter().count() == 1)
        {
            pad(
                &self.expected_type.to_string(),
                &self.actual_type.to_string(),
            )
        } else {
            let format_factor =
                |name: &str, exponent: &Exponent| format!(" × {name}{}", pretty_exponent(exponent));

            let mut shared_factors = HashMap::<&String, (Exponent, Exponent)>::new();
            let mut expected_factors = HashMap::<&String, Exponent>::new();
            let mut actual_factors = HashMap::<&String, Exponent>::new();

            for BaseRepresentationFactor(name, expected_exponent) in self.expected_type.iter() {
                if let Some(BaseRepresentationFactor(_, actual_exponent)) =
                    self.actual_type.iter().find(|f| *name == f.0)
                {
                    shared_factors.insert(name, (*expected_exponent, *actual_exponent));
                } else {
                    expected_factors.insert(name, *expected_exponent);
                }
            }

            for BaseRepresentationFactor(name, exponent) in self.actual_type.iter() {
                if !shared_factors.contains_key(&name) {
                    actual_factors.insert(name, *exponent);
                }
            }

            let mut expected_result_string = String::new();
            let mut actual_result_string = String::new();

            for (name, (exp1, exp2)) in shared_factors
                .iter()
                .sorted_unstable_by_key(|entry| entry.0)
            {
                let (str1, str2) = pad(&format_factor(name, exp1), &format_factor(name, exp2));

                expected_result_string.push_str(&str1);
                actual_result_string.push_str(&str2);
            }

            let mut expected_factors_string = String::new();

            for (name, exp) in expected_factors
                .iter()
                .sorted_unstable_by_key(|entry| entry.0)
            {
                expected_factors_string.push_str(&format_factor(name, exp));
            }

            let mut actual_factors_string = String::new();

            for (name, exp) in actual_factors
                .iter()
                .sorted_unstable_by_key(|entry| entry.0)
            {
                actual_factors_string.push_str(&format_factor(name, exp));
            }

            expected_result_string.push_str(&format!(
                "{expected_factors_string: <width$}",
                width = expected_factors_string.width() + actual_factors_string.width()
            ));
            actual_result_string.push_str(&" ".repeat(expected_factors_string.width()));
            actual_result_string.push_str(&actual_factors_string);

            (expected_result_string, actual_result_string)
        };

        if !self.expected_dimensions.is_empty() {
            expected_result_string
                .push_str(&format!("    [= {}]", self.expected_dimensions.join(", ")));
        }

        if !self.actual_dimensions.is_empty() {
            actual_result_string
                .push_str(&format!("    [= {}]", self.actual_dimensions.join(", ")));
        }

        write!(
            f,
            "{}: {}",
            self.expected_name,
            expected_result_string.trim_start_matches(" × ").trim_end(),
        )?;

        write!(
            f,
            "\n{}: {}",
            self.actual_name,
            actual_result_string.trim_start_matches(" × ").trim_end(),
        )?;

        if let Some(fix) = suggested_fix(
            &self.expected_type,
            &self.actual_type,
            self.actual_name_for_fix,
        ) {
            write!(f, "\n\nSuggested fix: {fix}")?;
        }

        Ok(())
    }
}

impl Error for IncompatibleDimensionsError {}
