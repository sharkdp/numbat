//! Parser for simple quantity literals.
//!
//! This module provides a restricted parser that only accepts simple quantity literals
//! of the form `<number>` or `<number> <unit>`, not arbitrary expressions.

use crate::ast::{BinaryOperator, Expression, Statement, UnaryOperator};
use crate::number::Number;
use crate::parser::{ParseError, parse};
use crate::prefix_transformer::Transformer;
use crate::quantity::Quantity;
use crate::typechecker::TypeChecker;
use crate::typechecker::type_scheme::TypeScheme;
use crate::unit::Unit;
use crate::Type;

/// Error type for parsing quantity literals
#[derive(Debug, Clone, PartialEq)]
pub enum QuantityLiteralError {
    /// Input could not be parsed at all
    ParseError(String),
    /// Input was parsed but doesn't match the expected `<number> [<unit>]` pattern
    InvalidPattern(String),
    /// Name resolution error (unknown unit)
    NameResolutionError(String),
}

impl std::fmt::Display for QuantityLiteralError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            QuantityLiteralError::ParseError(msg) => write!(f, "Parse error: {msg}"),
            QuantityLiteralError::InvalidPattern(msg) => write!(f, "Invalid pattern: {msg}"),
            QuantityLiteralError::NameResolutionError(msg) => {
                write!(f, "Name resolution error: {msg}")
            }
        }
    }
}

/// Parse and evaluate a quantity literal, returning a Quantity and its TypeScheme.
///
/// This runs the full pipeline: parsing → transforming → type inference → evaluation.
///
/// Valid examples:
/// - `1.5`
/// - `-3.14`
/// - `1.5 km`
/// - `1.5 * km`
/// - `100 m`
pub fn parse_quantity_literal(
    input: &str,
    transformer: &Transformer,
    typechecker: &TypeChecker,
    unit_lookup: impl Fn(&str) -> Option<Unit>,
) -> Result<(Quantity, TypeScheme), QuantityLiteralError> {
    // Step 1: Parse and validate the AST
    let mut expr = parse_quantity_ast(input)?;

    // Step 2: Transform (resolve unit names)
    transformer.transform_expression(&mut expr);

    // Step 3: Infer the type of the expression
    let type_scheme = get_expression_type(&expr, typechecker)?;

    // Step 4: Evaluate
    let quantity = evaluate_quantity_expression(&expr, &unit_lookup)?;

    Ok((quantity, type_scheme))
}

/// Infer the type of a quantity literal expression.
///
/// For scalars, returns the scalar (dimensionless) type.
/// For scalar × unit, returns the unit's type scheme.
fn get_expression_type(
    expr: &Expression,
    typechecker: &TypeChecker,
) -> Result<TypeScheme, QuantityLiteralError> {
    match expr {
        // Plain scalar - dimensionless
        Expression::Scalar(_, _) => Ok(TypeScheme::concrete(Type::scalar())),

        // Scalar × Unit - get type from unit
        Expression::BinaryOperator {
            op: BinaryOperator::Mul,
            rhs,
            ..
        } => {
            if let Expression::UnitIdentifier { name, .. } = rhs.as_ref() {
                typechecker.lookup_identifier_type(name).ok_or_else(|| {
                    QuantityLiteralError::NameResolutionError(format!("Unknown unit: {name}"))
                })
            } else {
                Err(QuantityLiteralError::InvalidPattern(
                    "Expected unit identifier".to_string(),
                ))
            }
        }

        // Negation - recurse
        Expression::UnaryOperator {
            op: UnaryOperator::Negate,
            expr: inner,
            ..
        } => get_expression_type(inner, typechecker),

        _ => Err(QuantityLiteralError::InvalidPattern(
            "Unexpected expression type".to_string(),
        )),
    }
}

/// Evaluate a quantity literal expression to produce a Quantity.
fn evaluate_quantity_expression(
    expr: &Expression,
    unit_lookup: &impl Fn(&str) -> Option<Unit>,
) -> Result<Quantity, QuantityLiteralError> {
    match expr {
        // Plain scalar
        Expression::Scalar(_, n) => Ok(Quantity::from_scalar(n.to_f64())),

        // Scalar × Unit
        Expression::BinaryOperator {
            op: BinaryOperator::Mul,
            lhs,
            rhs,
            ..
        } => {
            let scalar = extract_scalar(lhs)?;
            let unit = extract_unit(rhs, unit_lookup)?;
            Ok(Quantity::new(Number::from_f64(scalar), unit))
        }

        // Negation
        Expression::UnaryOperator {
            op: UnaryOperator::Negate,
            expr: inner,
            ..
        } => {
            let quantity = evaluate_quantity_expression(inner, unit_lookup)?;
            Ok(-quantity)
        }

        _ => Err(QuantityLiteralError::InvalidPattern(
            "Unexpected expression type".to_string(),
        )),
    }
}

/// Extract the scalar value from a Scalar expression.
fn extract_scalar(expr: &Expression) -> Result<f64, QuantityLiteralError> {
    match expr {
        Expression::Scalar(_, n) => Ok(n.to_f64()),
        _ => Err(QuantityLiteralError::InvalidPattern(
            "Expected scalar".to_string(),
        )),
    }
}

/// Extract the Unit from a UnitIdentifier expression.
fn extract_unit(
    expr: &Expression,
    unit_lookup: &impl Fn(&str) -> Option<Unit>,
) -> Result<Unit, QuantityLiteralError> {
    match expr {
        Expression::UnitIdentifier { prefix, name, .. } => {
            let base_unit = unit_lookup(name).ok_or_else(|| {
                QuantityLiteralError::NameResolutionError(format!("Unknown unit: {name}"))
            })?;
            Ok(base_unit.with_prefix(*prefix))
        }
        _ => Err(QuantityLiteralError::InvalidPattern(
            "Expected unit identifier".to_string(),
        )),
    }
}

/// Parse a quantity literal and return the expression AST.
///
/// This is a restricted parser that only accepts simple quantity literals,
/// not arbitrary expressions. Returns the expression on success.
///
/// Valid examples:
/// - `1.5`
/// - `-3.14`
/// - `1.5 km`
/// - `1.5 * km`
/// - `100 m`
///
/// Invalid examples (will return an error):
/// - `1 + 2`
/// - `1 km + 500 m`
/// - `100 km/h` (compound units not supported)
/// - `x` (variables not supported)
pub fn parse_quantity_ast(input: &str) -> Result<Expression<'_>, QuantityLiteralError> {
    let input = input.trim();
    if input.is_empty() {
        return Err(QuantityLiteralError::ParseError("Empty input".to_string()));
    }

    let mut statements = parse(input, 0).map_err(|(_stmts, errs): (_, Vec<ParseError>)| {
        let msg = errs
            .iter()
            .map(|e| format!("{:?}", e.kind))
            .collect::<Vec<_>>()
            .join(", ");
        QuantityLiteralError::ParseError(msg)
    })?;

    if statements.len() != 1 {
        return Err(QuantityLiteralError::InvalidPattern(format!(
            "Expected a single expression, got {} statements",
            statements.len()
        )));
    }

    let statement = statements.remove(0);

    let expr = match statement {
        Statement::Expression(expr) => expr,
        _ => {
            return Err(QuantityLiteralError::InvalidPattern(
                "Expected an expression, not a declaration".to_string(),
            ));
        }
    };

    if !is_valid_quantity_literal(&expr) {
        return Err(QuantityLiteralError::InvalidPattern(
            "Expected '<number>' or '<number> <unit>', got a more complex expression".to_string(),
        ));
    }

    Ok(expr)
}

fn is_valid_quantity_literal(expr: &Expression) -> bool {
    match expr {
        Expression::Scalar(_, _) => true,

        // Scalar times unit (implicit or explicit multiplication)
        Expression::BinaryOperator {
            op: BinaryOperator::Mul,
            lhs,
            rhs,
            ..
        } if lhs.is_scalar() && rhs.is_identifier() => true,

        // Negation of a valid quantity literal
        Expression::UnaryOperator {
            op: UnaryOperator::Negate,
            expr: inner,
            ..
        } => is_valid_quantity_literal(inner),

        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::ReplaceSpans;
    use insta::assert_snapshot;

    /// Pretty print expression with spans normalized for comparison
    fn pp(expr: &Expression) -> String {
        format!("{:?}", expr.replace_spans())
    }

    #[test]
    fn test_parse_scalar() {
        let expr = parse_quantity_ast("1.5").unwrap();
        assert_snapshot!(pp(&expr), @"Scalar(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }, Number(1.5))");
    }

    #[test]
    fn test_parse_negative_scalar() {
        let expr = parse_quantity_ast("-3.14").unwrap();
        assert_snapshot!(pp(&expr), @"UnaryOperator { op: Negate, expr: Scalar(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }, Number(3.14)), span_op: Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 } }");
    }

    #[test]
    fn test_parse_scalar_with_unit() {
        let expr = parse_quantity_ast("1.5 km").unwrap();
        assert_snapshot!(pp(&expr), @r#"BinaryOperator { op: Mul, lhs: Scalar(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }, Number(1.5)), rhs: Identifier(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }, "km"), span_op: Some(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }) }"#);
    }

    #[test]
    fn test_parse_negative_scalar_with_unit() {
        let expr = parse_quantity_ast("-100 m").unwrap();
        assert_snapshot!(pp(&expr), @r#"UnaryOperator { op: Negate, expr: BinaryOperator { op: Mul, lhs: Scalar(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }, Number(100.0)), rhs: Identifier(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }, "m"), span_op: Some(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }) }, span_op: Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 } }"#);
    }

    #[test]
    fn test_parse_integer() {
        let expr = parse_quantity_ast("42").unwrap();
        assert_snapshot!(pp(&expr), @"Scalar(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }, Number(42.0))");
    }

    #[test]
    fn test_parse_double_negation() {
        let expr = parse_quantity_ast("--42").unwrap();
        assert_snapshot!(pp(&expr), @"UnaryOperator { op: Negate, expr: UnaryOperator { op: Negate, expr: Scalar(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }, Number(42.0)), span_op: Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 } }, span_op: Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 } }");
    }

    #[test]
    fn test_parse_triple_negation_with_unit() {
        let expr = parse_quantity_ast("---2 km").unwrap();
        assert_snapshot!(pp(&expr), @r#"UnaryOperator { op: Negate, expr: UnaryOperator { op: Negate, expr: UnaryOperator { op: Negate, expr: BinaryOperator { op: Mul, lhs: Scalar(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }, Number(2.0)), rhs: Identifier(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }, "km"), span_op: Some(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }) }, span_op: Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 } }, span_op: Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 } }, span_op: Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 } }"#);
    }

    #[test]
    fn test_reject_addition() {
        let result = parse_quantity_ast("1 + 2");
        assert!(matches!(
            result,
            Err(QuantityLiteralError::InvalidPattern(_))
        ));
    }

    #[test]
    fn test_reject_compound_units() {
        let result = parse_quantity_ast("100 km/h");
        assert!(matches!(
            result,
            Err(QuantityLiteralError::InvalidPattern(_))
        ));
    }

    #[test]
    fn test_reject_variable() {
        let result = parse_quantity_ast("x");
        assert!(matches!(
            result,
            Err(QuantityLiteralError::InvalidPattern(_))
        ));
    }

    #[test]
    fn test_reject_empty() {
        let result = parse_quantity_ast("");
        assert!(matches!(result, Err(QuantityLiteralError::ParseError(_))));

        let result = parse_quantity_ast("   ");
        assert!(matches!(result, Err(QuantityLiteralError::ParseError(_))));
    }

    #[test]
    fn test_parse_explicit_multiplication() {
        let expr = parse_quantity_ast("1.5 * km").unwrap();
        assert_snapshot!(pp(&expr), @r#"BinaryOperator { op: Mul, lhs: Scalar(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }, Number(1.5)), rhs: Identifier(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }, "km"), span_op: Some(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }) }"#);
    }

    #[test]
    fn test_reject_scalar_times_scalar() {
        let result = parse_quantity_ast("2 * 3");
        assert!(matches!(
            result,
            Err(QuantityLiteralError::InvalidPattern(_))
        ));
    }

    #[test]
    fn test_reject_unit_squared() {
        let result = parse_quantity_ast("1 m^2");
        assert!(matches!(
            result,
            Err(QuantityLiteralError::InvalidPattern(_))
        ));
    }
}
