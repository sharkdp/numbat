//! Parser for simple quantity literals.
//!
//! This module provides a restricted parser that only accepts simple quantity literals
//! of the form `<number>` or `<number> <unit>`, not arbitrary expressions.

use crate::ast::{BinaryOperator, Expression, Statement, UnaryOperator};
use crate::parser::{ParseError, parse};

/// Error type for parsing quantity literals
#[derive(Debug, Clone, PartialEq)]
pub enum QuantityLiteralError {
    /// Input could not be parsed at all
    ParseError(String),
    /// Input was parsed but doesn't match the expected `<number> [<unit>]` pattern
    InvalidPattern(String),
}

impl std::fmt::Display for QuantityLiteralError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            QuantityLiteralError::ParseError(msg) => write!(f, "Parse error: {}", msg),
            QuantityLiteralError::InvalidPattern(msg) => write!(f, "Invalid pattern: {}", msg),
        }
    }
}

/// Parse a quantity literal of the form `<number>` or `<number> <unit>`.
///
/// This is a restricted parser that only accepts simple quantity literals,
/// not arbitrary expressions. Returns the AST on success for further processing
/// by the type checker.
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
pub fn parse_quantity_literal(input: &str) -> Result<Statement<'_>, QuantityLiteralError> {
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

    let expr = match &statement {
        Statement::Expression(expr) => expr,
        _ => {
            return Err(QuantityLiteralError::InvalidPattern(
                "Expected an expression, not a declaration".to_string(),
            ));
        }
    };

    if !is_valid_quantity_literal(expr) {
        return Err(QuantityLiteralError::InvalidPattern(
            "Expected '<number>' or '<number> <unit>', got a more complex expression".to_string(),
        ));
    }

    Ok(statement)
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

    /// Pretty print statement with spans normalized for comparison
    fn pp(stmt: &Statement) -> String {
        format!("{:?}", stmt.replace_spans())
    }

    #[test]
    fn test_parse_scalar() {
        let stmt = parse_quantity_literal("1.5").unwrap();
        assert_snapshot!(pp(&stmt), @"Expression(Scalar(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }, Number(1.5)))");
    }

    #[test]
    fn test_parse_negative_scalar() {
        let stmt = parse_quantity_literal("-3.14").unwrap();
        assert_snapshot!(pp(&stmt), @"Expression(UnaryOperator { op: Negate, expr: Scalar(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }, Number(3.14)), span_op: Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 } })");
    }

    #[test]
    fn test_parse_scalar_with_unit() {
        let stmt = parse_quantity_literal("1.5 km").unwrap();
        assert_snapshot!(pp(&stmt), @r#"Expression(BinaryOperator { op: Mul, lhs: Scalar(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }, Number(1.5)), rhs: Identifier(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }, "km"), span_op: Some(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }) })"#);
    }

    #[test]
    fn test_parse_negative_scalar_with_unit() {
        let stmt = parse_quantity_literal("-100 m").unwrap();
        assert_snapshot!(pp(&stmt), @r#"Expression(UnaryOperator { op: Negate, expr: BinaryOperator { op: Mul, lhs: Scalar(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }, Number(100.0)), rhs: Identifier(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }, "m"), span_op: Some(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }) }, span_op: Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 } })"#);
    }

    #[test]
    fn test_parse_integer() {
        let stmt = parse_quantity_literal("42").unwrap();
        assert_snapshot!(pp(&stmt), @"Expression(Scalar(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }, Number(42.0)))");
    }

    #[test]
    fn test_parse_double_negation() {
        let stmt = parse_quantity_literal("--42").unwrap();
        assert_snapshot!(pp(&stmt), @"Expression(UnaryOperator { op: Negate, expr: UnaryOperator { op: Negate, expr: Scalar(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }, Number(42.0)), span_op: Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 } }, span_op: Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 } })");
    }

    #[test]
    fn test_parse_triple_negation_with_unit() {
        let stmt = parse_quantity_literal("---2 km").unwrap();
        assert_snapshot!(pp(&stmt), @r#"Expression(UnaryOperator { op: Negate, expr: UnaryOperator { op: Negate, expr: UnaryOperator { op: Negate, expr: BinaryOperator { op: Mul, lhs: Scalar(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }, Number(2.0)), rhs: Identifier(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }, "km"), span_op: Some(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }) }, span_op: Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 } }, span_op: Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 } }, span_op: Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 } })"#);
    }

    #[test]
    fn test_reject_addition() {
        let result = parse_quantity_literal("1 + 2");
        assert!(matches!(
            result,
            Err(QuantityLiteralError::InvalidPattern(_))
        ));
    }

    #[test]
    fn test_reject_compound_units() {
        let result = parse_quantity_literal("100 km/h");
        assert!(matches!(
            result,
            Err(QuantityLiteralError::InvalidPattern(_))
        ));
    }

    #[test]
    fn test_reject_variable() {
        let result = parse_quantity_literal("x");
        assert!(matches!(
            result,
            Err(QuantityLiteralError::InvalidPattern(_))
        ));
    }

    #[test]
    fn test_reject_empty() {
        let result = parse_quantity_literal("");
        assert!(matches!(result, Err(QuantityLiteralError::ParseError(_))));

        let result = parse_quantity_literal("   ");
        assert!(matches!(result, Err(QuantityLiteralError::ParseError(_))));
    }

    #[test]
    fn test_parse_explicit_multiplication() {
        let stmt = parse_quantity_literal("1.5 * km").unwrap();
        assert_snapshot!(pp(&stmt), @r#"Expression(BinaryOperator { op: Mul, lhs: Scalar(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }, Number(1.5)), rhs: Identifier(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }, "km"), span_op: Some(Span { start: ByteIndex(0), end: ByteIndex(0), code_source_id: 0 }) })"#);
    }

    #[test]
    fn test_reject_scalar_times_scalar() {
        let result = parse_quantity_literal("2 * 3");
        assert!(matches!(
            result,
            Err(QuantityLiteralError::InvalidPattern(_))
        ));
    }

    #[test]
    fn test_reject_unit_squared() {
        let result = parse_quantity_literal("1 m^2");
        assert!(matches!(
            result,
            Err(QuantityLiteralError::InvalidPattern(_))
        ));
    }
}
