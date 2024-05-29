use crate::arithmetic::{Exponent, Rational};
use crate::{ast, typed_ast};

use num_traits::{CheckedAdd, CheckedDiv, CheckedMul, CheckedSub, FromPrimitive, Zero};

use super::{error::Result, TypeCheckError};

fn to_rational_exponent(exponent_f64: f64) -> Option<Exponent> {
    Rational::from_f64(exponent_f64)
}

/// Evaluates a limited set of expressions *at compile time*. This is needed to
/// support type checking of expressions like `(2 * meter)^(2*3 - 4)` where we
/// need to know not just the *type* but also the *value* of the exponent.
pub fn evaluate_const_expr(expr: &typed_ast::Expression) -> Result<Exponent> {
    match expr {
        typed_ast::Expression::Scalar(span, n, _type) => {
            Ok(to_rational_exponent(n.to_f64())
                .ok_or(TypeCheckError::NonRationalExponent(*span))?)
        }
        typed_ast::Expression::UnaryOperator(_, ast::UnaryOperator::Negate, ref expr, _) => {
            Ok(-evaluate_const_expr(expr)?)
        }
        e @ typed_ast::Expression::UnaryOperator(_, ast::UnaryOperator::Factorial, _, _) => Err(
            TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "factorial"),
        ),
        e @ typed_ast::Expression::UnaryOperator(_, ast::UnaryOperator::LogicalNeg, _, _) => Err(
            TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "logical"),
        ),
        e @ typed_ast::Expression::BinaryOperator(_span_op, op, lhs_expr, rhs_expr, _) => {
            let lhs = evaluate_const_expr(lhs_expr)?;
            let rhs = evaluate_const_expr(rhs_expr)?;
            match op {
                typed_ast::BinaryOperator::Add => Ok(lhs
                    .checked_add(&rhs)
                    .ok_or_else(|| TypeCheckError::OverflowInConstExpr(expr.full_span()))?),
                typed_ast::BinaryOperator::Sub => Ok(lhs
                    .checked_sub(&rhs)
                    .ok_or_else(|| TypeCheckError::OverflowInConstExpr(expr.full_span()))?),
                typed_ast::BinaryOperator::Mul => Ok(lhs
                    .checked_mul(&rhs)
                    .ok_or_else(|| TypeCheckError::OverflowInConstExpr(expr.full_span()))?),
                typed_ast::BinaryOperator::Div => {
                    if rhs == Rational::zero() {
                        Err(TypeCheckError::DivisionByZeroInConstEvalExpression(
                            e.full_span(),
                        ))
                    } else {
                        Ok(lhs
                            .checked_div(&rhs)
                            .ok_or_else(|| TypeCheckError::OverflowInConstExpr(expr.full_span()))?)
                    }
                }
                typed_ast::BinaryOperator::Power => {
                    if rhs.is_integer() {
                        Ok(num_traits::checked_pow(
                            lhs,
                            rhs.to_integer().try_into().map_err(|_| {
                                TypeCheckError::OverflowInConstExpr(expr.full_span())
                            })?,
                        )
                        .ok_or_else(|| TypeCheckError::OverflowInConstExpr(expr.full_span()))?)
                    } else {
                        Err(TypeCheckError::UnsupportedConstEvalExpression(
                            e.full_span(),
                            "exponentiation with non-integer exponent",
                        ))
                    }
                }
                typed_ast::BinaryOperator::ConvertTo => Err(
                    TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "conversion"),
                ),
                typed_ast::BinaryOperator::LessThan
                | typed_ast::BinaryOperator::GreaterThan
                | typed_ast::BinaryOperator::LessOrEqual
                | typed_ast::BinaryOperator::GreaterOrEqual
                | typed_ast::BinaryOperator::Equal
                | typed_ast::BinaryOperator::NotEqual => Err(
                    TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "comparison"),
                ),
                typed_ast::BinaryOperator::LogicalAnd | typed_ast::BinaryOperator::LogicalOr => {
                    Err(TypeCheckError::UnsupportedConstEvalExpression(
                        e.full_span(),
                        "logical",
                    ))
                }
            }
        }
        e @ typed_ast::Expression::Identifier(..) => Err(
            TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "variable"),
        ),
        e @ typed_ast::Expression::UnitIdentifier(..) => Err(
            TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "unit identifier"),
        ),
        e @ typed_ast::Expression::FunctionCall(_, _, _, _, _) => Err(
            TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "function call"),
        ),
        e @ &typed_ast::Expression::CallableCall(_, _, _, _) => Err(
            TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "function call"),
        ),
        e @ typed_ast::Expression::Boolean(_, _) => Err(
            TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "Boolean value"),
        ),
        e @ typed_ast::Expression::String(_, _) => Err(
            TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "String"),
        ),
        e @ typed_ast::Expression::Condition(..) => Err(
            TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "Conditional"),
        ),
        e @ typed_ast::Expression::BinaryOperatorForDate(..) => {
            Err(TypeCheckError::UnsupportedConstEvalExpression(
                e.full_span(),
                "binary operator for datetimes",
            ))
        }
        e @ typed_ast::Expression::InstantiateStruct(_, _, _) => Err(
            TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "instantiate struct"),
        ),
        e @ typed_ast::Expression::AccessField(_, _, _, _, _, _) => Err(
            TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "access field of struct"),
        ),
        e @ typed_ast::Expression::List(_, _, _) => Err(
            TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "lists"),
        ),
    }
}
