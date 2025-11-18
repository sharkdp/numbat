use crate::arithmetic::{Exponent, Rational};
use crate::{ast, typed_ast};

use num_traits::{CheckedAdd, CheckedDiv, CheckedMul, CheckedSub, FromPrimitive, Zero};

use super::{TypeCheckError, error::Result};

fn to_rational_exponent(exponent_f64: f64) -> Option<Exponent> {
    Rational::from_f64(exponent_f64)
}

/// Evaluates a limited set of expressions *at compile time*. This is needed to
/// support type checking of expressions like `(2 * meter)^(2*3 - 4)` where we
/// need to know not just the *type* but also the *value* of the exponent.
pub fn evaluate_const_expr(expr: &typed_ast::Expression) -> Result<Exponent> {
    let name = match expr {
        typed_ast::Expression::Scalar(span, n, _type) => {
            return Ok(to_rational_exponent(n.to_f64())
                .ok_or(TypeCheckError::NonRationalExponent(*span))?);
        }
        typed_ast::Expression::UnaryOperator(_, ast::UnaryOperator::Negate, expr, _) => {
            return Ok(-evaluate_const_expr(expr)?);
        }
        typed_ast::Expression::UnaryOperator(_, ast::UnaryOperator::Factorial(_order), _, _) => {
            "factorial"
        }
        typed_ast::Expression::UnaryOperator(_, ast::UnaryOperator::LogicalNeg, _, _) => "logical",

        e @ typed_ast::Expression::BinaryOperator(_span_op, op, lhs_expr, rhs_expr, _) => {
            let lhs = evaluate_const_expr(lhs_expr)?;
            let rhs = evaluate_const_expr(rhs_expr)?;
            return match op {
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
                        Err(Box::new(
                            TypeCheckError::DivisionByZeroInConstEvalExpression(e.full_span()),
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
                        Err(Box::new(TypeCheckError::UnsupportedConstEvalExpression(
                            e.full_span(),
                            "exponentiation with non-integer exponent",
                        )))
                    }
                }
                typed_ast::BinaryOperator::ConvertTo => Err(Box::new(
                    TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "conversion"),
                )),
                typed_ast::BinaryOperator::LessThan
                | typed_ast::BinaryOperator::GreaterThan
                | typed_ast::BinaryOperator::LessOrEqual
                | typed_ast::BinaryOperator::GreaterOrEqual
                | typed_ast::BinaryOperator::Equal
                | typed_ast::BinaryOperator::NotEqual => Err(Box::new(
                    TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "comparison"),
                )),
                typed_ast::BinaryOperator::LogicalAnd | typed_ast::BinaryOperator::LogicalOr => {
                    Err(Box::new(TypeCheckError::UnsupportedConstEvalExpression(
                        e.full_span(),
                        "logical",
                    )))
                }
            };
        }
        typed_ast::Expression::Identifier(..) => "variable",
        typed_ast::Expression::UnitIdentifier(..) => "unit identifier",
        typed_ast::Expression::FunctionCall(_, _, _, _, _) => "function call",
        typed_ast::Expression::CallableCall(_, _, _, _) => "function call",
        typed_ast::Expression::Boolean(_, _) => "Boolean value",
        typed_ast::Expression::String(_, _) => "String",
        typed_ast::Expression::Condition(..) => "Conditional",
        typed_ast::Expression::BinaryOperatorForDate(..) => "binary operator for datetimes",
        typed_ast::Expression::InstantiateStruct(_, _, _) => "instantiate struct",
        typed_ast::Expression::AccessField(_, _, _, _, _, _) => "access field of struct",
        typed_ast::Expression::List(_, _, _) => "lists",
        typed_ast::Expression::TypedHole(_, _) => "typed hole",
    };

    Err(Box::new(TypeCheckError::UnsupportedConstEvalExpression(
        expr.full_span(),
        name,
    )))
}
