pub use crate::ast::{BinaryOperator, DimensionExpression};
use crate::{
    decorator::Decorator, number::Number, prefix::Prefix, registry::BaseRepresentation, span::Span,
};

pub type Type = BaseRepresentation;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Scalar(Span, Number),
    Identifier(Span, String, Type),
    UnitIdentifier(Span, Prefix, String, String, Type),
    Negate(Span, Box<Expression>, Type),
    BinaryOperator(
        Option<Span>,
        BinaryOperator,
        Box<Expression>,
        Box<Expression>,
        Type,
    ),
    FunctionCall(Span, Span, String, Vec<Expression>, Type),
}

impl Expression {
    pub fn full_span(&self) -> Span {
        match self {
            Expression::Scalar(span, ..) => *span,
            Expression::Identifier(span, ..) => *span,
            Expression::UnitIdentifier(span, ..) => *span,
            Expression::Negate(span, expr, _) => span.extend(&expr.full_span()),
            Expression::BinaryOperator(span_op, _op, lhs, rhs, _) => {
                let mut span = lhs.full_span().extend(&rhs.full_span());
                if let Some(span_op) = span_op {
                    span = span.extend(&span_op);
                }
                span
            }
            Expression::FunctionCall(_identifier_span, full_span, _, _, _) => *full_span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expression(Expression),
    DefineVariable(String, Expression, Type),
    DefineFunction(
        String,
        Vec<(Span, String, bool, Type)>,
        Option<Expression>,
        Type,
    ),
    DefineDimension(String),
    DefineBaseUnit(String, Vec<Decorator>, Type),
    DefineDerivedUnit(String, Expression, Vec<Decorator>),
    ProcedureCall(crate::ast::ProcedureKind, Vec<Expression>),
}

impl Expression {
    pub(crate) fn get_type(&self) -> Type {
        match self {
            Expression::Scalar(_, _) => Type::unity(),
            Expression::Identifier(_, _, type_) => type_.clone(),
            Expression::UnitIdentifier(_, _, _, _, _type) => _type.clone(),
            Expression::Negate(_, _, type_) => type_.clone(),
            Expression::BinaryOperator(_, _, _, _, type_) => type_.clone(),
            Expression::FunctionCall(_, _, _, _, type_) => type_.clone(),
        }
    }
}
