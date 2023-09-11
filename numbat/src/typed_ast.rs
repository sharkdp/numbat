pub use crate::ast::{BinaryOperator, DimensionExpression, UnaryOperator};
use crate::{
    decorator::Decorator, number::Number, prefix::Prefix, registry::BaseRepresentation, span::Span,
};

/// Dimension type
pub type DType = BaseRepresentation;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Dimension(DType),
    Boolean,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Dimension(d) => d.fmt(f),
            Type::Boolean => write!(f, "bool"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Scalar(Span, Number),
    Identifier(Span, String, Type),
    UnitIdentifier(Span, Prefix, String, String, Type),
    UnaryOperator(Span, UnaryOperator, Box<Expression>, Type),
    BinaryOperator(
        Option<Span>,
        BinaryOperator,
        Box<Expression>,
        Box<Expression>,
        Type,
    ),
    FunctionCall(Span, Span, String, Vec<Expression>, DType),
    Boolean(Span, bool),
    Condition(Span, Box<Expression>, Box<Expression>, Box<Expression>),
}

impl Expression {
    pub fn full_span(&self) -> Span {
        match self {
            Expression::Scalar(span, ..) => *span,
            Expression::Identifier(span, ..) => *span,
            Expression::UnitIdentifier(span, ..) => *span,
            Expression::UnaryOperator(span, _, expr, _) => span.extend(&expr.full_span()),
            Expression::BinaryOperator(span_op, _op, lhs, rhs, _) => {
                let mut span = lhs.full_span().extend(&rhs.full_span());
                if let Some(span_op) = span_op {
                    span = span.extend(span_op);
                }
                span
            }
            Expression::FunctionCall(_identifier_span, full_span, _, _, _) => *full_span,
            Expression::Boolean(span, _) => *span,
            Expression::Condition(span_if, _, _, then_expr) => {
                span_if.extend(&then_expr.full_span())
            }
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
            Expression::Scalar(_, _) => Type::Dimension(DType::unity()),
            Expression::Identifier(_, _, type_) => type_.clone(),
            Expression::UnitIdentifier(_, _, _, _, _type) => _type.clone(),
            Expression::UnaryOperator(_, _, _, type_) => type_.clone(),
            Expression::BinaryOperator(_, _, _, _, type_) => type_.clone(),
            Expression::FunctionCall(_, _, _, _, type_) => Type::Dimension(type_.clone()),
            Expression::Boolean(_, _) => Type::Boolean,
            Expression::Condition(_, _, then, _) => then.get_type(),
        }
    }
}
