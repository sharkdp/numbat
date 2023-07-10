pub use crate::ast::{BinaryOperator, DimensionExpression};
use crate::{
    decorator::Decorator, number::Number, prefix::Prefix, registry::BaseRepresentation, span::Span,
};

pub type Type = BaseRepresentation;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Scalar(Number),
    Identifier(Span, String, Type),
    UnitIdentifier(Span, Prefix, String, String, Type),
    Negate(Span, Box<Expression>, Type),
    BinaryOperator(Span, BinaryOperator, Box<Expression>, Box<Expression>, Type),
    FunctionCall(Span, String, Vec<Expression>, Type),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expression(Expression),
    DeclareVariable(String, Expression, Type),
    DeclareFunction(
        String,
        Vec<(Span, String, bool, Type)>,
        Option<Expression>,
        Type,
    ),
    DeclareDimension(String),
    DeclareBaseUnit(String, Vec<Decorator>, Type),
    DeclareDerivedUnit(String, Expression, Vec<Decorator>),
    ProcedureCall(crate::ast::ProcedureKind, Vec<Expression>),
}

impl Expression {
    pub(crate) fn get_type(&self) -> Type {
        match self {
            Expression::Scalar(_) => Type::unity(),
            Expression::Identifier(_, _, type_) => type_.clone(),
            Expression::UnitIdentifier(_, _, _, _, _type) => _type.clone(),
            Expression::Negate(_, _, type_) => type_.clone(),
            Expression::BinaryOperator(_, _, _, _, type_) => type_.clone(),
            Expression::FunctionCall(_, _, _, type_) => type_.clone(),
        }
    }
}
