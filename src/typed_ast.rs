pub use crate::ast::{BinaryOperator, Command, DimensionExpression};
use crate::number::Number;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Scalar(Number),
    Identifier(String),
    Negate(Box<Expression>),
    BinaryOperator(BinaryOperator, Box<Expression>, Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Command(Command),
    DeclareVariable(String, Expression, Option<DimensionExpression>),
    Expression(Expression),
    DeclareDimension(String, Vec<DimensionExpression>),
    DeclareBaseUnit(String, DimensionExpression),
    DeclareDerivedUnit(String, Expression, Option<DimensionExpression>),
}
