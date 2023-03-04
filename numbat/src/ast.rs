use crate::markup::{self as m, FormattedString};
use crate::{
    arithmetic::Exponent, decorator::Decorator, markup::Markup, number::Number, prefix::Prefix,
    pretty_print::PrettyPrint,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Power,
    ConvertTo,
}

impl PrettyPrint for BinaryOperator {
    fn pretty_print(&self) -> Markup {
        use BinaryOperator::*;

        m::operator(match self {
            Add => "+",
            Sub => "-",
            Mul => "×",
            Div => "/",
            Power => "^",
            ConvertTo => "→",
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Scalar(Number),
    Identifier(String),
    UnitIdentifier(Prefix, String),
    Negate(Box<Expression>),
    BinaryOperator(BinaryOperator, Box<Expression>, Box<Expression>),
    FunctionCall(String, Vec<Expression>),
}

#[cfg(test)]
macro_rules! scalar {
    ( $num:expr ) => {{
        Expression::Scalar(Number::from_f64($num))
    }};
}

#[cfg(test)]
macro_rules! identifier {
    ( $name:expr ) => {{
        Expression::Identifier($name.into())
    }};
}

#[cfg(test)]
macro_rules! negate {
    ( $rhs:expr ) => {{
        Expression::Negate(Box::new($rhs))
    }};
}

#[cfg(test)]
macro_rules! binop {
    ( $lhs:expr, $op:ident, $rhs: expr ) => {{
        Expression::BinaryOperator(BinaryOperator::$op, Box::new($lhs), Box::new($rhs))
    }};
}
#[cfg(test)]
pub(crate) use binop;
#[cfg(test)]
pub(crate) use identifier;
#[cfg(test)]
pub(crate) use negate;
#[cfg(test)]
pub(crate) use scalar;

impl PrettyPrint for Expression {
    fn pretty_print(&self) -> Markup {
        use Expression::*;

        match self {
            Scalar(Number(n)) => m::value(&format!("{n}")),
            Identifier(name) => m::identifier(name),
            UnitIdentifier(prefix, name) => m::unit(format!("{}{}", prefix, name)),
            Negate(rhs) => m::operator("-") + rhs.pretty_print(),
            BinaryOperator(op, lhs, rhs) => {
                m::operator("(")
                    + lhs.pretty_print()
                    + m::text(" ")
                    + op.pretty_print()
                    + m::text(" ")
                    + rhs.pretty_print()
                    + m::operator(")")
            }
            FunctionCall(name, args) => {
                m::identifier(name)
                    + m::operator("(")
                    + Markup(
                        itertools::Itertools::intersperse(
                            args.iter().flat_map(|e| e.pretty_print().0),
                            FormattedString(
                                m::OutputType::Normal,
                                m::FormatType::Operator,
                                ",".into(),
                            ),
                        )
                        .collect(),
                    )
                    + m::operator(")")
            }
        }
    }
}

#[test]
fn expression_pretty_print() {
    let expr = binop!(
        scalar!(2.0),
        Mul,
        binop!(negate!(scalar!(3.0)), Add, scalar!(4.0))
    );

    assert_eq!(expr.pretty_print().to_string(), "(2 × (-3 + 4))");
}

#[derive(Debug, Clone, PartialEq)]

pub enum DimensionExpression {
    Unity,
    Dimension(String),
    Multiply(Box<DimensionExpression>, Box<DimensionExpression>),
    Divide(Box<DimensionExpression>, Box<DimensionExpression>),
    Power(Box<DimensionExpression>, Exponent),
}

impl PrettyPrint for DimensionExpression {
    fn pretty_print(&self) -> Markup {
        match self {
            DimensionExpression::Unity => m::type_identifier("1"),
            DimensionExpression::Dimension(ident) => m::type_identifier(ident),
            DimensionExpression::Multiply(lhs, rhs) => {
                lhs.pretty_print()
                    + m::text(" ")
                    + m::operator("×")
                    + m::text(" ")
                    + rhs.pretty_print()
            }
            DimensionExpression::Divide(lhs, rhs) => {
                lhs.pretty_print()
                    + m::text(" ")
                    + m::operator("/")
                    + m::text(" ")
                    + rhs.pretty_print()
            }
            DimensionExpression::Power(lhs, exp) => {
                m::operator("(")
                    + lhs.pretty_print()
                    + m::operator(")")
                    + m::operator("^")
                    + m::value(format!("{exp}"))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ProcedureKind {
    Print,
    AssertEq,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expression(Expression),
    DeclareVariable(String, Expression, Option<DimensionExpression>),
    DeclareFunction(
        /// Function name
        String,
        /// Introduced type parameters
        Vec<String>,
        /// Arguments, optionally with type annotations
        Vec<(String, Option<DimensionExpression>)>,
        /// Function body. If it is absent, the function is implemented via FFI
        Option<Expression>,
        /// Optional annotated return type
        Option<DimensionExpression>,
    ),
    DeclareDimension(String, Vec<DimensionExpression>),
    DeclareBaseUnit(String, DimensionExpression, Vec<Decorator>),
    DeclareDerivedUnit(
        String,
        Expression,
        Option<DimensionExpression>,
        Vec<Decorator>,
    ),
    ProcedureCall(ProcedureKind, Vec<Expression>),
}

impl PrettyPrint for Statement {
    fn pretty_print(&self) -> Markup {
        match self {
            Statement::DeclareVariable(identifier, expr, _dexpr) => {
                m::keyword("let")
                    + m::text(" ")
                    + m::identifier(identifier)
                    + m::text(" ")
                    + m::operator("=")
                    + m::text(" ")
                    + expr.pretty_print() // TODO: add type information
            }
            Statement::DeclareFunction(identifier, _type_variables, _parameters, body, _dexpr) => {
                m::keyword("fn")
                    + m::text(" ")
                    + m::identifier(identifier)
                    + m::operator("(")
                    + m::text("…")
                    + m::operator(")")
                    + m::text(" ")
                    + m::operator("=")
                    + m::text(" ")
                    + body.as_ref().map(|e| e.pretty_print()).unwrap_or_default()
                // TODO: add type information, parameters, etc.
            }
            Statement::Expression(expr) => expr.pretty_print(),
            Statement::DeclareDimension(identifier, dexprs) if dexprs.is_empty() => {
                m::keyword("dimension") + m::text(" ") + m::type_identifier(identifier)
            }
            Statement::DeclareDimension(identifier, dexprs) => {
                m::keyword("dimension")
                    + m::text(" ")
                    + m::type_identifier(identifier)
                    + m::text(" ")
                    + m::operator("=")
                    + m::text(" ")
                    + dexprs[0].pretty_print() // TODO: print all dexprs
            }
            Statement::DeclareBaseUnit(identifier, dexpr, _decorators) => {
                m::keyword("unit")
                    + m::text(" ")
                    + m::unit(identifier)
                    + m::operator(":")
                    + m::text(" ")
                    + dexpr.pretty_print()
            }
            Statement::DeclareDerivedUnit(identifier, expr, dexpr, _decorators) => {
                m::keyword("unit")
                    + m::text(" ")
                    + m::unit(identifier)
                    + (if let Some(dexpr) = dexpr {
                        m::operator(":") + m::text(" ") + dexpr.pretty_print()
                    } else {
                        Markup::default()
                    })
                    + m::text(" ")
                    + m::operator("=")
                    + m::text(" ")
                    + expr.pretty_print()
            }
            Statement::ProcedureCall(_kind, _args) => m::text("TODO"),
        }
    }
}
