use crate::{arithmetic::Exponent, number::Number, pretty_print::PrettyPrint};

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
    fn pretty_print(&self) -> String {
        use BinaryOperator::*;

        match self {
            Add => "+".into(),
            Sub => "-".into(),
            Mul => "×".into(),
            Div => "/".into(),
            Power => "^".into(),
            ConvertTo => "→".into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Scalar(Number),
    Identifier(String),
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
    fn pretty_print(&self) -> String {
        use Expression::*;

        match self {
            Scalar(Number(n)) => format!("{n}"),
            Identifier(name) => name.clone(),
            Negate(rhs) => format!("-{rhs}", rhs = rhs.pretty_print()),
            BinaryOperator(op, lhs, rhs) => format!(
                "({lhs} {op} {rhs})",
                lhs = lhs.pretty_print(),
                op = op.pretty_print(),
                rhs = rhs.pretty_print()
            ),
            FunctionCall(name, args) => format!(
                "{name}({args})",
                name = name,
                args = args
                    .iter()
                    .map(|e| e.pretty_print())
                    .collect::<Vec<_>>()
                    .join(",")
            ),
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

    assert_eq!(expr.pretty_print(), "(2.0 × (-3.0 + 4.0))");
}

#[derive(Debug, Clone, PartialEq)]

pub enum DimensionExpression {
    Dimension(String),
    Multiply(Box<DimensionExpression>, Box<DimensionExpression>),
    Divide(Box<DimensionExpression>, Box<DimensionExpression>),
    Power(Box<DimensionExpression>, Exponent),
}

impl PrettyPrint for DimensionExpression {
    fn pretty_print(&self) -> String {
        match self {
            DimensionExpression::Dimension(ident) => ident.clone(),
            DimensionExpression::Multiply(lhs, rhs) => {
                format!("{} × {}", lhs.pretty_print(), rhs.pretty_print())
            }
            DimensionExpression::Divide(lhs, rhs) => {
                format!("{} / ({})", lhs.pretty_print(), rhs.pretty_print())
            }
            DimensionExpression::Power(dexpr, exp) => format!("({})^{}", dexpr.pretty_print(), exp),
        }
    }
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
        /// Function body
        Expression,
        /// Return type, optionally with a type annotation
        Option<DimensionExpression>,
    ),
    DeclareDimension(String, Vec<DimensionExpression>),
    DeclareBaseUnit(String, DimensionExpression),
    DeclareDerivedUnit(String, Expression, Option<DimensionExpression>),
}

impl PrettyPrint for Statement {
    fn pretty_print(&self) -> String {
        match self {
            Statement::DeclareVariable(identifier, expr, dexpr) => {
                format!(
                    "let {}{} = {}",
                    identifier,
                    if let Some(dexpr) = dexpr {
                        format!(": {}", dexpr.pretty_print())
                    } else {
                        "".into()
                    },
                    expr.pretty_print()
                )
            }
            Statement::DeclareFunction(identifier, _type_variables, _parameters, expr, _dexpr) => {
                // TODO(minor): print args
                format!("fn {}(…) = {}", identifier, expr.pretty_print())
            }
            Statement::Expression(expr) => expr.pretty_print(),
            Statement::DeclareDimension(ident, vec) if vec.is_empty() => {
                format!("dimension {}", ident)
            }
            Statement::DeclareDimension(ident, dexpr) => {
                format!("dimension {} = {}", ident, dexpr[0].pretty_print())
            }
            Statement::DeclareBaseUnit(ident, dexpr) => {
                format!("unit {}: {}", ident, dexpr.pretty_print())
            }
            Statement::DeclareDerivedUnit(ident, expr, dexpr) => {
                format!(
                    "unit {}: {} = {}",
                    ident,
                    if let Some(dexpr) = dexpr {
                        format!(": {}", dexpr.pretty_print())
                    } else {
                        "".into()
                    },
                    expr.pretty_print()
                )
            }
        }
    }
}
