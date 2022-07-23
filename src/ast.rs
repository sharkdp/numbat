use crate::pretty_print::PrettyPrint;

#[derive(Debug, Clone, PartialEq)]
pub struct Number(f64);

impl Number {
    pub fn from_f64(n: f64) -> Self {
        Number(n)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
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
            ConvertTo => "→".into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Scalar(Number),
    Negate(Box<Expression>),
    BinaryOperator(BinaryOperator, Box<Expression>, Box<Expression>),
}

#[cfg(test)]
macro_rules! scalar {
    ( $num:expr ) => {{
        Expression::Scalar(Number::from_f64($num))
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
pub(crate) use negate;
#[cfg(test)]
pub(crate) use scalar;

impl PrettyPrint for Expression {
    fn pretty_print(&self) -> String {
        use Expression::*;

        match self {
            Scalar(Number(n)) => format!("{n:.1}"),
            Negate(rhs) => format!("-{rhs}", rhs = rhs.pretty_print()),
            BinaryOperator(op, lhs, rhs) => format!(
                "({lhs} {op} {rhs})",
                lhs = lhs.pretty_print(),
                op = op.pretty_print(),
                rhs = rhs.pretty_print()
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
