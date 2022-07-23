use crate::pretty_print::PrettyPrint;

#[derive(Clone, PartialEq)]
pub struct Number(f64);

impl Number {
    pub fn from_f64(n: f64) -> Self {
        Number(n)
    }
}

#[derive(Clone, Copy, PartialEq)]
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

#[derive(Clone, PartialEq)]
pub enum Expression {
    Scalar(Number),
    Negate(Box<Expression>),
    BinaryOperator(BinaryOperator, Box<Expression>, Box<Expression>),
}

#[macro_export]
macro_rules! scalar {
    ( $num:expr ) => {{
        Expression::Scalar(Number::from_f64($num))
    }};
}

#[macro_export]
macro_rules! negate {
    ( $rhs:expr ) => {{
        Expression::Negate(Box::new($rhs))
    }};
}

#[macro_export]
macro_rules! binop {
    ( $op:ident, $lhs:expr, $rhs: expr ) => {{
        Expression::BinaryOperator(BinaryOperator::$op, Box::new($lhs), Box::new($rhs))
    }};
}

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
        Mul,
        scalar!(2.0),
        binop!(Add, negate!(scalar!(3.0)), scalar!(4.0))
    );

    assert_eq!(expr.pretty_print(), "(2.0 × (-3.0 + 4.0))");
}
