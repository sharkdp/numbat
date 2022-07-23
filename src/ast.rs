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
    let expr = Expression::BinaryOperator(
        BinaryOperator::Mul,
        Box::new(Expression::Scalar(Number(2.0))),
        Box::new(Expression::BinaryOperator(
            BinaryOperator::Add,
            Box::new(Expression::Negate(Box::new(Expression::Scalar(Number(
                3.0,
            ))))),
            Box::new(Expression::Scalar(Number(4.0))),
        )),
    );

    assert_eq!(expr.pretty_print(), "(2.0 × (-3.0 + 4.0))");
}
