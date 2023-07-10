use crate::markup as m;
use crate::prefix_parser::AcceptsPrefix;
use crate::span::Span;
use crate::{
    arithmetic::Exponent, decorator::Decorator, markup::Markup, number::Number, prefix::Prefix,
    pretty_print::PrettyPrint, resolver::ModulePath,
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

        match self {
            Add => m::space() + m::operator("+") + m::space(),
            Sub => m::space() + m::operator("-") + m::space(),
            Mul => m::space() + m::operator("×") + m::space(),
            Div => m::space() + m::operator("/") + m::space(),
            Power => m::operator("^"),
            ConvertTo => m::space() + m::operator("➞") + m::space(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Scalar(Span, Number),
    Identifier(Span, String),
    UnitIdentifier(Span, Prefix, String, String),
    Negate(Box<Expression>),
    BinaryOperator {
        op: BinaryOperator,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        span_op: Option<Span>, // not available for implicit multiplication and unicode exponents
    },
    FunctionCall(String, Vec<Expression>),
}

#[cfg(test)]
macro_rules! scalar {
    ( $num:expr ) => {{
        Expression::Scalar(Span::dummy(), Number::from_f64($num))
    }};
}

#[cfg(test)]
macro_rules! identifier {
    ( $name:expr ) => {{
        Expression::Identifier(Span::dummy(), $name.into())
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
        Expression::BinaryOperator {
            op: BinaryOperator::$op,
            lhs: Box::new($lhs),
            rhs: Box::new($rhs),
            span_op: Some(Span::dummy()),
        }
    }};
}
#[cfg(test)]
pub(crate) use binop;
#[cfg(test)]
pub(crate) use identifier;
use itertools::Itertools;
#[cfg(test)]
pub(crate) use negate;
#[cfg(test)]
pub(crate) use scalar;

fn pretty_scalar(Number(n): Number) -> Markup {
    m::value(format!("{n}"))
}

fn with_parens(expr: &Expression) -> Markup {
    match expr {
        Expression::Scalar(..)
        | Expression::Identifier(..)
        | Expression::UnitIdentifier(..)
        | Expression::FunctionCall(..) => expr.pretty_print(),
        Expression::Negate(..) | Expression::BinaryOperator { .. } => {
            m::operator("(") + expr.pretty_print() + m::operator(")")
        }
    }
}

/// Add parens, if needed -- liberal version, can not be used for exponentiation.
fn with_parens_liberal(expr: &Expression) -> Markup {
    match expr {
        Expression::BinaryOperator {
            op: BinaryOperator::Mul,
            lhs,
            rhs,
            span_op: _,
        } if matches!(**lhs, Expression::Scalar(..))
            && matches!(**rhs, Expression::UnitIdentifier(..)) =>
        {
            expr.pretty_print()
        }
        _ => with_parens(expr),
    }
}

fn pretty_print_binop(op: &BinaryOperator, lhs: &Expression, rhs: &Expression) -> Markup {
    match op {
        BinaryOperator::ConvertTo => {
            // never needs parens, it has the lowest precedence:
            lhs.pretty_print() + op.pretty_print() + rhs.pretty_print()
        }
        BinaryOperator::Mul => match (lhs, rhs) {
            (Expression::Scalar(_, s), Expression::UnitIdentifier(_, prefix, _name, full_name)) => {
                // Fuse multiplication of a scalar and a unit to a quantity
                pretty_scalar(*s)
                    + m::space()
                    + m::unit(format!("{}{}", prefix.as_string_long(), full_name))
            }
            (Expression::Scalar(_, s), Expression::Identifier(_, name)) => {
                // Fuse multiplication of a scalar and identifier
                pretty_scalar(*s) + m::space() + m::identifier(name)
            }
            _ => {
                let add_parens_if_needed = |expr: &Expression| {
                    if matches!(
                        expr,
                        Expression::BinaryOperator {
                            op: BinaryOperator::Power,
                            ..
                        } | Expression::BinaryOperator {
                            op: BinaryOperator::Mul,
                            ..
                        }
                    ) {
                        expr.pretty_print()
                    } else {
                        with_parens_liberal(expr)
                    }
                };

                add_parens_if_needed(lhs) + op.pretty_print() + add_parens_if_needed(rhs)
            }
        },
        BinaryOperator::Div => {
            let lhs_add_parens_if_needed = |expr: &Expression| {
                if matches!(
                    expr,
                    Expression::BinaryOperator {
                        op: BinaryOperator::Power,
                        ..
                    } | Expression::BinaryOperator {
                        op: BinaryOperator::Mul,
                        ..
                    }
                ) {
                    expr.pretty_print()
                } else {
                    with_parens_liberal(expr)
                }
            };
            let rhs_add_parens_if_needed = |expr: &Expression| {
                if matches!(
                    expr,
                    Expression::BinaryOperator {
                        op: BinaryOperator::Power,
                        ..
                    }
                ) {
                    expr.pretty_print()
                } else {
                    with_parens_liberal(expr)
                }
            };

            lhs_add_parens_if_needed(lhs) + op.pretty_print() + rhs_add_parens_if_needed(rhs)
        }
        BinaryOperator::Add => {
            let add_parens_if_needed = |expr: &Expression| {
                if matches!(
                    expr,
                    Expression::BinaryOperator {
                        op: BinaryOperator::Power,
                        ..
                    } | Expression::BinaryOperator {
                        op: BinaryOperator::Mul,
                        ..
                    } | Expression::BinaryOperator {
                        op: BinaryOperator::Add,
                        ..
                    }
                ) {
                    expr.pretty_print()
                } else {
                    with_parens_liberal(expr)
                }
            };

            add_parens_if_needed(lhs) + op.pretty_print() + add_parens_if_needed(rhs)
        }
        BinaryOperator::Sub => {
            let add_parens_if_needed = |expr: &Expression| {
                if matches!(
                    expr,
                    Expression::BinaryOperator {
                        op: BinaryOperator::Power,
                        ..
                    } | Expression::BinaryOperator {
                        op: BinaryOperator::Mul,
                        ..
                    }
                ) {
                    expr.pretty_print()
                } else {
                    with_parens_liberal(expr)
                }
            };

            add_parens_if_needed(lhs) + op.pretty_print() + add_parens_if_needed(rhs)
        }
        BinaryOperator::Power if matches!(rhs, Expression::Scalar(_, n) if n.to_f64() == 2.0) => {
            with_parens(lhs) + m::operator("²")
        }
        BinaryOperator::Power if matches!(rhs, Expression::Scalar(_, n) if n.to_f64() == 3.0) => {
            with_parens(lhs) + m::operator("³")
        }
        _ => with_parens(lhs) + op.pretty_print() + with_parens(rhs),
    }
}

impl PrettyPrint for Expression {
    fn pretty_print(&self) -> Markup {
        use Expression::*;

        match self {
            Scalar(_, n) => pretty_scalar(*n),
            Identifier(_, name) => m::identifier(name),
            UnitIdentifier(_, prefix, _name, full_name) => {
                m::unit(format!("{}{}", prefix.as_string_long(), full_name))
            }
            Negate(rhs) => m::operator("-") + with_parens(rhs),
            BinaryOperator {
                op,
                lhs,
                rhs,
                span_op: _,
            } => pretty_print_binop(op, lhs, rhs),
            FunctionCall(name, args) => {
                m::identifier(name)
                    + m::operator("(")
                    + itertools::Itertools::intersperse(
                        args.iter().map(|e| e.pretty_print()),
                        m::operator(",") + m::space(),
                    )
                    .sum()
                    + m::operator(")")
            }
        }
    }
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
                lhs.pretty_print() + m::space() + m::operator("×") + m::space() + rhs.pretty_print()
            }
            DimensionExpression::Divide(lhs, rhs) => {
                lhs.pretty_print() + m::space() + m::operator("/") + m::space() + rhs.pretty_print()
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
    DeclareVariable(Span, String, Expression, Option<DimensionExpression>),
    DeclareFunction(
        Span,
        /// Function name
        String,
        /// Introduced type parameters
        Vec<String>,
        /// Arguments, optionally with type annotations. The boolean argument specifies whether or not the parameter is variadic
        Vec<(String, Option<DimensionExpression>, bool)>,
        /// Function body. If it is absent, the function is implemented via FFI
        Option<Expression>,
        /// Optional annotated return type
        Option<DimensionExpression>,
    ),
    DeclareDimension(String, Vec<DimensionExpression>),
    DeclareBaseUnit(Span, String, DimensionExpression, Vec<Decorator>),
    DeclareDerivedUnit(
        Span,
        String,
        Expression,
        Option<DimensionExpression>,
        Vec<Decorator>,
    ),
    ProcedureCall(ProcedureKind, Vec<Expression>),
    ModuleImport(Span, ModulePath),
}

fn accepts_prefix_markup(accepts_prefix: &Option<AcceptsPrefix>) -> Markup {
    if let Some(accepts_prefix) = accepts_prefix {
        m::operator(":")
            + m::space()
            + match accepts_prefix {
                AcceptsPrefix {
                    short: true,
                    long: true,
                } => m::keyword("both"),
                AcceptsPrefix {
                    short: true,
                    long: false,
                } => m::keyword("short"),
                AcceptsPrefix {
                    short: false,
                    long: true,
                } => m::keyword("long"),
                AcceptsPrefix {
                    short: false,
                    long: false,
                } => m::keyword("none"),
            }
    } else {
        Markup::default()
    }
}

fn decorator_markup(decorators: &Vec<Decorator>) -> Markup {
    let mut markup_decorators = Markup::default();
    for decorator in decorators {
        markup_decorators = markup_decorators
            + match decorator {
                Decorator::MetricPrefixes => m::decorator("@metric_prefixes"),
                Decorator::BinaryPrefixes => m::decorator("@binary_prefixes"),
                Decorator::Aliases(names) => {
                    m::decorator("@aliases")
                        + m::operator("(")
                        + Itertools::intersperse(
                            names.iter().map(|(name, accepts_prefix)| {
                                m::unit(name) + accepts_prefix_markup(accepts_prefix)
                            }),
                            m::operator(", "),
                        )
                        .sum()
                        + m::operator(")")
                }
            }
            + m::nl();
    }
    markup_decorators
}

impl PrettyPrint for Statement {
    fn pretty_print(&self) -> Markup {
        match self {
            Statement::DeclareVariable(_span, identifier, expr, dexpr) => {
                m::keyword("let")
                    + m::space()
                    + m::identifier(identifier)
                    + dexpr
                        .as_ref()
                        .map(|d| m::operator(":") + m::space() + d.pretty_print())
                        .unwrap_or_default()
                    + m::space()
                    + m::operator("=")
                    + m::space()
                    + expr.pretty_print()
            }
            Statement::DeclareFunction(
                _span,
                identifier,
                type_variables,
                parameters,
                body,
                dexpr,
            ) => {
                let markup_type_variables = if type_variables.is_empty() {
                    Markup::default()
                } else {
                    m::operator("<")
                        + Itertools::intersperse(
                            type_variables.iter().map(m::type_identifier),
                            m::operator(", "),
                        )
                        .sum()
                        + m::operator(">")
                };

                let markup_parameters = Itertools::intersperse(
                    parameters.iter().map(|(name, dexpr, is_variadic)| {
                        m::identifier(name)
                            + dexpr
                                .as_ref()
                                .map(|d| {
                                    m::operator(":")
                                        + m::space()
                                        + d.pretty_print()
                                        + if *is_variadic {
                                            m::operator("…")
                                        } else {
                                            Markup::default()
                                        }
                                })
                                .unwrap_or_default()
                    }),
                    m::operator(", "),
                )
                .sum();

                let markup_return_type = dexpr
                    .as_ref()
                    .map(|d| m::space() + m::operator("->") + m::space() + d.pretty_print())
                    .unwrap_or_default();

                m::keyword("fn")
                    + m::space()
                    + m::identifier(identifier)
                    + markup_type_variables
                    + m::operator("(")
                    + markup_parameters
                    + m::operator(")")
                    + markup_return_type
                    + body
                        .as_ref()
                        .map(|e| m::space() + m::operator("=") + m::space() + e.pretty_print())
                        .unwrap_or_default()
            }
            Statement::Expression(expr) => expr.pretty_print(),
            Statement::DeclareDimension(identifier, dexprs) if dexprs.is_empty() => {
                m::keyword("dimension") + m::space() + m::type_identifier(identifier)
            }
            Statement::DeclareDimension(identifier, dexprs) => {
                m::keyword("dimension")
                    + m::space()
                    + m::type_identifier(identifier)
                    + m::space()
                    + m::operator("=")
                    + m::space()
                    + Itertools::intersperse(
                        dexprs.iter().map(|d| d.pretty_print()),
                        m::space() + m::operator("=") + m::space(),
                    )
                    .sum()
            }
            Statement::DeclareBaseUnit(_span, identifier, dexpr, decorators) => {
                decorator_markup(decorators)
                    + m::keyword("unit")
                    + m::space()
                    + m::unit(identifier)
                    + m::operator(":")
                    + m::space()
                    + dexpr.pretty_print()
            }
            Statement::DeclareDerivedUnit(_span, identifier, expr, dexpr, decorators) => {
                decorator_markup(decorators)
                    + m::keyword("unit")
                    + m::space()
                    + m::unit(identifier)
                    + dexpr
                        .as_ref()
                        .map(|d| m::operator(":") + m::space() + d.pretty_print())
                        .unwrap_or_default()
                    + m::space()
                    + m::operator("=")
                    + m::space()
                    + expr.pretty_print()
            }
            Statement::ProcedureCall(kind, args) => {
                let identifier = match kind {
                    ProcedureKind::Print => "print",
                    ProcedureKind::AssertEq => "assert_eq",
                };
                m::identifier(identifier)
                    + m::operator("(")
                    + Itertools::intersperse(
                        args.iter().map(|a| a.pretty_print()),
                        m::operator(",") + m::space(),
                    )
                    .sum()
                    + m::operator(")")
            }
            Statement::ModuleImport(_span, module_path) => {
                m::keyword("use")
                    + m::space()
                    + Itertools::intersperse(
                        module_path.0.iter().map(m::identifier),
                        m::operator("::"),
                    )
                    .sum()
            }
        }
    }
}

#[cfg(test)]
pub trait ReplaceSpans {
    fn replace_spans(&self) -> Self;
}

#[cfg(test)]
impl ReplaceSpans for Expression {
    fn replace_spans(&self) -> Self {
        match self {
            Expression::Scalar(_, name) => Expression::Scalar(Span::dummy(), name.clone()),
            Expression::Identifier(_, name) => Expression::Identifier(Span::dummy(), name.clone()),
            Expression::UnitIdentifier(_, prefix, name, full_name) => Expression::UnitIdentifier(
                Span::dummy(),
                prefix.clone(),
                name.clone(),
                full_name.clone(),
            ),
            Expression::Negate(expr) => Expression::Negate(Box::new(expr.replace_spans())),
            Expression::BinaryOperator {
                op,
                lhs,
                rhs,
                span_op: _,
            } => Expression::BinaryOperator {
                op: *op,
                lhs: Box::new(lhs.replace_spans()),
                rhs: Box::new(rhs.replace_spans()),
                span_op: Some(Span::dummy()),
            },
            Expression::FunctionCall(name, args) => Expression::FunctionCall(
                name.clone(),
                args.iter().map(|a| a.replace_spans()).collect(),
            ),
        }
    }
}

#[cfg(test)]
impl ReplaceSpans for Statement {
    fn replace_spans(&self) -> Self {
        match self {
            Statement::Expression(expr) => Statement::Expression(expr.replace_spans()),
            Statement::DeclareVariable(_, name, expr, type_) => Statement::DeclareVariable(
                Span::dummy(),
                name.clone(),
                expr.replace_spans(),
                type_.clone(),
            ),
            Statement::DeclareFunction(_span, name, type_params, args, body, type_) => {
                Statement::DeclareFunction(
                    Span::dummy(),
                    name.clone(),
                    type_params.clone(),
                    args.clone(),
                    body.clone().map(|b| b.replace_spans()),
                    type_.clone(),
                )
            }
            s @ Statement::DeclareDimension(_, _) => s.clone(),
            Statement::DeclareBaseUnit(_, name, type_, decorators) => Statement::DeclareBaseUnit(
                Span::dummy(),
                name.clone(),
                type_.clone(),
                decorators.clone(),
            ),
            Statement::DeclareDerivedUnit(_, name, expr, type_, decorators) => {
                Statement::DeclareDerivedUnit(
                    Span::dummy(),
                    name.clone(),
                    expr.replace_spans(),
                    type_.clone(),
                    decorators.clone(),
                )
            }
            Statement::ProcedureCall(proc, args) => Statement::ProcedureCall(
                proc.clone(),
                args.iter().map(|a| a.replace_spans()).collect(),
            ),
            Statement::ModuleImport(_, module_path) => {
                Statement::ModuleImport(Span::dummy(), module_path.clone())
            }
        }
    }
}

#[cfg(test)]
impl ReplaceSpans for Vec<Statement> {
    fn replace_spans(&self) -> Self {
        self.iter().map(|s| s.replace_spans()).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        markup::{Formatter, PlainTextFormatter},
        prefix_transformer::Transformer,
    };

    #[test]
    fn expression_pretty_print() {
        let expr = binop!(
            scalar!(2.0),
            Mul,
            binop!(negate!(scalar!(3.0)), Add, scalar!(4.0))
        );

        assert_eq!(expr.pretty_print().to_string(), "2 × ((-3) + 4)");
    }

    fn parse(code: &str) -> Statement {
        let mut transformer = Transformer::new();
        transformer
            .register_name_and_aliases(
                &"meter".into(),
                &[
                    Decorator::Aliases(vec![("m".into(), Some(AcceptsPrefix::only_short()))]),
                    Decorator::MetricPrefixes,
                ],
                Span::dummy(),
            )
            .unwrap();
        transformer
            .register_name_and_aliases(
                &"second".into(),
                &[
                    Decorator::Aliases(vec![("s".into(), Some(AcceptsPrefix::only_short()))]),
                    Decorator::MetricPrefixes,
                ],
                Span::dummy(),
            )
            .unwrap();
        transformer
            .register_name_and_aliases(
                &"radian".into(),
                &[
                    Decorator::Aliases(vec![("rad".into(), Some(AcceptsPrefix::only_short()))]),
                    Decorator::MetricPrefixes,
                ],
                Span::dummy(),
            )
            .unwrap();
        transformer
            .register_name_and_aliases(
                &"degree".into(),
                &[Decorator::Aliases(vec![("°".into(), None)])],
                Span::dummy(),
            )
            .unwrap();
        transformer
            .register_name_and_aliases(
                &"inch".into(),
                &[Decorator::Aliases(vec![("in".into(), None)])],
                Span::dummy(),
            )
            .unwrap();

        let statements = crate::parser::parse(code, 0).unwrap();
        transformer.transform(statements).unwrap()[0].clone()
    }

    fn pretty_print(stmt: &Statement) -> String {
        let markup = stmt.pretty_print();

        (PlainTextFormatter {}).format(&markup, false)
    }

    fn equal_pretty(input: &str, output: &str) {
        assert_eq!(pretty_print(&parse(input)), output);
    }

    #[test]
    fn pretty_print_basic() {
        equal_pretty("2+3", "2 + 3");
        equal_pretty("2*3", "2 × 3");
        equal_pretty("2^3", "2³");
        equal_pretty("2km", "2 kilometer");
        equal_pretty("2kilometer", "2 kilometer");
        equal_pretty("sin(30°)", "sin(30 degree)");
        equal_pretty("2*3*4", "2 × 3 × 4");
        equal_pretty("2*(3*4)", "2 × 3 × 4");
        equal_pretty("2+3+4", "2 + 3 + 4");
        equal_pretty("2+(3+4)", "2 + 3 + 4");
        equal_pretty("atan(30cm / 2m)", "atan(30 centimeter / 2 meter)");
        equal_pretty("1mrad -> °", "1 milliradian ➞ degree");
        equal_pretty("2km+2cm -> in", "2 kilometer + 2 centimeter ➞ inch");
        equal_pretty("2^3 + 4^5", "2³ + 4^5");
        equal_pretty("2^3 - 4^5", "2³ - 4^5");
        equal_pretty("2^3 * 4^5", "2³ × 4^5");
        equal_pretty("2 * 3 + 4 * 5", "2 × 3 + 4 × 5");
        equal_pretty("2 * 3 / 4", "2 × 3 / 4");
        equal_pretty("123.123 km² / s²", "123.123 × kilometer² / second²");
        equal_pretty(" sin(  2  ,  3  ,  4   )  ", "sin(2, 3, 4)");
    }

    fn roundtrip_check(code: &str) {
        let ast1 = parse(code).replace_spans();
        let ast2 = parse(&pretty_print(&ast1)).replace_spans();
        assert_eq!(ast1, ast2);
    }

    #[test]
    fn pretty_print_roundtrip_check() {
        roundtrip_check("1.0");
        roundtrip_check("2");
        roundtrip_check("1 + 2");

        roundtrip_check("-2.3e-12387");
        roundtrip_check("2.3e-12387");
        roundtrip_check("18379173");
        roundtrip_check("2+3");
        roundtrip_check("2+3*5");
        roundtrip_check("-3^4+2/(4+2*3)");
        roundtrip_check("1-2-3-4-(5-6-7)");
        roundtrip_check("1/2/3/4/(5/6/7)");
        roundtrip_check("kg");
        roundtrip_check("2meter/second");
        roundtrip_check("a+b*c^d-e*f");
        roundtrip_check("sin(x)^3");
        roundtrip_check("sin(cos(atanh(x)+2))^3");
        roundtrip_check("2^3^4^5");
        roundtrip_check("(2^3)^(4^5)");
        roundtrip_check("sqrt(1.4^2 + 1.5^2) * cos(pi/3)^2");
        roundtrip_check("40 kilometer * 9.8meter/second^2 * 150centimeter");
        roundtrip_check("4/3 * pi * r³");
        roundtrip_check("vol * density -> kg");
        roundtrip_check("atan(30 centimeter / 2 meter)");
        roundtrip_check("500kilometer/second -> centimeter/second");
        roundtrip_check("länge * x_2 * µ * _prefixed");
        roundtrip_check("2meter^3");
        roundtrip_check("(2meter)^3");
        roundtrip_check("-sqrt(-30meter^3)");
        roundtrip_check("-3^4");
        roundtrip_check("(-3)^4");
        roundtrip_check("sin(2,3,4)");

        // TODO: when we support factorials
        // roundtrip_check("5!³");
        // roundtrip_check("2^3!");
        // roundtrip_check("-3!");
        // roundtrip_check("(-3)!");
    }
}
