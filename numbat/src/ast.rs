use crate::markup as m;
use crate::prefix_parser::AcceptsPrefix;
use crate::span::Span;
use crate::{
    arithmetic::Exponent, decorator::Decorator, markup::Markup, number::Number, prefix::Prefix,
    pretty_print::PrettyPrint, resolver::ModulePath,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Factorial,
    Negate,
}

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
    UnaryOperator {
        op: UnaryOperator,
        expr: Box<Expression>,
        span_op: Span,
    },
    BinaryOperator {
        op: BinaryOperator,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        span_op: Option<Span>, // not available for implicit multiplication and unicode exponents
    },
    FunctionCall(Span, Span, String, Vec<Expression>),
}

impl Expression {
    pub fn full_span(&self) -> Span {
        match self {
            Expression::Scalar(span, _) => *span,
            Expression::Identifier(span, _) => *span,
            Expression::UnitIdentifier(span, _, _, _) => *span,
            Expression::UnaryOperator {
                op: _,
                expr,
                span_op,
            } => span_op.extend(&expr.full_span()),
            Expression::BinaryOperator {
                op: _,
                lhs,
                rhs,
                span_op,
            } => {
                let mut span = lhs.full_span().extend(&rhs.full_span());
                if let Some(span_op) = span_op {
                    span = span.extend(&span_op);
                }
                span
            }
            Expression::FunctionCall(_identifier_span, full_span, _, _) => *full_span,
        }
    }
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
        Expression::UnaryOperator {
            op: UnaryOperator::Negate,
            expr: Box::new($rhs),
            span_op: Span::dummy(),
        }
    }};
}

#[cfg(test)]
macro_rules! factorial {
    ( $lhs:expr ) => {{
        Expression::UnaryOperator {
            op: UnaryOperator::Factorial,
            expr: Box::new($lhs),
            span_op: Span::dummy(),
        }
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
pub(crate) use factorial;
#[cfg(test)]
pub(crate) use identifier;
use itertools::Itertools;
#[cfg(test)]
pub(crate) use negate;
use num_traits::Signed;
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
        Expression::UnaryOperator { .. } | Expression::BinaryOperator { .. } => {
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
            UnaryOperator {
                op: self::UnaryOperator::Negate,
                expr,
                span_op: _,
            } => m::operator("-") + with_parens(expr),
            UnaryOperator {
                op: self::UnaryOperator::Factorial,
                expr,
                span_op: _,
            } => with_parens(expr) + m::operator("!"),
            BinaryOperator {
                op,
                lhs,
                rhs,
                span_op: _,
            } => pretty_print_binop(op, lhs, rhs),
            FunctionCall(_, _, name, args) => {
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
    Unity(Span),
    Dimension(Span, String),
    Multiply(Span, Box<DimensionExpression>, Box<DimensionExpression>),
    Divide(Span, Box<DimensionExpression>, Box<DimensionExpression>),
    Power(Span, Box<DimensionExpression>, Span, Exponent),
}

impl DimensionExpression {
    pub fn full_span(&self) -> Span {
        match self {
            DimensionExpression::Unity(s) => *s,
            DimensionExpression::Dimension(s, _) => *s,
            DimensionExpression::Multiply(span_op, lhs, rhs) => {
                span_op.extend(&lhs.full_span()).extend(&rhs.full_span())
            }
            DimensionExpression::Divide(span_op, lhs, rhs) => {
                span_op.extend(&lhs.full_span()).extend(&rhs.full_span())
            }
            DimensionExpression::Power(span_op, lhs, span_exponent, _exp) => {
                span_op.extend(&lhs.full_span()).extend(&span_exponent)
            }
        }
    }
}

impl PrettyPrint for DimensionExpression {
    fn pretty_print(&self) -> Markup {
        match self {
            DimensionExpression::Unity(_) => m::type_identifier("1"),
            DimensionExpression::Dimension(_, ident) => m::type_identifier(ident),
            DimensionExpression::Multiply(_, lhs, rhs) => {
                lhs.pretty_print() + m::space() + m::operator("×") + m::space() + rhs.pretty_print()
            }
            DimensionExpression::Divide(_, lhs, rhs) => {
                lhs.pretty_print() + m::space() + m::operator("/") + m::space() + rhs.pretty_print()
            }
            DimensionExpression::Power(_, lhs, _, exp) => {
                m::operator("(")
                    + lhs.pretty_print()
                    + m::operator(")")
                    + m::operator("^")
                    + if exp.is_positive() {
                        m::value(format!("{exp}"))
                    } else {
                        m::operator("(") + m::value(format!("{exp}")) + m::operator(")")
                    }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ProcedureKind {
    Print,
    AssertEq,
    Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expression(Expression),
    DefineVariable {
        identifier_span: Span,
        identifier: String,
        expr: Expression,
        type_annotation: Option<DimensionExpression>,
    },
    DefineFunction {
        function_name_span: Span,
        function_name: String,
        type_parameters: Vec<(Span, String)>,
        /// Parameters, optionally with type annotations. The boolean argument specifies whether or not the parameter is variadic
        parameters: Vec<(Span, String, Option<DimensionExpression>, bool)>,
        /// Function body. If it is absent, the function is implemented via FFI
        body: Option<Expression>,
        return_type_span: Option<Span>,
        /// Optional annotated return type
        return_type_annotation: Option<DimensionExpression>,
    },
    DefineDimension(String, Vec<DimensionExpression>),
    DefineBaseUnit(Span, String, Option<DimensionExpression>, Vec<Decorator>),
    DefineDerivedUnit {
        identifier_span: Span,
        identifier: String,
        expr: Expression,
        type_annotation_span: Option<Span>,
        type_annotation: Option<DimensionExpression>,
        decorators: Vec<Decorator>,
    },
    ProcedureCall(Span, ProcedureKind, Vec<Expression>),
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
            Statement::DefineVariable {
                identifier_span: _,
                identifier,
                expr,
                type_annotation,
            } => {
                m::keyword("let")
                    + m::space()
                    + m::identifier(identifier)
                    + type_annotation
                        .as_ref()
                        .map(|d| m::operator(":") + m::space() + d.pretty_print())
                        .unwrap_or_default()
                    + m::space()
                    + m::operator("=")
                    + m::space()
                    + expr.pretty_print()
            }
            Statement::DefineFunction {
                function_name_span: _,
                function_name,
                type_parameters,
                parameters: arguments,
                body,
                return_type_span: _,
                return_type_annotation,
            } => {
                let markup_type_parameters = if type_parameters.is_empty() {
                    Markup::default()
                } else {
                    m::operator("<")
                        + Itertools::intersperse(
                            type_parameters
                                .iter()
                                .map(|(_, name)| m::type_identifier(name)),
                            m::operator(", "),
                        )
                        .sum()
                        + m::operator(">")
                };

                let markup_parameters = Itertools::intersperse(
                    arguments.iter().map(|(_span, name, dexpr, is_variadic)| {
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

                let markup_return_type = return_type_annotation
                    .as_ref()
                    .map(|d| m::space() + m::operator("->") + m::space() + d.pretty_print())
                    .unwrap_or_default();

                m::keyword("fn")
                    + m::space()
                    + m::identifier(function_name)
                    + markup_type_parameters
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
            Statement::DefineDimension(identifier, dexprs) if dexprs.is_empty() => {
                m::keyword("dimension") + m::space() + m::type_identifier(identifier)
            }
            Statement::DefineDimension(identifier, dexprs) => {
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
            Statement::DefineBaseUnit(_span, identifier, dexpr, decorators) => {
                decorator_markup(decorators)
                    + m::keyword("unit")
                    + m::space()
                    + m::unit(identifier)
                    + if let Some(dexpr) = dexpr {
                        m::operator(":") + m::space() + dexpr.pretty_print()
                    } else {
                        Markup::default()
                    }
            }
            Statement::DefineDerivedUnit {
                identifier_span: _,
                identifier,
                expr,
                type_annotation_span: _,
                type_annotation,
                decorators,
            } => {
                decorator_markup(decorators)
                    + m::keyword("unit")
                    + m::space()
                    + m::unit(identifier)
                    + type_annotation
                        .as_ref()
                        .map(|d| m::operator(":") + m::space() + d.pretty_print())
                        .unwrap_or_default()
                    + m::space()
                    + m::operator("=")
                    + m::space()
                    + expr.pretty_print()
            }
            Statement::ProcedureCall(_, kind, args) => {
                let identifier = match kind {
                    ProcedureKind::Print => "print",
                    ProcedureKind::AssertEq => "assert_eq",
                    ProcedureKind::Type => "type",
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
impl ReplaceSpans for DimensionExpression {
    fn replace_spans(&self) -> Self {
        match self {
            DimensionExpression::Unity(_) => DimensionExpression::Unity(Span::dummy()),
            DimensionExpression::Dimension(_, d) => {
                DimensionExpression::Dimension(Span::dummy(), d.clone())
            }
            DimensionExpression::Multiply(_, lhs, rhs) => DimensionExpression::Multiply(
                Span::dummy(),
                Box::new(lhs.replace_spans()),
                Box::new(rhs.replace_spans()),
            ),
            DimensionExpression::Divide(_, lhs, rhs) => DimensionExpression::Divide(
                Span::dummy(),
                Box::new(lhs.replace_spans()),
                Box::new(rhs.replace_spans()),
            ),
            DimensionExpression::Power(_, lhs, _, exp) => DimensionExpression::Power(
                Span::dummy(),
                Box::new(lhs.replace_spans()),
                Span::dummy(),
                exp.clone(),
            ),
        }
    }
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
            Expression::UnaryOperator {
                op,
                expr,
                span_op: _,
            } => Expression::UnaryOperator {
                op: *op,
                expr: Box::new(expr.replace_spans()),
                span_op: Span::dummy(),
            },
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
            Expression::FunctionCall(_, _, name, args) => Expression::FunctionCall(
                Span::dummy(),
                Span::dummy(),
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
            Statement::DefineVariable {
                identifier_span: _,
                identifier,
                expr,
                type_annotation,
            } => Statement::DefineVariable {
                identifier_span: Span::dummy(),
                identifier: identifier.clone(),
                expr: expr.replace_spans(),
                type_annotation: type_annotation.as_ref().map(|t| t.replace_spans()),
            },
            Statement::DefineFunction {
                function_name_span: _,
                function_name,
                type_parameters,
                parameters,
                body,
                return_type_span,
                return_type_annotation,
            } => Statement::DefineFunction {
                function_name_span: Span::dummy(),
                function_name: function_name.clone(),
                type_parameters: type_parameters
                    .iter()
                    .map(|(_, name)| (Span::dummy(), name.clone()))
                    .collect(),
                parameters: parameters
                    .iter()
                    .map(|(_, name, type_, is_variadic)| {
                        (
                            Span::dummy(),
                            name.clone(),
                            type_.as_ref().map(|t| t.replace_spans()),
                            *is_variadic,
                        )
                    })
                    .collect(),
                body: body.clone().map(|b| b.replace_spans()),
                return_type_span: return_type_span.map(|_| Span::dummy()),
                return_type_annotation: return_type_annotation.as_ref().map(|t| t.replace_spans()),
            },
            Statement::DefineDimension(name, dexprs) => Statement::DefineDimension(
                name.clone(),
                dexprs.iter().map(|t| t.replace_spans()).collect(),
            ),
            Statement::DefineBaseUnit(_, name, type_, decorators) => Statement::DefineBaseUnit(
                Span::dummy(),
                name.clone(),
                type_.as_ref().map(|t| t.replace_spans()),
                decorators.clone(),
            ),
            Statement::DefineDerivedUnit {
                identifier_span: _,
                identifier,
                expr,
                type_annotation_span,
                type_annotation,
                decorators,
            } => Statement::DefineDerivedUnit {
                identifier_span: Span::dummy(),
                identifier: identifier.clone(),
                expr: expr.replace_spans(),
                type_annotation_span: type_annotation_span.map(|_| Span::dummy()),
                type_annotation: type_annotation.as_ref().map(|t| t.replace_spans()),
                decorators: decorators.clone(),
            },
            Statement::ProcedureCall(_, proc, args) => Statement::ProcedureCall(
                Span::dummy(),
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
        roundtrip_check("2^3!");
        roundtrip_check("-3!");
        roundtrip_check("(-3)!");
    }
}
