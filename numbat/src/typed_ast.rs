use indexmap::IndexMap;
use itertools::Itertools;

use crate::arithmetic::{Exponent, Rational};
use crate::ast::ProcedureKind;
pub use crate::ast::{BinaryOperator, TypeExpression, UnaryOperator};
use crate::dimension::DimensionRegistry;
use crate::{
    decorator::Decorator, markup::Markup, number::Number, prefix::Prefix,
    prefix_parser::AcceptsPrefix, pretty_print::PrettyPrint, registry::BaseRepresentation,
    span::Span,
};
use crate::{markup as m, BaseRepresentationFactor};

/// Dimension type
pub type DType = BaseRepresentation;

impl DType {
    pub fn is_scalar(&self) -> bool {
        self == &DType::unity()
    }
    pub fn to_readable_type(&self, registry: &DimensionRegistry) -> m::Markup {
        if self.is_scalar() {
            return m::type_identifier("Scalar");
        }

        let mut names = vec![];

        let factors: Vec<_> = self.iter().collect();
        if factors.len() == 1 && factors[0].1 == Exponent::from_integer(1) {
            names.push(factors[0].0.clone());
        }

        names.extend(registry.get_derived_entry_names_for(self));
        match &names[..] {
            [] => self.pretty_print(),
            [single] => m::type_identifier(single),
            multiple => {
                Itertools::intersperse(multiple.iter().map(m::type_identifier), m::dimmed(" or "))
                    .sum()
            }
        }
    }
    /// Is the current dimension type the Time dimension?
    ///
    /// This is special helper that's useful when dealing with DateTimes
    pub fn is_time_dimension(&self) -> bool {
        *self
            == BaseRepresentation::from_factor(BaseRepresentationFactor(
                "Time".into(),
                Rational::from_integer(1),
            ))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructInfo {
    pub definition_span: Span,
    pub name: String,
    pub fields: IndexMap<String, (Span, Type)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Never,
    Dimension(DType),
    Boolean,
    String,
    DateTime,
    Fn(Vec<Type>, Box<Type>),
    Struct(StructInfo),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Never => write!(f, "!"),
            Type::Dimension(d) => d.fmt(f),
            Type::Boolean => write!(f, "Bool"),
            Type::String => write!(f, "String"),
            Type::DateTime => write!(f, "DateTime"),
            Type::Fn(param_types, return_type) => {
                write!(
                    f,
                    "Fn[({ps}) -> {return_type}]",
                    ps = param_types.iter().map(|p| p.to_string()).join(", ")
                )
            }
            Type::Struct(StructInfo { name, fields, .. }) => {
                write!(
                    f,
                    "{name} {{{}}}",
                    fields
                        .iter()
                        .map(|(n, (_, t))| n.to_string() + ": " + &t.to_string())
                        .join(", ")
                )
            }
        }
    }
}

impl PrettyPrint for Type {
    fn pretty_print(&self) -> Markup {
        match self {
            Type::Never => m::keyword("!"),
            Type::Dimension(d) => d.pretty_print(),
            Type::Boolean => m::keyword("Bool"),
            Type::String => m::keyword("String"),
            Type::DateTime => m::keyword("DateTime"),
            Type::Fn(param_types, return_type) => {
                m::type_identifier("Fn")
                    + m::operator("[(")
                    + Itertools::intersperse(
                        param_types.iter().map(|t| t.pretty_print()),
                        m::operator(",") + m::space(),
                    )
                    .sum()
                    + m::operator(")")
                    + m::space()
                    + m::operator("->")
                    + m::space()
                    + return_type.pretty_print()
                    + m::operator("]")
            }
            Type::Struct(StructInfo { name, .. }) => m::type_identifier(name),
        }
    }
}

impl Type {
    pub fn to_readable_type(&self, registry: &DimensionRegistry) -> Markup {
        match self {
            Type::Dimension(d) => d.to_readable_type(registry),
            _ => self.pretty_print(),
        }
    }

    pub fn scalar() -> Type {
        Type::Dimension(DType::unity())
    }

    pub fn is_never(&self) -> bool {
        matches!(self, Type::Never)
    }

    pub fn is_dtype(&self) -> bool {
        matches!(self, Type::Dimension(..))
    }

    pub fn is_fn_type(&self) -> bool {
        matches!(self, Type::Fn(..))
    }

    pub fn is_subtype_of(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Never, _) => true,
            (_, Type::Never) => false,
            (t1, t2) => t1 == t2,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StringPart {
    Fixed(String),
    Interpolation {
        span: Span,
        expr: Box<Expression>,
        format_specifiers: Option<String>,
    },
}

impl PrettyPrint for StringPart {
    fn pretty_print(&self) -> Markup {
        match self {
            StringPart::Fixed(s) => m::string(s),
            StringPart::Interpolation {
                span: _,
                expr,
                format_specifiers,
            } => {
                let mut markup = m::operator("{") + expr.pretty_print();

                if let Some(format_specifiers) = format_specifiers {
                    markup += m::text(format_specifiers);
                }

                markup += m::operator("}");

                markup
            }
        }
    }
}

impl PrettyPrint for &Vec<StringPart> {
    fn pretty_print(&self) -> Markup {
        m::operator("\"") + self.iter().map(|p| p.pretty_print()).sum() + m::operator("\"")
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
    /// A special binary operator that has a DateTime as one (or both) of the operands
    BinaryOperatorForDate(
        Option<Span>,
        BinaryOperator,
        /// LHS must evaluate to a DateTime
        Box<Expression>,
        /// RHS can evaluate to a DateTime or a quantity of type Time
        Box<Expression>,
        Type,
    ),
    // A 'proper' function call
    FunctionCall(Span, Span, String, Vec<Expression>, Type),
    // A call via a function object
    CallableCall(Span, Box<Expression>, Vec<Expression>, Type),
    Boolean(Span, bool),
    Condition(Span, Box<Expression>, Box<Expression>, Box<Expression>),
    String(Span, Vec<StringPart>),
    InstantiateStruct(Span, Vec<(String, Expression)>, StructInfo),
    AccessField(Span, Span, Box<Expression>, String, StructInfo, Type),
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
            Expression::BinaryOperatorForDate(span_op, _op, lhs, rhs, ..) => {
                let mut span = lhs.full_span().extend(&rhs.full_span());
                if let Some(span_op) = span_op {
                    span = span.extend(span_op);
                }
                span
            }
            Expression::FunctionCall(_identifier_span, full_span, _, _, _) => *full_span,
            Expression::CallableCall(full_span, _, _, _) => *full_span,
            Expression::Boolean(span, _) => *span,
            Expression::Condition(span_if, _, _, then_expr) => {
                span_if.extend(&then_expr.full_span())
            }
            Expression::String(span, _) => *span,
            Expression::InstantiateStruct(span, _, _) => *span,
            Expression::AccessField(_span, full_span, _, _, _, _) => *full_span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expression(Expression),
    DefineVariable(String, Vec<Decorator>, Expression, Markup, Type),
    DefineFunction(
        String,
        Vec<Decorator>, // decorators
        Vec<String>,    // type parameters
        Vec<(
            // parameter:
            Span,   // span of the parameter
            String, // parameter name
            bool,   // whether or not it is variadic
            Markup, // readable parameter type
            Type,   // parameter type
        )>,
        Option<Expression>, // function body
        Markup,             // readable return type
        Type,               // return type
    ),
    DefineDimension(String, Vec<TypeExpression>),
    DefineBaseUnit(String, Vec<Decorator>, Markup, Type),
    DefineDerivedUnit(String, Expression, Vec<Decorator>, Markup, Type),
    ProcedureCall(crate::ast::ProcedureKind, Vec<Expression>),
    DefineStruct(StructInfo),
}

impl Statement {
    pub fn as_expression(&self) -> Option<&Expression> {
        if let Self::Expression(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl Expression {
    pub fn get_type(&self) -> Type {
        match self {
            Expression::Scalar(_, _) => Type::Dimension(DType::unity()),
            Expression::Identifier(_, _, type_) => type_.clone(),
            Expression::UnitIdentifier(_, _, _, _, _type) => _type.clone(),
            Expression::UnaryOperator(_, _, _, type_) => type_.clone(),
            Expression::BinaryOperator(_, _, _, _, type_) => type_.clone(),
            Expression::BinaryOperatorForDate(_, _, _, _, type_, ..) => type_.clone(),
            Expression::FunctionCall(_, _, _, _, type_) => type_.clone(),
            Expression::CallableCall(_, _, _, type_) => type_.clone(),
            Expression::Boolean(_, _) => Type::Boolean,
            Expression::Condition(_, _, then_, else_) => {
                if then_.get_type().is_never() {
                    else_.get_type()
                } else {
                    then_.get_type()
                }
            }
            Expression::String(_, _) => Type::String,
            Expression::InstantiateStruct(_, _, type_) => Type::Struct(type_.clone()),
            Expression::AccessField(_, _, _, _, _, type_) => type_.clone(),
        }
    }
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
        m::empty()
    }
}

fn decorator_markup(decorators: &Vec<Decorator>) -> Markup {
    let mut markup_decorators = m::empty();
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
                Decorator::Url(url) => {
                    m::decorator("@url") + m::operator("(") + m::string(url) + m::operator(")")
                }
                Decorator::Name(name) => {
                    m::decorator("@name") + m::operator("(") + m::string(name) + m::operator(")")
                }
                Decorator::Description(description) => {
                    m::decorator("@description")
                        + m::operator("(")
                        + m::string(description)
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
            Statement::DefineVariable(identifier, _decs, expr, readable_type, _type) => {
                m::keyword("let")
                    + m::space()
                    + m::identifier(identifier)
                    + m::operator(":")
                    + m::space()
                    + readable_type.clone()
                    + m::space()
                    + m::operator("=")
                    + m::space()
                    + expr.pretty_print()
            }
            Statement::DefineFunction(
                function_name,
                _decorators,
                type_parameters,
                parameters,
                body,
                readable_return_type,
                _return_type,
            ) => {
                let markup_type_parameters = if type_parameters.is_empty() {
                    m::empty()
                } else {
                    m::operator("<")
                        + Itertools::intersperse(
                            type_parameters.iter().map(m::type_identifier),
                            m::operator(", "),
                        )
                        .sum()
                        + m::operator(">")
                };

                let markup_parameters = Itertools::intersperse(
                    parameters
                        .iter()
                        .map(|(_span, name, is_variadic, readable_type, _type)| {
                            m::identifier(name)
                                + m::operator(":")
                                + m::space()
                                + readable_type.clone()
                                + if *is_variadic {
                                    m::operator("…")
                                } else {
                                    m::empty()
                                }
                        }),
                    m::operator(", "),
                )
                .sum();

                let markup_return_type =
                    m::space() + m::operator("->") + m::space() + readable_return_type.clone();

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
            Statement::DefineBaseUnit(identifier, decorators, readable_type, _type) => {
                decorator_markup(decorators)
                    + m::keyword("unit")
                    + m::space()
                    + m::unit(identifier)
                    + m::operator(":")
                    + m::space()
                    + readable_type.clone()
            }
            Statement::DefineDerivedUnit(identifier, expr, decorators, readable_type, _type) => {
                decorator_markup(decorators)
                    + m::keyword("unit")
                    + m::space()
                    + m::unit(identifier)
                    + m::operator(":")
                    + m::space()
                    + readable_type.clone()
                    + m::space()
                    + m::operator("=")
                    + m::space()
                    + expr.pretty_print()
            }
            Statement::ProcedureCall(kind, args) => {
                let identifier = match kind {
                    ProcedureKind::Print => "print",
                    ProcedureKind::Assert => "assert",
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
            Statement::DefineStruct(StructInfo { name, fields, .. }) => {
                m::keyword("struct")
                    + m::space()
                    + m::type_identifier(name.clone())
                    + m::space()
                    + m::operator("{")
                    + if fields.is_empty() {
                        m::empty()
                    } else {
                        m::space()
                            + Itertools::intersperse(
                                fields.iter().map(|(n, (_, t))| {
                                    m::identifier(n)
                                        + m::operator(":")
                                        + m::space()
                                        + t.pretty_print()
                                }),
                                m::operator(",") + m::space(),
                            )
                            .sum()
                            + m::space()
                    }
                    + m::operator("}")
            }
        }
    }
}

fn pretty_scalar(n: Number) -> Markup {
    m::value(n.pretty_print())
}

fn with_parens(expr: &Expression) -> Markup {
    match expr {
        Expression::Scalar(..)
        | Expression::Identifier(..)
        | Expression::UnitIdentifier(..)
        | Expression::FunctionCall(..)
        | Expression::CallableCall(..)
        | Expression::Boolean(..)
        | Expression::String(..)
        | Expression::InstantiateStruct(..)
        | Expression::AccessField(..) => expr.pretty_print(),
        Expression::UnaryOperator { .. }
        | Expression::BinaryOperator { .. }
        | Expression::BinaryOperatorForDate { .. }
        | Expression::Condition(..) => m::operator("(") + expr.pretty_print() + m::operator(")"),
    }
}

/// Add parens, if needed -- liberal version, can not be used for exponentiation.
fn with_parens_liberal(expr: &Expression) -> Markup {
    match expr {
        Expression::BinaryOperator(_, BinaryOperator::Mul, lhs, rhs, _type)
            if matches!(**lhs, Expression::Scalar(..))
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
            (
                Expression::Scalar(_, s),
                Expression::UnitIdentifier(_, prefix, _name, full_name, _type),
            ) => {
                // Fuse multiplication of a scalar and a unit to a quantity
                pretty_scalar(*s)
                    + m::space()
                    + m::unit(format!("{}{}", prefix.as_string_long(), full_name))
            }
            (Expression::Scalar(_, s), Expression::Identifier(_, name, _type)) => {
                // Fuse multiplication of a scalar and identifier
                pretty_scalar(*s) + m::space() + m::identifier(name)
            }
            _ => {
                let add_parens_if_needed = |expr: &Expression| {
                    if matches!(
                        expr,
                        Expression::BinaryOperator(_, BinaryOperator::Power, ..)
                            | Expression::BinaryOperator(_, BinaryOperator::Mul, ..)
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
                    Expression::BinaryOperator(_, BinaryOperator::Power, ..)
                        | Expression::BinaryOperator(_, BinaryOperator::Mul, ..)
                ) {
                    expr.pretty_print()
                } else {
                    with_parens_liberal(expr)
                }
            };
            let rhs_add_parens_if_needed = |expr: &Expression| {
                if matches!(
                    expr,
                    Expression::BinaryOperator(_, BinaryOperator::Power, ..)
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
                    Expression::BinaryOperator(_, BinaryOperator::Power, ..)
                        | Expression::BinaryOperator(_, BinaryOperator::Mul, ..)
                        | Expression::BinaryOperator(_, BinaryOperator::Add, ..)
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
                    Expression::BinaryOperator(_, BinaryOperator::Power, ..)
                        | Expression::BinaryOperator(_, BinaryOperator::Mul, ..)
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
            Identifier(_, name, _type) => m::identifier(name),
            UnitIdentifier(_, prefix, _name, full_name, _type) => {
                m::unit(format!("{}{}", prefix.as_string_long(), full_name))
            }
            UnaryOperator(_, self::UnaryOperator::Negate, expr, _type) => {
                m::operator("-") + with_parens(expr)
            }
            UnaryOperator(_, self::UnaryOperator::Factorial, expr, _type) => {
                with_parens(expr) + m::operator("!")
            }
            UnaryOperator(_, self::UnaryOperator::LogicalNeg, expr, _type) => {
                m::operator("!") + with_parens(expr)
            }
            BinaryOperator(_, op, lhs, rhs, _type) => pretty_print_binop(op, lhs, rhs),
            BinaryOperatorForDate(_, op, lhs, rhs, _type) => pretty_print_binop(op, lhs, rhs),
            FunctionCall(_, _, name, args, _type) => {
                m::identifier(name)
                    + m::operator("(")
                    + itertools::Itertools::intersperse(
                        args.iter().map(|e| e.pretty_print()),
                        m::operator(",") + m::space(),
                    )
                    .sum()
                    + m::operator(")")
            }
            CallableCall(_, expr, args, _type) => {
                expr.pretty_print()
                    + m::operator("(")
                    + itertools::Itertools::intersperse(
                        args.iter().map(|e| e.pretty_print()),
                        m::operator(",") + m::space(),
                    )
                    .sum()
                    + m::operator(")")
            }
            Boolean(_, val) => val.pretty_print(),
            String(_, parts) => parts.pretty_print(),
            Condition(_, condition, then, else_) => {
                m::keyword("if")
                    + m::space()
                    + with_parens(condition)
                    + m::space()
                    + m::keyword("then")
                    + m::space()
                    + with_parens(then)
                    + m::space()
                    + m::keyword("else")
                    + m::space()
                    + with_parens(else_)
            }
            InstantiateStruct(_, exprs, struct_info) => {
                m::type_identifier(struct_info.name.clone())
                    + m::space()
                    + m::operator("{")
                    + if exprs.is_empty() {
                        m::empty()
                    } else {
                        m::space()
                            + itertools::Itertools::intersperse(
                                exprs.iter().map(|(n, e)| {
                                    m::identifier(n)
                                        + m::operator(":")
                                        + m::space()
                                        + e.pretty_print()
                                }),
                                m::operator(",") + m::space(),
                            )
                            .sum()
                            + m::space()
                    }
                    + m::operator("}")
            }
            AccessField(_, _, expr, attr, _, _) => {
                expr.pretty_print() + m::operator(".") + m::identifier(attr)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::ReplaceSpans;
    use crate::markup::{Formatter, PlainTextFormatter};
    use crate::prefix_transformer::Transformer;

    fn parse(code: &str) -> Statement {
        let statements = crate::parser::parse(
            &format!(
                "dimension Scalar = 1
                 dimension Length
                 dimension Time
                 dimension Mass

                 fn sin(x: Scalar) -> Scalar
                 fn cos(x: Scalar) -> Scalar
                 fn asin(x: Scalar) -> Scalar
                 fn atan(x: Scalar) -> Scalar
                 fn atan2<T>(x: T, y: T) -> Scalar
                 fn sqrt(x) = x^(1/2)
                 let pi = 2 asin(1)

                 @aliases(m: short)
                 @metric_prefixes
                 unit meter: Length

                 @aliases(s: short)
                 @metric_prefixes
                 unit second: Time

                 @aliases(g: short)
                 @metric_prefixes
                 unit gram: Mass

                 @aliases(rad: short)
                 @metric_prefixes
                 unit radian: Scalar = meter / meter

                 @aliases(°: none)
                 unit degree = 180/pi × radian

                 @aliases(in: short)
                 unit inch = 0.0254 m

                 @metric_prefixes
                 unit points

                 struct Foo {{foo: Length, bar: Time}}

                 let a = 1
                 let b = 1
                 let c = 1
                 let d = 1
                 let e = 1
                 let f = 1
                 let x = 1
                 let r = 2 m
                 let vol = 3 m^3
                 let density = 1000 kg / m^3
                 let länge = 1
                 let x_2 = 1
                 let µ = 1
                 let _prefixed = 1

                 {code}"
            ),
            0,
        )
        .unwrap();

        let mut transformer = Transformer::new();
        let transformed_statements = transformer.transform(statements).unwrap().replace_spans();

        crate::typechecker::TypeChecker::default()
            .check_statements(transformed_statements)
            .unwrap()
            .last()
            .unwrap()
            .clone()
    }

    fn pretty_print(stmt: &Statement) -> String {
        let markup = stmt.pretty_print();

        (PlainTextFormatter {}).format(&markup, false)
    }

    fn equal_pretty(input: &str, expected: &str) {
        println!();
        println!("expected: '{expected}'");
        let actual = pretty_print(&parse(input));
        println!("actual:   '{actual}'");
        assert_eq!(actual, expected);
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
    }

    fn roundtrip_check(code: &str) {
        println!("Roundtrip check for code = '{code}'");
        let ast1 = parse(code);
        let code_pretty = pretty_print(&ast1);
        println!("     pretty printed code = '{code_pretty}'");
        let ast2 = parse(&code_pretty);
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
        roundtrip_check("kilogram");
        roundtrip_check("2meter/second");
        roundtrip_check("a+b*c^d-e*f");
        roundtrip_check("sin(x)^3");
        roundtrip_check("sin(cos(atan(x)+2))^3");
        roundtrip_check("2^3^4^5");
        roundtrip_check("(2^3)^(4^5)");
        roundtrip_check("sqrt(1.4^2 + 1.5^2) * cos(pi/3)^2");
        roundtrip_check("40 kilometer * 9.8meter/second^2 * 150centimeter");
        roundtrip_check("4/3 * pi * r³");
        roundtrip_check("vol * density -> kilogram");
        roundtrip_check("atan(30 centimeter / 2 meter)");
        roundtrip_check("500kilometer/second -> centimeter/second");
        roundtrip_check("länge * x_2 * µ * _prefixed");
        roundtrip_check("2meter^3");
        roundtrip_check("(2meter)^3");
        roundtrip_check("-sqrt(-30meter^3)");
        roundtrip_check("-3^4");
        roundtrip_check("(-3)^4");
        roundtrip_check("atan2(2,3)");
        roundtrip_check("2^3!");
        roundtrip_check("-3!");
        roundtrip_check("(-3)!");
        roundtrip_check("megapoints");
        roundtrip_check("Foo { foo: 1 meter, bar: 1 second }");
    }

    #[test]
    fn pretty_print_dexpr() {
        roundtrip_check("unit z: Length");
        roundtrip_check("unit z: Length * Time");
        roundtrip_check("unit z: Length * Time^2");
        roundtrip_check("unit z: Length^-3 * Time^2");
        roundtrip_check("unit z: Length / Time");
        roundtrip_check("unit z: Length / Time^2");
        roundtrip_check("unit z: Length / Time^(-2)");
        roundtrip_check("unit z: Length / (Time * Mass)");
        roundtrip_check("unit z: Length^5 * Time^4 / (Time^2 * Mass^3)");
    }
}
