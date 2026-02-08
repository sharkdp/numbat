use std::sync::Arc;

use compact_str::{CompactString, ToCompactString, format_compact};
use indexmap::IndexMap;
use itertools::Itertools;

use crate::arithmetic::Exponent;
pub use crate::ast::{BinaryOperator, TypeExpression, UnaryOperator};
use crate::ast::{ProcedureKind, TypeAnnotation, TypeParameterBound};
use crate::dimension::DimensionRegistry;
use crate::pretty_print::escape_numbat_string;
use crate::traversal::{ForAllExpressions, ForAllTypeSchemes};
use crate::type_variable::TypeVariable;
use crate::typechecker::TypeCheckError;
use crate::typechecker::qualified_type::QualifiedType;
use crate::typechecker::type_scheme::TypeScheme;
use crate::{BaseRepresentation, BaseRepresentationFactor, markup as m};
use crate::{
    decorator::Decorator, markup::Markup, number::Number, prefix::Prefix,
    prefix_parser::AcceptsPrefix, pretty_print::PrettyPrint, span::Span,
};

/// Dimension type
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DTypeFactor {
    TVar(TypeVariable),
    TPar(CompactString),
    BaseDimension(CompactString),
}

impl DTypeFactor {
    pub fn name(&self) -> &str {
        match self {
            DTypeFactor::TVar(TypeVariable::Named(name)) => name,
            DTypeFactor::TVar(TypeVariable::Quantified(_)) => unreachable!(),
            DTypeFactor::TPar(name) => name,
            DTypeFactor::BaseDimension(name) => name,
        }
    }
}

type DtypeFactorPower = (DTypeFactor, Exponent);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DType {
    // Always in canonical form
    factors: Arc<Vec<DtypeFactorPower>>,
}

impl DType {
    pub fn factors(&self) -> &[DtypeFactorPower] {
        &self.factors
    }

    pub fn into_factors(self) -> Arc<Vec<DtypeFactorPower>> {
        self.factors
    }

    pub fn from_factors(factors: Arc<Vec<DtypeFactorPower>>) -> DType {
        let mut dtype = DType { factors };
        dtype.canonicalize();
        dtype
    }

    pub fn scalar() -> DType {
        DType::from_factors(Arc::new(vec![]))
    }

    pub fn is_scalar(&self) -> bool {
        self == &Self::scalar()
    }

    pub fn to_readable_type(&self, registry: &DimensionRegistry) -> m::Markup {
        if self.is_scalar() {
            return m::type_identifier("Scalar");
        }

        let mut names = vec![];

        if self.factors.len() == 1 && self.factors[0].1 == Exponent::from_integer(1) {
            names.push(self.factors[0].0.name().to_compact_string());
        }

        let base_representation = self.to_base_representation();
        names.extend(registry.get_derived_entry_names_for(&base_representation));
        match &names[..] {
            [] => self.pretty_print(),
            [single] => m::type_identifier(single.to_compact_string()),
            multiple => Itertools::intersperse(
                multiple.iter().cloned().map(m::type_identifier),
                m::dimmed(" or "),
            )
            .sum(),
        }
    }

    /// Is the current dimension type the Time dimension?
    ///
    /// This is special helper that's useful when dealing with DateTimes
    pub fn is_time_dimension(&self) -> bool {
        *self == DType::base_dimension("Time")
    }

    pub fn from_type_variable(v: TypeVariable) -> DType {
        DType::from_factors(Arc::new(vec![(
            DTypeFactor::TVar(v),
            Exponent::from_integer(1),
        )]))
    }

    pub fn from_type_parameter(name: CompactString) -> DType {
        DType::from_factors(Arc::new(vec![(
            DTypeFactor::TPar(name),
            Exponent::from_integer(1),
        )]))
    }

    pub fn deconstruct_as_single_type_variable(&self) -> Option<TypeVariable> {
        match &self.factors[..] {
            [(DTypeFactor::TVar(v), exponent)] if exponent == &Exponent::from_integer(1) => {
                Some(v.clone())
            }
            _ => None,
        }
    }

    pub fn from_tgen(i: usize) -> DType {
        DType::from_factors(Arc::new(vec![(
            DTypeFactor::TVar(TypeVariable::Quantified(i)),
            Exponent::from_integer(1),
        )]))
    }

    pub fn base_dimension(name: &str) -> DType {
        DType::from_factors(Arc::new(vec![(
            DTypeFactor::BaseDimension(name.into()),
            Exponent::from_integer(1),
        )]))
    }

    fn canonicalize(&mut self) {
        // Move all type-variable and tgen factors to the front, sort by name
        Arc::make_mut(&mut self.factors).sort_by(|(f1, _), (f2, _)| match (f1, f2) {
            (DTypeFactor::TVar(v1), DTypeFactor::TVar(v2)) => v1.cmp(v2),
            (DTypeFactor::TVar(_), _) => std::cmp::Ordering::Less,

            (DTypeFactor::BaseDimension(d1), DTypeFactor::BaseDimension(d2)) => d1.cmp(d2),
            (DTypeFactor::BaseDimension(_), DTypeFactor::TVar(_)) => std::cmp::Ordering::Greater,
            (DTypeFactor::BaseDimension(_), DTypeFactor::TPar(_)) => std::cmp::Ordering::Less,

            (DTypeFactor::TPar(p1), DTypeFactor::TPar(p2)) => p1.cmp(p2),
            (DTypeFactor::TPar(_), _) => std::cmp::Ordering::Greater,
        });

        // Merge powers of equal factors:
        let mut new_factors = Vec::new();
        for (f, n) in self.factors.iter() {
            if let Some((last_f, last_n)) = new_factors.last_mut()
                && f == last_f
            {
                *last_n += n;
                continue;
            }
            new_factors.push((f.clone(), *n));
        }

        // Remove factors with zero exponent:
        new_factors.retain(|(_, n)| *n != Exponent::from_integer(0));

        self.factors = Arc::new(new_factors);
    }

    pub fn multiply(&self, other: &DType) -> DType {
        let mut factors = self.factors.clone();
        Arc::make_mut(&mut factors).extend(other.factors.iter().cloned());
        DType::from_factors(factors)
    }

    pub fn power(&self, n: Exponent) -> DType {
        let factors = self
            .factors
            .iter()
            .map(|(f, m)| (f.clone(), n * m))
            .collect();
        DType::from_factors(Arc::new(factors))
    }

    pub fn inverse(&self) -> DType {
        self.power(-Exponent::from_integer(1))
    }

    pub fn divide(&self, other: &DType) -> DType {
        self.multiply(&other.inverse())
    }

    pub fn type_variables(&self, including_type_parameters: bool) -> Vec<TypeVariable> {
        let mut vars: Vec<_> = self
            .factors
            .iter()
            .filter_map(|(f, _)| match f {
                DTypeFactor::TVar(v) => Some(v.clone()),
                DTypeFactor::TPar(v) => {
                    if including_type_parameters {
                        Some(TypeVariable::new(v))
                    } else {
                        None
                    }
                }
                DTypeFactor::BaseDimension(_) => None,
            })
            .collect();
        vars.sort();
        vars.dedup();
        vars
    }

    pub fn contains(&self, name: &TypeVariable, including_type_parameters: bool) -> bool {
        self.type_variables(including_type_parameters)
            .contains(name)
    }

    pub fn split_first_factor(&self) -> Option<(&DtypeFactorPower, &[DtypeFactorPower])> {
        self.factors.split_first()
    }

    fn instantiate(&self, type_variables: &[TypeVariable]) -> DType {
        let mut factors = Vec::new();

        for (f, n) in self.factors.iter() {
            match f {
                DTypeFactor::TVar(TypeVariable::Quantified(i)) => {
                    factors.push((DTypeFactor::TVar(type_variables[*i].clone()), *n));
                }
                _ => {
                    factors.push((f.clone(), *n));
                }
            }
        }
        Self::from_factors(Arc::new(factors))
    }

    pub fn to_base_representation(&self) -> BaseRepresentation {
        let mut factors = vec![];
        for (f, n) in self.factors.iter() {
            match f {
                DTypeFactor::BaseDimension(name) => {
                    factors.push(BaseRepresentationFactor(name.clone(), *n));
                }
                DTypeFactor::TVar(TypeVariable::Named(name)) => {
                    factors.push(BaseRepresentationFactor(name.clone(), *n));
                }
                DTypeFactor::TVar(TypeVariable::Quantified(id)) => {
                    // Quantified type variables can appear during constraint solving
                    // before they're fully resolved
                    factors.push(BaseRepresentationFactor(format!("?{id}").into(), *n));
                }
                DTypeFactor::TPar(name) => {
                    factors.push(BaseRepresentationFactor(name.clone(), *n));
                }
            }
        }
        BaseRepresentation::from_factors(factors)
    }
}

impl PrettyPrint for DType {
    fn pretty_print(&self) -> Markup {
        self.to_base_representation().pretty_print()
    }
}

impl std::fmt::Display for DType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.pretty_print())
    }
}

impl From<BaseRepresentation> for DType {
    fn from(base_representation: BaseRepresentation) -> Self {
        let factors = base_representation
            .into_iter()
            .map(|BaseRepresentationFactor(name, exp)| (DTypeFactor::BaseDimension(name), exp))
            .collect();
        DType::from_factors(Arc::new(factors))
    }
}

/// Represents whether a struct is a generic definition or a concrete instance
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StructKind {
    /// Generic struct definition with type parameters (e.g., `struct Vec<D: Dim>`)
    Definition(Vec<(Span, CompactString, Option<TypeParameterBound>)>),
    /// Instantiated struct with concrete type arguments (e.g., `Vec<Length>`)
    Instance(Vec<Type>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructInfo {
    pub definition_span: Span,
    pub name: CompactString,
    pub kind: StructKind,
    pub fields: IndexMap<CompactString, (Span, Type)>,
}

/// A monomorphic type (no quantifiers).
///
/// - `TVar`: Unification variable, to be solved during type inference. Example: when
///   type-checking `fn f(x) = x + 1`, the parameter `x` gets a fresh `TVar(Named("T0"))`.
/// - `TPar`: User-written type parameter in a polymorphic definition. Example: in
///   `fn square<D: Dim>(x: D) -> D²`, the `D` in type annotations becomes `TPar("D")`.
///
/// During generalization, both `TVar` and `TPar` become `TVar(Quantified(i))` in a `TypeScheme`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    TVar(TypeVariable),
    TPar(CompactString),
    Dimension(DType),
    Boolean,
    String,
    DateTime,
    Fn(Vec<Type>, Box<Type>),
    Struct(Box<StructInfo>),
    List(Box<Type>),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::TVar(TypeVariable::Named(name)) => write!(f, "{name}"),
            Type::TVar(TypeVariable::Quantified(_)) => {
                unreachable!("Quantified types should not be printed")
            }
            Type::TPar(name) => write!(f, "{name}"),
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
            Type::Struct(info) => {
                write!(f, "{}", info.name)?;
                if let StructKind::Instance(type_args) = &info.kind
                    && !type_args.is_empty()
                {
                    write!(
                        f,
                        "<{}>",
                        type_args.iter().map(|t| t.to_string()).join(", ")
                    )?;
                }
                write!(
                    f,
                    " {{{}}}",
                    info.fields
                        .iter()
                        .map(|(n, (_, t))| n.to_string() + ": " + &t.to_string())
                        .join(", ")
                )
            }
            Type::List(element_type) => write!(f, "List<{element_type}>"),
        }
    }
}

impl PrettyPrint for Type {
    fn pretty_print(&self) -> Markup {
        match self {
            Type::TVar(TypeVariable::Named(name)) => m::type_identifier(name.to_compact_string()),
            Type::TVar(TypeVariable::Quantified(_)) => {
                unreachable!("Quantified types should not be printed")
            }
            Type::TPar(name) => m::type_identifier(name.clone()),
            Type::Dimension(d) => d.pretty_print(),
            Type::Boolean => m::type_identifier("Bool"),
            Type::String => m::type_identifier("String"),
            Type::DateTime => m::type_identifier("DateTime"),
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
            Type::Struct(info) => {
                let mut markup = m::type_identifier(info.name.clone());
                if let StructKind::Instance(type_args) = &info.kind
                    && !type_args.is_empty()
                {
                    markup += m::operator("<");
                    markup += Itertools::intersperse(
                        type_args.iter().map(|t| t.pretty_print()),
                        m::operator(",") + m::space(),
                    )
                    .sum();
                    markup += m::operator(">");
                }
                markup
            }
            Type::List(element_type) => {
                m::type_identifier("List")
                    + m::operator("<")
                    + element_type.pretty_print()
                    + m::operator(">")
            }
        }
    }
}

impl Type {
    pub fn to_readable_type(&self, registry: &DimensionRegistry) -> Markup {
        match self {
            Type::Dimension(d) => d.to_readable_type(registry),
            Type::Struct(info) => {
                let mut markup = m::type_identifier(info.name.clone());
                if let StructKind::Instance(type_args) = &info.kind
                    && !type_args.is_empty()
                {
                    markup += m::operator("<");
                    markup += Itertools::intersperse(
                        type_args.iter().map(|t| t.to_readable_type(registry)),
                        m::operator(",") + m::space(),
                    )
                    .sum();
                    markup += m::operator(">");
                }
                markup
            }
            Type::List(element_type) => {
                m::type_identifier("List")
                    + m::operator("<")
                    + element_type.to_readable_type(registry)
                    + m::operator(">")
            }
            _ => self.pretty_print(),
        }
    }

    pub fn scalar() -> Type {
        Type::Dimension(DType::scalar())
    }

    pub fn is_dtype(&self) -> bool {
        matches!(self, Type::Dimension(..))
    }

    pub fn is_fn_type(&self) -> bool {
        matches!(self, Type::Fn(..))
    }

    pub(crate) fn type_variables(&self, including_type_parameters: bool) -> Vec<TypeVariable> {
        match self {
            Type::TVar(v) => vec![v.clone()],
            Type::TPar(n) => {
                if including_type_parameters {
                    vec![TypeVariable::new(n)]
                } else {
                    vec![]
                }
            }
            Type::Dimension(d) => d.type_variables(including_type_parameters),
            Type::Boolean | Type::String | Type::DateTime => vec![],
            Type::Fn(param_types, return_type) => {
                let mut vars = return_type.type_variables(including_type_parameters);
                for param_type in param_types {
                    vars.extend(param_type.type_variables(including_type_parameters));
                }
                vars.sort();
                vars.dedup();
                vars
            }
            Type::Struct(info) => {
                let mut vars = vec![];
                for (_, (_, t)) in &info.fields {
                    vars.extend(t.type_variables(including_type_parameters));
                }
                vars
            }
            Type::List(element_type) => element_type.type_variables(including_type_parameters),
        }
    }

    pub(crate) fn contains(&self, x: &TypeVariable, including_type_parameters: bool) -> bool {
        self.type_variables(including_type_parameters).contains(x)
    }

    /// A type is called 'closed' if it does not change under substitutions (contains no unification variables)
    pub(crate) fn is_closed(&self) -> bool {
        self.type_variables(false).is_empty()
    }

    pub(crate) fn instantiate(&self, type_variables: &[TypeVariable]) -> Type {
        match self {
            Type::TVar(TypeVariable::Quantified(i)) => Type::TVar(type_variables[*i].clone()),
            Type::TVar(v) => Type::TVar(v.clone()),
            Type::TPar(n) => Type::TPar(n.clone()),
            Type::Dimension(d) => Type::Dimension(d.instantiate(type_variables)),
            Type::Boolean | Type::String | Type::DateTime => self.clone(),
            Type::Fn(param_types, return_type) => Type::Fn(
                param_types
                    .iter()
                    .map(|t| t.instantiate(type_variables))
                    .collect(),
                Box::new(return_type.instantiate(type_variables)),
            ),
            Type::Struct(info) => {
                let instantiated_fields = info
                    .fields
                    .iter()
                    .map(|(name, (span, field_type))| {
                        (
                            name.clone(),
                            (*span, field_type.instantiate(type_variables)),
                        )
                    })
                    .collect();
                let instantiated_kind = match &info.kind {
                    StructKind::Definition(params) => StructKind::Definition(params.clone()),
                    StructKind::Instance(type_args) => StructKind::Instance(
                        type_args
                            .iter()
                            .map(|t| t.instantiate(type_variables))
                            .collect(),
                    ),
                };
                Type::Struct(Box::new(StructInfo {
                    definition_span: info.definition_span,
                    name: info.name.clone(),
                    kind: instantiated_kind,
                    fields: instantiated_fields,
                }))
            }
            Type::List(element_type) => {
                Type::List(Box::new(element_type.instantiate(type_variables)))
            }
        }
    }

    pub(crate) fn is_scalar(&self) -> bool {
        match self {
            Type::Dimension(d) => d.is_scalar(),
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StringPart<'a> {
    Fixed(CompactString),
    Interpolation {
        span: Span,
        expr: Box<Expression<'a>>,
        format_specifiers: Option<&'a str>,
    },
}

impl PrettyPrint for StringPart<'_> {
    fn pretty_print(&self) -> Markup {
        match self {
            StringPart::Fixed(s) => m::string(escape_numbat_string(s)),
            StringPart::Interpolation {
                span: _,
                expr,
                format_specifiers,
            } => {
                let mut markup = m::operator("{") + expr.pretty_print();

                if let Some(format_specifiers) = format_specifiers {
                    markup += m::text(format_specifiers.to_compact_string());
                }

                markup += m::operator("}");

                markup
            }
        }
    }
}

impl PrettyPrint for &Vec<StringPart<'_>> {
    fn pretty_print(&self) -> Markup {
        m::operator("\"") + self.iter().map(|p| p.pretty_print()).sum() + m::operator("\"")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'a> {
    Scalar {
        span: Span,
        value: Number,
        type_scheme: TypeScheme,
    },
    Identifier {
        span: Span,
        name: &'a str,
        type_scheme: TypeScheme,
    },
    UnitIdentifier {
        span: Span,
        prefix: Prefix,
        name: CompactString,
        full_name: CompactString,
        type_scheme: TypeScheme,
    },
    UnaryOperator {
        span: Span,
        op: UnaryOperator,
        expr: Box<Expression<'a>>,
        type_scheme: TypeScheme,
    },
    BinaryOperator {
        op_span: Option<Span>,
        op: BinaryOperator,
        lhs: Box<Expression<'a>>,
        rhs: Box<Expression<'a>>,
        type_scheme: TypeScheme,
    },
    /// A special binary operator that has a DateTime as one (or both) of the operands
    BinaryOperatorForDate {
        op_span: Option<Span>,
        op: BinaryOperator,
        /// LHS must evaluate to a DateTime
        lhs: Box<Expression<'a>>,
        /// RHS can evaluate to a DateTime or a quantity of type Time
        rhs: Box<Expression<'a>>,
        type_scheme: TypeScheme,
    },
    /// A 'proper' function call
    FunctionCall {
        full_span: Span,
        ident_span: Span,
        name: &'a str,
        args: Vec<Expression<'a>>,
        type_scheme: TypeScheme,
    },
    /// A call via a function object
    CallableCall {
        full_span: Span,
        callable: Box<Expression<'a>>,
        args: Vec<Expression<'a>>,
        type_scheme: TypeScheme,
    },
    Boolean(Span, bool),
    Condition {
        span: Span,
        condition: Box<Expression<'a>>,
        then_expr: Box<Expression<'a>>,
        else_expr: Box<Expression<'a>>,
    },
    String(Span, Vec<StringPart<'a>>),
    InstantiateStruct {
        span: Span,
        fields: Vec<(&'a str, Expression<'a>)>,
        struct_info: StructInfo,
    },
    AccessField {
        full_span: Span,
        ident_span: Span,
        expr: Box<Expression<'a>>,
        field_name: &'a str,
        struct_type: TypeScheme,
        field_type: TypeScheme,
    },
    List {
        span: Span,
        elements: Vec<Expression<'a>>,
        type_scheme: TypeScheme,
    },
    TypedHole(Span, TypeScheme),
}

impl Expression<'_> {
    pub fn full_span(&self) -> Span {
        match self {
            Expression::Scalar { span, .. } => *span,
            Expression::Identifier { span, .. } => *span,
            Expression::UnitIdentifier { span, .. } => *span,
            Expression::UnaryOperator { span, expr, .. } => span.extend(&expr.full_span()),
            Expression::BinaryOperator {
                op_span, lhs, rhs, ..
            } => {
                let mut span = lhs.full_span().extend(&rhs.full_span());
                if let Some(op_span) = op_span {
                    span = span.extend(op_span);
                }
                span
            }
            Expression::BinaryOperatorForDate {
                op_span, lhs, rhs, ..
            } => {
                let mut span = lhs.full_span().extend(&rhs.full_span());
                if let Some(op_span) = op_span {
                    span = span.extend(op_span);
                }
                span
            }
            Expression::FunctionCall { full_span, .. } => *full_span,
            Expression::CallableCall { full_span, .. } => *full_span,
            Expression::Boolean(span, _) => *span,
            Expression::Condition {
                span, else_expr, ..
            } => span.extend(&else_expr.full_span()),
            Expression::String(span, _) => *span,
            Expression::InstantiateStruct { span, .. } => *span,
            Expression::AccessField { full_span, .. } => *full_span,
            Expression::List { span, .. } => *span,
            Expression::TypedHole(span, _) => *span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefineVariable<'a> {
    pub name: &'a str,
    pub decorators: Vec<Decorator<'a>>,
    pub expr: Expression<'a>,
    pub type_annotation: Option<TypeAnnotation>,
    pub type_scheme: TypeScheme,
    pub readable_type: Markup,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'a> {
    Expression(Expression<'a>),
    DefineVariable(DefineVariable<'a>),
    DefineFunction {
        function_name: &'a str,
        decorators: Vec<Decorator<'a>>,
        type_parameters: Vec<(&'a str, Option<TypeParameterBound>)>,
        parameters: Vec<(
            Span,                   // span of the parameter
            &'a str,                // parameter name
            Option<TypeAnnotation>, // parameter type annotation
            Markup,                 // readable parameter type
        )>,
        body: Option<Expression<'a>>,
        local_variables: Vec<DefineVariable<'a>>,
        fn_type: TypeScheme,
        return_type_annotation: Option<TypeAnnotation>,
        readable_return_type: Markup,
    },
    DefineDimension(&'a str, Vec<TypeExpression>),
    DefineBaseUnit {
        name: &'a str,
        identifier_span: Span,
        decorators: Vec<Decorator<'a>>,
        type_annotation: Option<TypeAnnotation>,
        type_scheme: TypeScheme,
    },
    DefineDerivedUnit {
        name: &'a str,
        identifier_span: Span,
        expr: Expression<'a>,
        decorators: Vec<Decorator<'a>>,
        type_annotation: Option<TypeAnnotation>,
        type_scheme: TypeScheme,
        readable_type: Markup,
    },
    ProcedureCall {
        kind: ProcedureKind,
        span: Span,
        args: Vec<Expression<'a>>,
    },
    DefineStruct(StructInfo),
}

impl Statement<'_> {
    pub fn as_expression(&self) -> Option<&Expression<'_>> {
        if let Self::Expression(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub(crate) fn generalize_types(&mut self, dtype_variables: &[TypeVariable]) {
        self.for_all_type_schemes(&mut |type_: &mut TypeScheme| type_.generalize(dtype_variables));
    }

    fn create_readable_type(
        registry: &DimensionRegistry,
        type_: &TypeScheme,
        annotation: &Option<TypeAnnotation>,
        with_quantifiers: bool,
    ) -> Markup {
        if let Some(annotation) = annotation {
            annotation.pretty_print()
        } else {
            type_.to_readable_type(registry, with_quantifiers)
        }
    }

    pub(crate) fn update_readable_types(&mut self, registry: &DimensionRegistry) {
        match self {
            Statement::Expression(_) => {}
            Statement::DefineVariable(DefineVariable {
                type_annotation,
                type_scheme,
                readable_type,
                ..
            }) => {
                *readable_type =
                    Self::create_readable_type(registry, type_scheme, type_annotation, true);
            }
            Statement::DefineFunction {
                type_parameters,
                parameters,
                local_variables,
                fn_type,
                return_type_annotation,
                readable_return_type,
                ..
            } => {
                let (fn_type, _) =
                    fn_type.instantiate_for_printing(Some(type_parameters.iter().map(|(n, _)| *n)));

                for DefineVariable {
                    type_annotation,
                    type_scheme,
                    readable_type,
                    ..
                } in local_variables
                {
                    *readable_type =
                        Self::create_readable_type(registry, type_scheme, type_annotation, false);
                }

                let Type::Fn(parameter_types, return_type) = fn_type.inner else {
                    unreachable!("Expected a function type")
                };

                *readable_return_type = Self::create_readable_type(
                    registry,
                    &TypeScheme::concrete(*return_type),
                    return_type_annotation,
                    false,
                );

                for ((_, _, type_annotation, readable_parameter_type), parameter_type) in
                    parameters.iter_mut().zip(parameter_types.iter())
                {
                    *readable_parameter_type = Self::create_readable_type(
                        registry,
                        &TypeScheme::concrete(parameter_type.clone()),
                        type_annotation,
                        false,
                    );
                }
            }
            Statement::DefineDimension(_, _) => {}
            Statement::DefineBaseUnit { .. } => {}
            Statement::DefineDerivedUnit {
                type_annotation,
                type_scheme,
                readable_type,
                ..
            } => {
                *readable_type =
                    Self::create_readable_type(registry, type_scheme, type_annotation, false);
            }
            Statement::ProcedureCall { .. } => {}
            Statement::DefineStruct(_) => {}
        }
    }

    pub(crate) fn exponents_for(&mut self, tv: &TypeVariable) -> Vec<Exponent> {
        // TODO: things to not need to be mutable in this function
        let mut exponents = vec![];
        self.for_all_type_schemes(&mut |type_: &mut TypeScheme| {
            if let Type::Dimension(dtype) = type_.unsafe_as_concrete() {
                for (factor, exp) in dtype.factors.iter() {
                    if factor == &DTypeFactor::TVar(tv.clone()) {
                        exponents.push(*exp)
                    }
                }
            }
        });
        exponents
    }

    pub(crate) fn find_typed_hole(
        &self,
    ) -> Result<Option<(Span, TypeScheme)>, Box<TypeCheckError>> {
        let mut hole = None;
        let mut found_multiple_holes = false;
        self.for_all_expressions(&mut |expr| {
            if let Expression::TypedHole(span, type_) = expr {
                if hole.is_some() {
                    found_multiple_holes = true;
                }
                hole = Some((*span, type_.clone()))
            }
        });

        if found_multiple_holes {
            Err(Box::new(TypeCheckError::MultipleTypedHoles(
                hole.unwrap().0,
            )))
        } else {
            Ok(hole)
        }
    }

    /// Returns an iterator over local bindings (name, type) in the statement.
    /// This includes local variables in `where` clauses and function parameters.
    pub(crate) fn local_bindings(&self) -> Vec<(&str, TypeScheme)> {
        match self {
            Statement::DefineFunction {
                parameters,
                local_variables,
                fn_type,
                ..
            } => {
                let mut bindings = Vec::new();

                if let TypeScheme::Concrete(Type::Fn(param_types, _))
                | TypeScheme::Quantified(
                    _,
                    crate::typechecker::qualified_type::QualifiedType {
                        inner: Type::Fn(param_types, _),
                        ..
                    },
                ) = fn_type
                {
                    for ((_, param_name, _, _), param_type) in
                        parameters.iter().zip(param_types.iter())
                    {
                        bindings
                            .push((*param_name, TypeScheme::make_quantified(param_type.clone())));
                    }
                }

                for DefineVariable {
                    name, type_scheme, ..
                } in local_variables
                {
                    bindings.push((*name, type_scheme.clone()));
                }

                bindings
            }
            _ => Vec::new(),
        }
    }
}

impl Expression<'_> {
    pub fn get_type(&self) -> Type {
        match self {
            Expression::Scalar { type_scheme, .. } => type_scheme.unsafe_as_concrete(),
            Expression::Identifier { type_scheme, .. } => type_scheme.unsafe_as_concrete(),
            Expression::UnitIdentifier { type_scheme, .. } => type_scheme.unsafe_as_concrete(),
            Expression::UnaryOperator { type_scheme, .. } => type_scheme.unsafe_as_concrete(),
            Expression::BinaryOperator { type_scheme, .. } => type_scheme.unsafe_as_concrete(),
            Expression::BinaryOperatorForDate { type_scheme, .. } => {
                type_scheme.unsafe_as_concrete()
            }
            Expression::FunctionCall { type_scheme, .. } => type_scheme.unsafe_as_concrete(),
            Expression::CallableCall { type_scheme, .. } => type_scheme.unsafe_as_concrete(),
            Expression::Boolean(_, _) => Type::Boolean,
            Expression::Condition { then_expr, .. } => then_expr.get_type(),
            Expression::String(_, _) => Type::String,
            Expression::InstantiateStruct { struct_info, .. } => {
                Type::Struct(Box::new(struct_info.clone()))
            }
            Expression::AccessField { field_type, .. } => field_type.unsafe_as_concrete(),
            Expression::List { type_scheme, .. } => {
                Type::List(Box::new(type_scheme.unsafe_as_concrete()))
            }
            Expression::TypedHole(_, type_) => type_.unsafe_as_concrete(),
        }
    }

    pub fn get_type_scheme(&self) -> TypeScheme {
        match self {
            Expression::Scalar { type_scheme, .. } => type_scheme.clone(),
            Expression::Identifier { type_scheme, .. } => type_scheme.clone(),
            Expression::UnitIdentifier { type_scheme, .. } => type_scheme.clone(),
            Expression::UnaryOperator { type_scheme, .. } => type_scheme.clone(),
            Expression::BinaryOperator { type_scheme, .. } => type_scheme.clone(),
            Expression::BinaryOperatorForDate { type_scheme, .. } => type_scheme.clone(),
            Expression::FunctionCall { type_scheme, .. } => type_scheme.clone(),
            Expression::CallableCall { type_scheme, .. } => type_scheme.clone(),
            Expression::Boolean(_, _) => TypeScheme::make_quantified(Type::Boolean),
            Expression::Condition { then_expr, .. } => then_expr.get_type_scheme(),
            Expression::String(_, _) => TypeScheme::make_quantified(Type::String),
            Expression::InstantiateStruct { struct_info, .. } => {
                TypeScheme::make_quantified(Type::Struct(Box::new(struct_info.clone())))
            }
            Expression::AccessField { field_type, .. } => field_type.clone(),
            Expression::List { type_scheme, .. } => match type_scheme {
                TypeScheme::Concrete(t) => TypeScheme::Concrete(Type::List(Box::new(t.clone()))),
                TypeScheme::Quantified(ngen, qt) => TypeScheme::Quantified(
                    *ngen,
                    crate::typechecker::qualified_type::QualifiedType {
                        inner: Type::List(Box::new(qt.inner.clone())),
                        bounds: qt.bounds.clone(),
                    },
                ),
            },
            Expression::TypedHole(_, type_) => type_.clone(),
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
                Decorator::Abbreviation => m::decorator("@abbreviation"),
                Decorator::Aliases(names) => {
                    m::decorator("@aliases")
                        + m::operator("(")
                        + Itertools::intersperse(
                            names.iter().map(|(name, accepts_prefix, _)| {
                                m::unit(name.to_compact_string())
                                    + accepts_prefix_markup(accepts_prefix)
                            }),
                            m::operator(", "),
                        )
                        .sum()
                        + m::operator(")")
                }
                Decorator::Url(url) => {
                    m::decorator("@url")
                        + m::operator("(")
                        + m::string(url.clone())
                        + m::operator(")")
                }
                Decorator::Name(name) => {
                    m::decorator("@name")
                        + m::operator("(")
                        + m::string(name.clone())
                        + m::operator(")")
                }
                Decorator::Description(description) => {
                    m::decorator("@description")
                        + m::operator("(")
                        + m::string(description.clone())
                        + m::operator(")")
                }
                Decorator::Example(example_code, example_description) => {
                    m::decorator("@example")
                        + m::operator("(")
                        + m::string(example_code.clone())
                        + if let Some(example_description) = example_description {
                            m::operator(", ") + m::string(example_description.clone())
                        } else {
                            m::empty()
                        }
                        + m::operator(")")
                }
            }
            + m::nl();
    }
    markup_decorators
}

pub fn pretty_print_function_signature<'a>(
    function_name: &str,
    fn_type: &QualifiedType,
    type_parameters: &[TypeVariable],
    parameters: impl Iterator<
        Item = (
            &'a str, // parameter name
            Markup,  // readable parameter type
        ),
    >,
    readable_return_type: &Markup,
) -> Markup {
    let markup_type_parameters = if type_parameters.is_empty() {
        m::empty()
    } else {
        m::operator("<")
            + Itertools::intersperse(
                type_parameters.iter().map(|tv| {
                    m::type_identifier(tv.unsafe_name().to_compact_string())
                        + if fn_type.bounds.is_dtype_bound(tv) {
                            m::operator(":") + m::space() + m::type_identifier("Dim")
                        } else {
                            m::empty()
                        }
                }),
                m::operator(", "),
            )
            .sum()
            + m::operator(">")
    };

    let markup_parameters = Itertools::intersperse(
        parameters.map(|(name, parameter_type)| {
            m::identifier(name.to_compact_string()) + m::operator(":") + m::space() + parameter_type
        }),
        m::operator(", "),
    )
    .sum();

    let markup_return_type =
        m::space() + m::operator("->") + m::space() + readable_return_type.clone();

    m::keyword("fn")
        + m::space()
        + m::identifier(function_name.to_compact_string())
        + markup_type_parameters
        + m::operator("(")
        + markup_parameters
        + m::operator(")")
        + markup_return_type
}

impl PrettyPrint for Statement<'_> {
    fn pretty_print(&self) -> Markup {
        match self {
            Statement::DefineVariable(DefineVariable {
                name,
                expr,
                readable_type,
                ..
            }) => {
                m::keyword("let")
                    + m::space()
                    + m::identifier(name.to_compact_string())
                    + m::operator(":")
                    + m::space()
                    + readable_type.clone()
                    + m::space()
                    + m::operator("=")
                    + m::space()
                    + expr.pretty_print()
            }
            Statement::DefineFunction {
                function_name,
                type_parameters,
                parameters,
                body,
                local_variables,
                fn_type,
                readable_return_type,
                ..
            } => {
                let (fn_type, type_parameters) =
                    fn_type.instantiate_for_printing(Some(type_parameters.iter().map(|(n, _)| *n)));

                let mut pretty_local_variables = None;
                let mut first = true;
                if !local_variables.is_empty() {
                    let mut plv = m::empty();
                    for DefineVariable {
                        name,
                        expr,
                        readable_type,
                        ..
                    } in local_variables
                    {
                        let introducer_keyword = if first {
                            first = false;
                            m::space() + m::space() + m::keyword("where")
                        } else {
                            m::space() + m::space() + m::space() + m::space() + m::keyword("and")
                        };

                        plv += m::nl()
                            + introducer_keyword
                            + m::space()
                            + m::identifier(name.to_compact_string())
                            + m::operator(":")
                            + m::space()
                            + readable_type.clone()
                            + m::space()
                            + m::operator("=")
                            + m::space()
                            + expr.pretty_print();
                    }
                    pretty_local_variables = Some(plv);
                }

                pretty_print_function_signature(
                    function_name,
                    &fn_type,
                    &type_parameters,
                    parameters
                        .iter()
                        .map(|(_, name, _, type_)| (*name, type_.clone())),
                    readable_return_type,
                ) + body
                    .as_ref()
                    .map(|e| m::space() + m::operator("=") + m::space() + e.pretty_print())
                    .unwrap_or_default()
                    + pretty_local_variables.unwrap_or_default()
            }
            Statement::Expression(expr) => expr.pretty_print(),
            Statement::DefineDimension(identifier, dexprs) if dexprs.is_empty() => {
                m::keyword("dimension")
                    + m::space()
                    + m::type_identifier(identifier.to_compact_string())
            }
            Statement::DefineDimension(identifier, dexprs) => {
                m::keyword("dimension")
                    + m::space()
                    + m::type_identifier(identifier.to_compact_string())
                    + m::space()
                    + m::operator("=")
                    + m::space()
                    + Itertools::intersperse(
                        dexprs.iter().map(|d| d.pretty_print()),
                        m::space() + m::operator("=") + m::space(),
                    )
                    .sum()
            }
            Statement::DefineBaseUnit {
                name,
                decorators,
                type_annotation,
                type_scheme,
                ..
            } => {
                decorator_markup(decorators)
                    + m::keyword("unit")
                    + m::space()
                    + m::unit(name.to_compact_string())
                    + m::operator(":")
                    + m::space()
                    + type_annotation
                        .as_ref()
                        .map(|a: &TypeAnnotation| a.pretty_print())
                        .unwrap_or(type_scheme.pretty_print())
            }
            Statement::DefineDerivedUnit {
                name,
                expr,
                decorators,
                readable_type,
                ..
            } => {
                decorator_markup(decorators)
                    + m::keyword("unit")
                    + m::space()
                    + m::unit(name.to_compact_string())
                    + m::operator(":")
                    + m::space()
                    + readable_type.clone()
                    + m::space()
                    + m::operator("=")
                    + m::space()
                    + expr.pretty_print()
            }
            Statement::ProcedureCall { kind, args, .. } => {
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
                                    m::identifier(n.clone())
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
        Expression::Scalar { .. }
        | Expression::Identifier { .. }
        | Expression::UnitIdentifier { .. }
        | Expression::FunctionCall { .. }
        | Expression::CallableCall { .. }
        | Expression::Boolean(..)
        | Expression::String(..)
        | Expression::InstantiateStruct { .. }
        | Expression::AccessField { .. }
        | Expression::List { .. }
        | Expression::TypedHole(_, _) => expr.pretty_print(),
        Expression::UnaryOperator { .. }
        | Expression::BinaryOperator { .. }
        | Expression::BinaryOperatorForDate { .. }
        | Expression::Condition { .. } => m::operator("(") + expr.pretty_print() + m::operator(")"),
    }
}

/// Add parens, if needed -- liberal version, can not be used for exponentiation.
fn with_parens_liberal(expr: &Expression) -> Markup {
    match expr {
        Expression::BinaryOperator {
            op: BinaryOperator::Mul,
            lhs,
            rhs,
            ..
        } if matches!(**lhs, Expression::Scalar { .. })
            && matches!(**rhs, Expression::UnitIdentifier { .. }) =>
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
                Expression::Scalar { value: s, .. },
                Expression::UnitIdentifier {
                    prefix, full_name, ..
                },
            ) => {
                // Fuse multiplication of a scalar and a unit to a quantity
                pretty_scalar(*s)
                    + m::space()
                    + m::unit(format_compact!("{}{}", prefix.as_string_long(), full_name))
            }
            (Expression::Scalar { value: s, .. }, Expression::Identifier { name, .. }) => {
                // Fuse multiplication of a scalar and identifier
                pretty_scalar(*s) + m::space() + m::identifier(name.to_compact_string())
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
        BinaryOperator::Power if matches!(rhs, Expression::Scalar { value, .. } if value.to_f64() == 2.0) => {
            with_parens(lhs) + m::operator("²")
        }
        BinaryOperator::Power if matches!(rhs, Expression::Scalar { value, .. } if value.to_f64() == 3.0) => {
            with_parens(lhs) + m::operator("³")
        }
        _ => with_parens(lhs) + op.pretty_print() + with_parens(rhs),
    }
}

impl PrettyPrint for Expression<'_> {
    fn pretty_print(&self) -> Markup {
        use Expression::*;

        match self {
            Scalar { value, .. } => pretty_scalar(*value),
            Identifier { name, .. } => m::identifier(name.to_compact_string()),
            UnitIdentifier {
                prefix, full_name, ..
            } => m::unit(format_compact!("{}{}", prefix.as_string_long(), full_name)),
            UnaryOperator {
                op: self::UnaryOperator::Negate,
                expr,
                ..
            } => m::operator("-") + with_parens(expr),
            UnaryOperator {
                op: self::UnaryOperator::Factorial(order),
                expr,
                ..
            } => with_parens(expr) + (0..order.get()).map(|_| m::operator("!")).sum(),
            UnaryOperator {
                op: self::UnaryOperator::LogicalNeg,
                expr,
                ..
            } => m::operator("!") + with_parens(expr),
            BinaryOperator { op, lhs, rhs, .. } => pretty_print_binop(op, lhs, rhs),
            BinaryOperatorForDate { op, lhs, rhs, .. } => pretty_print_binop(op, lhs, rhs),
            FunctionCall { name, args, .. } => {
                // Special case: render special temperature conversion functions in their sugar form:
                if args.len() == 1 {
                    // from_celsius(x) / from_fahrenheit(x) -> "… °C" / "… °F"
                    if *name == "from_celsius" {
                        return with_parens_liberal(&args[0]) + m::space() + m::unit("°C");
                    } else if *name == "from_fahrenheit" {
                        return with_parens_liberal(&args[0]) + m::space() + m::unit("°F");
                    }
                    // °C(x) / celsius(x) / degree_celsius(x) -> "x -> °C"
                    // °F(x) / fahrenheit(x) / degree_fahrenheit(x) -> "x -> °F"
                    if *name == "°C" || *name == "celsius" || *name == "degree_celsius" {
                        return with_parens_liberal(&args[0])
                            + m::space()
                            + m::operator("->")
                            + m::space()
                            + m::unit("°C");
                    } else if *name == "°F" || *name == "fahrenheit" || *name == "degree_fahrenheit"
                    {
                        return with_parens_liberal(&args[0])
                            + m::space()
                            + m::operator("->")
                            + m::space()
                            + m::unit("°F");
                    }
                }

                m::identifier(name.to_compact_string())
                    + m::operator("(")
                    + itertools::Itertools::intersperse(
                        args.iter().map(|e: &Expression| e.pretty_print()),
                        m::operator(",") + m::space(),
                    )
                    .sum()
                    + m::operator(")")
            }
            CallableCall {
                callable: expr,
                args,
                ..
            } => {
                // See above
                if args.len() == 1
                    && let Expression::Identifier { name, .. } = expr.as_ref()
                {
                    if *name == "°C" || *name == "celsius" || *name == "degree_celsius" {
                        return with_parens_liberal(&args[0])
                            + m::space()
                            + m::operator("->")
                            + m::space()
                            + m::unit("°C");
                    } else if *name == "°F" || *name == "fahrenheit" || *name == "degree_fahrenheit"
                    {
                        return with_parens_liberal(&args[0])
                            + m::space()
                            + m::operator("->")
                            + m::space()
                            + m::unit("°F");
                    }
                }

                expr.pretty_print()
                    + m::operator("(")
                    + itertools::Itertools::intersperse(
                        args.iter().map(|e: &Expression| e.pretty_print()),
                        m::operator(",") + m::space(),
                    )
                    .sum()
                    + m::operator(")")
            }
            Boolean(_, val) => val.pretty_print(),
            String(_, parts) => parts.pretty_print(),
            Condition {
                condition,
                then_expr,
                else_expr,
                ..
            } => {
                m::keyword("if")
                    + m::space()
                    + with_parens(condition)
                    + m::space()
                    + m::keyword("then")
                    + m::space()
                    + with_parens(then_expr)
                    + m::space()
                    + m::keyword("else")
                    + m::space()
                    + with_parens(else_expr)
            }
            InstantiateStruct {
                fields,
                struct_info,
                ..
            } => {
                m::type_identifier(struct_info.name.clone())
                    + m::space()
                    + m::operator("{")
                    + if fields.is_empty() {
                        m::empty()
                    } else {
                        m::space()
                            + itertools::Itertools::intersperse(
                                fields.iter().map(|(n, e)| {
                                    m::identifier(n.to_compact_string())
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
            AccessField {
                expr, field_name, ..
            } => {
                expr.pretty_print()
                    + m::operator(".")
                    + m::identifier(field_name.to_compact_string())
            }
            List { elements, .. } => {
                m::operator("[")
                    + itertools::Itertools::intersperse(
                        elements.iter().map(|e| e.pretty_print()),
                        m::operator(",") + m::space(),
                    )
                    .sum()
                    + m::operator("]")
            }
            TypedHole(_, _) => m::operator("?"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::ReplaceSpans;
    use crate::markup::{Formatter, PlainTextFormatter};
    use crate::prefix_transformer::Transformer;

    fn parse(code: &str) -> Statement<'_> {
        let statements = crate::parser::parse(
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

                 struct Foo {foo: Length, bar: Time}

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
                 let _prefixed = 1",
            0,
        )
        .unwrap()
        .into_iter()
        .chain(crate::parser::parse(code, 0).unwrap());

        let mut transformer = Transformer::new();
        let transformed_statements = transformer.transform(statements).unwrap().replace_spans();

        crate::typechecker::TypeChecker::default()
            .check(&transformed_statements)
            .unwrap()
            .last()
            .unwrap()
            .clone()
    }

    fn pretty_print(stmt: &Statement) -> CompactString {
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
        roundtrip_check("\"foo\"");
        roundtrip_check("\"newline: \\n\"");
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
