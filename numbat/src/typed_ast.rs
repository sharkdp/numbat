use indexmap::IndexMap;
use itertools::Itertools;

use crate::arithmetic::Exponent;
pub use crate::ast::{BinaryOperator, TypeExpression, UnaryOperator};
use crate::ast::{ProcedureKind, TypeAnnotation, TypeParameterBound};
use crate::dimension::DimensionRegistry;
use crate::pretty_print::escape_numbat_string;
use crate::traversal::{ForAllExpressions, ForAllTypeSchemes};
use crate::type_variable::TypeVariable;
use crate::typechecker::qualified_type::QualifiedType;
use crate::typechecker::type_scheme::TypeScheme;
use crate::typechecker::TypeCheckError;
use crate::{
    decorator::Decorator, markup::Markup, number::Number, prefix::Prefix,
    prefix_parser::AcceptsPrefix, pretty_print::PrettyPrint, span::Span,
};
use crate::{markup as m, BaseRepresentation, BaseRepresentationFactor};

/// Dimension type
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DTypeFactor {
    TVar(TypeVariable),
    TPar(String),
    BaseDimension(String),
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
    factors: Vec<DtypeFactorPower>,
}

impl DType {
    pub fn factors(&self) -> &[DtypeFactorPower] {
        &self.factors
    }

    pub fn into_factors(self) -> Vec<DtypeFactorPower> {
        self.factors
    }

    pub fn from_factors(factors: Vec<DtypeFactorPower>) -> DType {
        let mut dtype = DType { factors };
        dtype.canonicalize();
        dtype
    }

    pub fn scalar() -> DType {
        DType::from_factors(vec![])
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
            names.push(self.factors[0].0.name().to_string());
        }

        let base_representation = self.to_base_representation();
        names.extend(registry.get_derived_entry_names_for(&base_representation));
        match &names[..] {
            [] => self.pretty_print(),
            [single] => m::type_identifier(single.to_string()),
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
        DType::from_factors(vec![(DTypeFactor::TVar(v), Exponent::from_integer(1))])
    }

    pub fn from_type_parameter(name: String) -> DType {
        DType::from_factors(vec![(DTypeFactor::TPar(name), Exponent::from_integer(1))])
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
        DType::from_factors(vec![(
            DTypeFactor::TVar(TypeVariable::Quantified(i)),
            Exponent::from_integer(1),
        )])
    }

    pub fn base_dimension(name: &str) -> DType {
        DType::from_factors(vec![(
            DTypeFactor::BaseDimension(name.into()),
            Exponent::from_integer(1),
        )])
    }

    fn canonicalize(&mut self) {
        // Move all type-variable and tgen factors to the front, sort by name
        self.factors.sort_by(|(f1, _), (f2, _)| match (f1, f2) {
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
        for (f, n) in &self.factors {
            if let Some((last_f, last_n)) = new_factors.last_mut() {
                if f == last_f {
                    *last_n += n;
                    continue;
                }
            }
            new_factors.push((f.clone(), *n));
        }
        self.factors = new_factors;

        // Remove factors with zero exponent:
        self.factors
            .retain(|(_, n)| *n != Exponent::from_integer(0));
    }

    pub fn multiply(&self, other: &DType) -> DType {
        let mut factors = self.factors.clone();
        factors.extend(other.factors.clone());
        DType::from_factors(factors)
    }

    pub fn power(&self, n: Exponent) -> DType {
        let factors = self
            .factors
            .iter()
            .map(|(f, m)| (f.clone(), n * m))
            .collect();
        DType::from_factors(factors)
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

        for (f, n) in &self.factors {
            match f {
                DTypeFactor::TVar(TypeVariable::Quantified(i)) => {
                    factors.push((DTypeFactor::TVar(type_variables[*i].clone()), *n));
                }
                _ => {
                    factors.push((f.clone(), *n));
                }
            }
        }
        Self::from_factors(factors)
    }

    pub fn to_base_representation(&self) -> BaseRepresentation {
        let mut factors = vec![];
        for (f, n) in &self.factors {
            match f {
                DTypeFactor::BaseDimension(name) => {
                    factors.push(BaseRepresentationFactor(name.clone(), *n));
                }
                DTypeFactor::TVar(TypeVariable::Named(name)) => {
                    factors.push(BaseRepresentationFactor(name.clone(), *n));
                }
                DTypeFactor::TVar(TypeVariable::Quantified(_)) => {
                    unreachable!("Unexpected quantified type")
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
        DType::from_factors(factors)
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
    TVar(TypeVariable),
    TPar(String),
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
                let StructInfo { name, fields, .. } = &**info;
                write!(
                    f,
                    "{name} {{{}}}",
                    fields
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
            Type::TVar(TypeVariable::Named(name)) => m::type_identifier(name.clone()),
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
            Type::Struct(info) => m::type_identifier(info.name.clone()),
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
            t @ Type::Struct(_) => t.clone(),
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
    Fixed(String),
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
                    markup += m::text(format_specifiers.to_string());
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
    Scalar(Span, Number, TypeScheme),
    Identifier(Span, &'a str, TypeScheme),
    UnitIdentifier(Span, Prefix, String, String, TypeScheme),
    UnaryOperator(Span, UnaryOperator, Box<Expression<'a>>, TypeScheme),
    BinaryOperator(
        Option<Span>,
        BinaryOperator,
        Box<Expression<'a>>,
        Box<Expression<'a>>,
        TypeScheme,
    ),
    /// A special binary operator that has a DateTime as one (or both) of the operands
    BinaryOperatorForDate(
        Option<Span>,
        BinaryOperator,
        /// LHS must evaluate to a DateTime
        Box<Expression<'a>>,
        /// RHS can evaluate to a DateTime or a quantity of type Time
        Box<Expression<'a>>,
        TypeScheme,
    ),
    // A 'proper' function call
    FunctionCall(Span, Span, &'a str, Vec<Expression<'a>>, TypeScheme),
    // A call via a function object
    CallableCall(Span, Box<Expression<'a>>, Vec<Expression<'a>>, TypeScheme),
    Boolean(Span, bool),
    Condition(
        Span,
        Box<Expression<'a>>,
        Box<Expression<'a>>,
        Box<Expression<'a>>,
    ),
    String(Span, Vec<StringPart<'a>>),
    InstantiateStruct(Span, Vec<(&'a str, Expression<'a>)>, StructInfo),
    AccessField(
        Span,
        Span,
        Box<Expression<'a>>,
        &'a str,    // field name
        TypeScheme, // struct type
        TypeScheme, // resulting field type
    ),
    List(Span, Vec<Expression<'a>>, TypeScheme),
    TypedHole(Span, TypeScheme),
}

impl Expression<'_> {
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
            Expression::List(full_span, _, _) => *full_span,
            Expression::TypedHole(span, _) => *span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefineVariable<'a>(
    pub &'a str,
    pub Vec<Decorator<'a>>,
    pub Expression<'a>,
    pub Option<TypeAnnotation>,
    pub TypeScheme,
    pub Markup,
);

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'a> {
    Expression(Expression<'a>),
    DefineVariable(DefineVariable<'a>),
    DefineFunction(
        &'a str,
        Vec<Decorator<'a>>,                         // decorators
        Vec<(&'a str, Option<TypeParameterBound>)>, // type parameters
        Vec<(
            // parameters:
            Span,                   // span of the parameter
            &'a str,                // parameter name
            Option<TypeAnnotation>, // parameter type annotation
            Markup,                 // readable parameter type
        )>,
        Option<Expression<'a>>,  // function body
        Vec<DefineVariable<'a>>, // local variables
        TypeScheme,              // function type
        Option<TypeAnnotation>,  // return type annotation
        Markup,                  // readable return type
    ),
    DefineDimension(&'a str, Vec<TypeExpression>),
    DefineBaseUnit(
        &'a str,
        Vec<Decorator<'a>>,
        Option<TypeAnnotation>,
        TypeScheme,
    ),
    DefineDerivedUnit(
        &'a str,
        Expression<'a>,
        Vec<Decorator<'a>>,
        Option<TypeAnnotation>,
        TypeScheme,
        Markup,
    ),
    ProcedureCall(crate::ast::ProcedureKind, Vec<Expression<'a>>),
    DefineStruct(StructInfo),
}

impl Statement<'_> {
    pub fn as_expression(&self) -> Option<&Expression> {
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
            Statement::DefineVariable(DefineVariable(
                _,
                _,
                _,
                type_annotation,
                type_,
                readable_type,
            )) => {
                *readable_type = Self::create_readable_type(registry, type_, type_annotation, true);
            }
            Statement::DefineFunction(
                _,
                _,
                type_parameters,
                parameters,
                _,
                local_variables,
                fn_type,
                return_type_annotation,
                readable_return_type,
            ) => {
                let (fn_type, _) =
                    fn_type.instantiate_for_printing(Some(type_parameters.iter().map(|(n, _)| *n)));

                for DefineVariable(_, _, _, type_annotation, type_, readable_type) in
                    local_variables
                {
                    *readable_type =
                        Self::create_readable_type(registry, type_, type_annotation, false);
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
            Statement::DefineBaseUnit(_, _, _, _) => {}
            Statement::DefineDerivedUnit(_, _, _, type_annotation, type_, readable_type) => {
                *readable_type =
                    Self::create_readable_type(registry, type_, type_annotation, false);
            }
            Statement::ProcedureCall(_, _) => {}
            Statement::DefineStruct(_) => {}
        }
    }

    pub(crate) fn exponents_for(&mut self, tv: &TypeVariable) -> Vec<Exponent> {
        // TODO: things to not need to be mutable in this function
        let mut exponents = vec![];
        self.for_all_type_schemes(&mut |type_: &mut TypeScheme| {
            if let Type::Dimension(dtype) = type_.unsafe_as_concrete() {
                for (factor, exp) in dtype.factors {
                    if factor == DTypeFactor::TVar(tv.clone()) {
                        exponents.push(exp)
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
}

impl Expression<'_> {
    pub fn get_type(&self) -> Type {
        match self {
            Expression::Scalar(_, _, type_) => type_.unsafe_as_concrete(),
            Expression::Identifier(_, _, type_) => type_.unsafe_as_concrete(),
            Expression::UnitIdentifier(_, _, _, _, _type) => _type.unsafe_as_concrete(),
            Expression::UnaryOperator(_, _, _, type_) => type_.unsafe_as_concrete(),
            Expression::BinaryOperator(_, _, _, _, type_) => type_.unsafe_as_concrete(),
            Expression::BinaryOperatorForDate(_, _, _, _, type_, ..) => type_.unsafe_as_concrete(),
            Expression::FunctionCall(_, _, _, _, type_) => type_.unsafe_as_concrete(),
            Expression::CallableCall(_, _, _, type_) => type_.unsafe_as_concrete(),
            Expression::Boolean(_, _) => Type::Boolean,
            Expression::Condition(_, _, then_, _) => then_.get_type(),
            Expression::String(_, _) => Type::String,
            Expression::InstantiateStruct(_, _, info_) => Type::Struct(Box::new(info_.clone())),
            Expression::AccessField(_, _, _, _, _struct_type, field_type) => {
                field_type.unsafe_as_concrete()
            }
            Expression::List(_, _, element_type) => {
                Type::List(Box::new(element_type.unsafe_as_concrete()))
            }
            Expression::TypedHole(_, type_) => type_.unsafe_as_concrete(),
        }
    }

    pub fn get_type_scheme(&self) -> TypeScheme {
        match self {
            Expression::Scalar(_, _, type_) => type_.clone(),
            Expression::Identifier(_, _, type_) => type_.clone(),
            Expression::UnitIdentifier(_, _, _, _, type_) => type_.clone(),
            Expression::UnaryOperator(_, _, _, type_) => type_.clone(),
            Expression::BinaryOperator(_, _, _, _, type_) => type_.clone(),
            Expression::BinaryOperatorForDate(_, _, _, _, type_, ..) => type_.clone(),
            Expression::FunctionCall(_, _, _, _, type_) => type_.clone(),
            Expression::CallableCall(_, _, _, type_) => type_.clone(),
            Expression::Boolean(_, _) => TypeScheme::make_quantified(Type::Boolean),
            Expression::Condition(_, _, then_, _) => then_.get_type_scheme(),
            Expression::String(_, _) => TypeScheme::make_quantified(Type::String),
            Expression::InstantiateStruct(_, _, info_) => {
                TypeScheme::make_quantified(Type::Struct(Box::new(info_.clone())))
            }
            Expression::AccessField(_, _, _, _, _struct_type, field_type) => field_type.clone(),
            Expression::List(_, _, inner) => match inner {
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
                Decorator::Aliases(names) => {
                    m::decorator("@aliases")
                        + m::operator("(")
                        + Itertools::intersperse(
                            names.iter().map(|(name, accepts_prefix, _)| {
                                m::unit(name.to_string()) + accepts_prefix_markup(accepts_prefix)
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
                    m::type_identifier(tv.unsafe_name().to_string())
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
            m::identifier(name.to_string()) + m::operator(":") + m::space() + parameter_type
        }),
        m::operator(", "),
    )
    .sum();

    let markup_return_type =
        m::space() + m::operator("->") + m::space() + readable_return_type.clone();

    m::keyword("fn")
        + m::space()
        + m::identifier(function_name.to_string())
        + markup_type_parameters
        + m::operator("(")
        + markup_parameters
        + m::operator(")")
        + markup_return_type
}

impl PrettyPrint for Statement<'_> {
    fn pretty_print(&self) -> Markup {
        match self {
            Statement::DefineVariable(DefineVariable(
                identifier,
                _decs,
                expr,
                _annotation,
                _type,
                readable_type,
            )) => {
                m::keyword("let")
                    + m::space()
                    + m::identifier(identifier.to_string())
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
                local_variables,
                fn_type,
                _return_type_annotation,
                readable_return_type,
            ) => {
                let (fn_type, type_parameters) =
                    fn_type.instantiate_for_printing(Some(type_parameters.iter().map(|(n, _)| *n)));

                let mut pretty_local_variables = None;
                let mut first = true;
                if !local_variables.is_empty() {
                    let mut plv = m::empty();
                    for DefineVariable(
                        identifier,
                        _decs,
                        expr,
                        _annotation,
                        _type,
                        readable_type,
                    ) in local_variables
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
                            + m::identifier(identifier.to_string())
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
                m::keyword("dimension") + m::space() + m::type_identifier(identifier.to_string())
            }
            Statement::DefineDimension(identifier, dexprs) => {
                m::keyword("dimension")
                    + m::space()
                    + m::type_identifier(identifier.to_string())
                    + m::space()
                    + m::operator("=")
                    + m::space()
                    + Itertools::intersperse(
                        dexprs.iter().map(|d| d.pretty_print()),
                        m::space() + m::operator("=") + m::space(),
                    )
                    .sum()
            }
            Statement::DefineBaseUnit(identifier, decorators, annotation, type_) => {
                decorator_markup(decorators)
                    + m::keyword("unit")
                    + m::space()
                    + m::unit(identifier.to_string())
                    + m::operator(":")
                    + m::space()
                    + annotation
                        .as_ref()
                        .map(|a| a.pretty_print())
                        .unwrap_or(type_.pretty_print())
            }
            Statement::DefineDerivedUnit(
                identifier,
                expr,
                decorators,
                _annotation,
                _type,
                readable_type,
            ) => {
                decorator_markup(decorators)
                    + m::keyword("unit")
                    + m::space()
                    + m::unit(identifier.to_string())
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
        Expression::Scalar(..)
        | Expression::Identifier(..)
        | Expression::UnitIdentifier(..)
        | Expression::FunctionCall(..)
        | Expression::CallableCall(..)
        | Expression::Boolean(..)
        | Expression::String(..)
        | Expression::InstantiateStruct(..)
        | Expression::AccessField(..)
        | Expression::List(..)
        | Expression::TypedHole(_, _) => expr.pretty_print(),
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
                Expression::Scalar(_, s, _type_scalar),
                Expression::UnitIdentifier(_, prefix, _name, full_name, _type),
            ) => {
                // Fuse multiplication of a scalar and a unit to a quantity
                pretty_scalar(*s)
                    + m::space()
                    + m::unit(format!("{}{}", prefix.as_string_long(), full_name))
            }
            (Expression::Scalar(_, s, _), Expression::Identifier(_, name, _type)) => {
                // Fuse multiplication of a scalar and identifier
                pretty_scalar(*s) + m::space() + m::identifier(name.to_string())
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
        BinaryOperator::Power if matches!(rhs, Expression::Scalar(_, n, _type) if n.to_f64() == 2.0) => {
            with_parens(lhs) + m::operator("²")
        }
        BinaryOperator::Power if matches!(rhs, Expression::Scalar(_, n, _type) if n.to_f64() == 3.0) => {
            with_parens(lhs) + m::operator("³")
        }
        _ => with_parens(lhs) + op.pretty_print() + with_parens(rhs),
    }
}

impl PrettyPrint for Expression<'_> {
    fn pretty_print(&self) -> Markup {
        use Expression::*;

        match self {
            Scalar(_, n, _) => pretty_scalar(*n),
            Identifier(_, name, _type) => m::identifier(name.to_string()),
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
                m::identifier(name.to_string())
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
                                    m::identifier(n.to_string())
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
                expr.pretty_print() + m::operator(".") + m::identifier(attr.to_string())
            }
            List(_, elements, _) => {
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

    fn parse(code: &str) -> Statement {
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
