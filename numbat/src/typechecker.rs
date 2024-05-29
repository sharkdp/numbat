use std::{
    collections::{HashMap, HashSet},
    error::Error,
    fmt,
};

use crate::{
    arithmetic::{pretty_exponent, Exponent, Power, Rational},
    ast::ProcedureKind,
};
use crate::{ast, decorator, ffi, suggestion};
use crate::{ast::StringPart, span::Span};
use crate::{
    ast::TypeAnnotation,
    registry::{BaseRepresentation, BaseRepresentationFactor, RegistryError},
};
use crate::{dimension::DimensionRegistry, typed_ast::DType};
use crate::{ffi::ArityRange, typed_ast::Expression};
use crate::{
    name_resolution::Namespace,
    typed_ast::{self, StructInfo, Type},
    NameResolutionError,
};
use crate::{name_resolution::LAST_RESULT_IDENTIFIERS, pretty_print::PrettyPrint};

use ast::{BinaryOperator, TypeExpression};
use itertools::Itertools;
use num_traits::{CheckedAdd, CheckedDiv, CheckedMul, CheckedSub, FromPrimitive, Zero};
use thiserror::Error;
use unicode_width::UnicodeWidthStr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IncompatibleDimensionsError {
    pub span_operation: Span,
    pub operation: String,
    pub span_expected: Span,
    pub expected_name: &'static str,
    pub expected_type: BaseRepresentation,
    pub expected_dimensions: Vec<String>,
    pub span_actual: Span,
    pub actual_name: &'static str,
    pub actual_name_for_fix: &'static str,
    pub actual_type: BaseRepresentation,
    pub actual_dimensions: Vec<String>,
}

fn pad(a: &str, b: &str) -> (String, String) {
    let max_length = a.width().max(b.width());

    (
        format!("{a: <width$}", width = max_length),
        format!("{b: <width$}", width = max_length),
    )
}

fn suggested_fix(
    expected_type: &BaseRepresentation,
    actual_type: &BaseRepresentation,
    expression_to_change: &str,
) -> Option<String> {
    // Heuristic 1: if actual_type == 1 / expected_type, suggest
    // to invert the 'actual' expression:
    if actual_type == &expected_type.clone().invert() {
        return Some(format!("invert the {expression_to_change}"));
    }

    // Heuristic 2: compute the "missing" factor between the expected
    // and the actual type. Suggest to multiply / divide with the
    // appropriate delta.
    let delta_type = expected_type.clone() / actual_type.clone();

    let num_factors = delta_type.iter().count();
    if num_factors > 1 {
        return None; // Do not suggest fixes with complicated dimensions
    }

    let exponent_sum: Rational = delta_type.iter().map(|a| a.1).sum();

    let (action, delta_type) = if exponent_sum >= Rational::zero() {
        ("multiply", delta_type)
    } else {
        ("divide", delta_type.invert())
    };

    Some(format!(
        "{action} the {expression_to_change} by a `{delta_type}` factor"
    ))
}

impl fmt::Display for IncompatibleDimensionsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let have_common_factors = self
            .expected_type
            .iter()
            .any(|f| self.actual_type.iter().map(|f| &f.0).contains(&f.0));

        let (mut expected_result_string, mut actual_result_string) = if !have_common_factors
            || (self.expected_type.iter().count() == 1 && self.actual_type.iter().count() == 1)
        {
            pad(
                &self.expected_type.to_string(),
                &self.actual_type.to_string(),
            )
        } else {
            let format_factor =
                |name: &str, exponent: &Exponent| format!(" × {name}{}", pretty_exponent(exponent));

            let mut shared_factors = HashMap::<&String, (Exponent, Exponent)>::new();
            let mut expected_factors = HashMap::<&String, Exponent>::new();
            let mut actual_factors = HashMap::<&String, Exponent>::new();

            for BaseRepresentationFactor(name, expected_exponent) in self.expected_type.iter() {
                if let Some(BaseRepresentationFactor(_, actual_exponent)) =
                    self.actual_type.iter().find(|f| *name == f.0)
                {
                    shared_factors.insert(name, (*expected_exponent, *actual_exponent));
                } else {
                    expected_factors.insert(name, *expected_exponent);
                }
            }

            for BaseRepresentationFactor(name, exponent) in self.actual_type.iter() {
                if !shared_factors.contains_key(&name) {
                    actual_factors.insert(name, *exponent);
                }
            }

            let mut expected_result_string = String::new();
            let mut actual_result_string = String::new();

            for (name, (exp1, exp2)) in shared_factors
                .iter()
                .sorted_unstable_by_key(|entry| entry.0)
            {
                let (str1, str2) = pad(&format_factor(name, exp1), &format_factor(name, exp2));

                expected_result_string.push_str(&str1);
                actual_result_string.push_str(&str2);
            }

            let mut expected_factors_string = String::new();

            for (name, exp) in expected_factors
                .iter()
                .sorted_unstable_by_key(|entry| entry.0)
            {
                expected_factors_string.push_str(&format_factor(name, exp));
            }

            let mut actual_factors_string = String::new();

            for (name, exp) in actual_factors
                .iter()
                .sorted_unstable_by_key(|entry| entry.0)
            {
                actual_factors_string.push_str(&format_factor(name, exp));
            }

            expected_result_string.push_str(&format!(
                "{expected_factors_string: <width$}",
                width = expected_factors_string.width() + actual_factors_string.width()
            ));
            actual_result_string.push_str(&" ".repeat(expected_factors_string.width()));
            actual_result_string.push_str(&actual_factors_string);

            (expected_result_string, actual_result_string)
        };

        if !self.expected_dimensions.is_empty() {
            expected_result_string
                .push_str(&format!("    [= {}]", self.expected_dimensions.join(", ")));
        }

        if !self.actual_dimensions.is_empty() {
            actual_result_string
                .push_str(&format!("    [= {}]", self.actual_dimensions.join(", ")));
        }

        write!(
            f,
            "{}: {}",
            self.expected_name,
            expected_result_string.trim_start_matches(" × ").trim_end(),
        )?;

        write!(
            f,
            "\n{}: {}",
            self.actual_name,
            actual_result_string.trim_start_matches(" × ").trim_end(),
        )?;

        if let Some(fix) = suggested_fix(
            &self.expected_type,
            &self.actual_type,
            self.actual_name_for_fix,
        ) {
            write!(f, "\n\nSuggested fix: {fix}")?;
        }

        Ok(())
    }
}

impl Error for IncompatibleDimensionsError {}

#[derive(Debug, Clone, Error, PartialEq, Eq)]
pub enum TypeCheckError {
    #[error("Unknown identifier '{1}'.")]
    UnknownIdentifier(Span, String, Option<String>),

    #[error(transparent)]
    IncompatibleDimensions(IncompatibleDimensionsError),

    #[error("Exponents need to be dimensionless (got {1}).")]
    NonScalarExponent(Span, DType),

    #[error("Argument of factorial needs to be dimensionless (got {1}).")]
    NonScalarFactorialArgument(Span, DType),

    #[error("Unsupported expression in const-evaluation of exponent: {1}.")]
    UnsupportedConstEvalExpression(Span, &'static str),

    #[error("Division by zero in const. eval. expression")]
    DivisionByZeroInConstEvalExpression(Span),

    #[error("{0}")]
    RegistryError(RegistryError),

    #[error("Incompatible alternative expressions have been provided for dimension '{0}'")]
    IncompatibleAlternativeDimensionExpression(String, Span, DType, Span, DType),

    #[error("Function or procedure '{callable_name}' called with {num_args} arguments(s), but needs {}..{}", arity.start(), arity.end())]
    WrongArity {
        callable_span: Span,
        callable_name: String,
        callable_definition_span: Option<Span>,
        arity: ArityRange,
        num_args: usize,
    },

    #[error("'{1}' can not be used as a type parameter because it is also an existing dimension identifier.")]
    TypeParameterNameClash(Span, String),

    #[error("Could not infer the type parameters {3} in the function call '{2}'.")]
    CanNotInferTypeParameters(Span, Span, String, String),

    #[error("Multiple unresolved generic parameters in a single function parameter type are not (yet) supported. Consider reordering the function parameters")]
    MultipleUnresolvedTypeParameters(Span, Span),

    #[error("Foreign function definition (without body) '{1}' needs parameter and return type annotations.")]
    ForeignFunctionNeedsTypeAnnotations(Span, String),

    #[error("Unknown foreign function (without body) '{1}'")]
    UnknownForeignFunction(Span, String),

    #[error("Out-of bounds or non-rational exponent value")]
    NonRationalExponent(Span),

    #[error("Numerical overflow in const-eval expression")]
    OverflowInConstExpr(Span),

    #[error("Expected dimension type, got {1} instead")]
    ExpectedDimensionType(Span, Type),

    #[error("Expected boolean value")]
    ExpectedBool(Span),

    #[error("Incompatible types in condition")]
    IncompatibleTypesInCondition(Span, Type, Span, Type, Span),

    #[error("Argument types in assert call must be boolean")]
    IncompatibleTypeInAssert(Span, Type, Span),

    #[error("Argument types in assert_eq calls must match")]
    IncompatibleTypesInAssertEq(Span, Type, Span, Type, Span),

    #[error("Incompatible types in {0}")]
    IncompatibleTypesInAnnotation(String, Span, Type, Span, Type, Span),

    #[error("Incompatible types in comparison operator")]
    IncompatibleTypesInComparison(Span, Type, Span, Type, Span),

    #[error("Incompatible types in operator")]
    IncompatibleTypesInOperator(Span, BinaryOperator, Type, Span, Type, Span),

    #[error("Incompatible types in function call: expected '{1}', got '{3}' instead")]
    IncompatibleTypesInFunctionCall(Option<Span>, Type, Span, Type),

    #[error("Incompatible types for struct field: expected '{1}', got '{3}' instead")]
    IncompatibleTypesForStructField(Span, Type, Span, Type),

    #[error("Missing a definition for dimension {1}")]
    MissingDimension(Span, String),

    #[error("Function references can not point to generic functions")]
    NoFunctionReferenceToGenericFunction(Span),

    #[error("Only functions and function references can be called")]
    OnlyFunctionsAndReferencesCanBeCalled(Span),

    #[error("Base units can not be dimensionless.")]
    NoDimensionlessBaseUnit(Span, String),

    #[error("Unknown struct '{1}")]
    UnknownStruct(Span, String),

    #[error("Field '{2}' does not exist in struct '{3}'")]
    UnknownFieldInStructInstantiation(Span, Span, String, String),

    #[error("Duplicate field '{2}' in struct definition")]
    DuplicateFieldInStructDefinition(Span, Span, String),

    #[error("Duplicate field '{2}' in struct instantiation")]
    DuplicateFieldInStructInstantiation(Span, Span, String),

    #[error("Can not access field '{2}' of non struct type '{3}'")]
    FieldAccessOfNonStructType(Span, Span, String, Type),

    #[error("Field '{2}' does not exist in struct '{3}'")]
    UnknownFieldAccess(Span, Span, String, Type),

    #[error("Missing fields in struct instantiation")]
    MissingFieldsInStructInstantiation(Span, Span, Vec<(String, Type)>),

    #[error(transparent)]
    NameResolutionError(#[from] NameResolutionError),
}

type Result<T> = std::result::Result<T, TypeCheckError>;

fn to_rational_exponent(exponent_f64: f64) -> Option<Exponent> {
    Rational::from_f64(exponent_f64)
}

fn dtype(e: &Expression) -> Result<DType> {
    match e.get_type() {
        Type::Dimension(dtype) => Ok(dtype),
        t => Err(TypeCheckError::ExpectedDimensionType(e.full_span(), t)),
    }
}

/// Evaluates a limited set of expressions *at compile time*. This is needed to
/// support type checking of expressions like `(2 * meter)^(2*3 - 4)` where we
/// need to know not just the *type* but also the *value* of the exponent.
fn evaluate_const_expr(expr: &typed_ast::Expression) -> Result<Exponent> {
    match expr {
        typed_ast::Expression::Scalar(span, n) => {
            Ok(to_rational_exponent(n.to_f64())
                .ok_or(TypeCheckError::NonRationalExponent(*span))?)
        }
        typed_ast::Expression::UnaryOperator(_, ast::UnaryOperator::Negate, ref expr, _) => {
            Ok(-evaluate_const_expr(expr)?)
        }
        e @ typed_ast::Expression::UnaryOperator(_, ast::UnaryOperator::Factorial, _, _) => Err(
            TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "factorial"),
        ),
        e @ typed_ast::Expression::UnaryOperator(_, ast::UnaryOperator::LogicalNeg, _, _) => Err(
            TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "logical"),
        ),
        e @ typed_ast::Expression::BinaryOperator(_span_op, op, lhs_expr, rhs_expr, _) => {
            let lhs = evaluate_const_expr(lhs_expr)?;
            let rhs = evaluate_const_expr(rhs_expr)?;
            match op {
                typed_ast::BinaryOperator::Add => Ok(lhs
                    .checked_add(&rhs)
                    .ok_or_else(|| TypeCheckError::OverflowInConstExpr(expr.full_span()))?),
                typed_ast::BinaryOperator::Sub => Ok(lhs
                    .checked_sub(&rhs)
                    .ok_or_else(|| TypeCheckError::OverflowInConstExpr(expr.full_span()))?),
                typed_ast::BinaryOperator::Mul => Ok(lhs
                    .checked_mul(&rhs)
                    .ok_or_else(|| TypeCheckError::OverflowInConstExpr(expr.full_span()))?),
                typed_ast::BinaryOperator::Div => {
                    if rhs == Rational::zero() {
                        Err(TypeCheckError::DivisionByZeroInConstEvalExpression(
                            e.full_span(),
                        ))
                    } else {
                        Ok(lhs
                            .checked_div(&rhs)
                            .ok_or_else(|| TypeCheckError::OverflowInConstExpr(expr.full_span()))?)
                    }
                }
                typed_ast::BinaryOperator::Power => {
                    if rhs.is_integer() {
                        Ok(num_traits::checked_pow(
                            lhs,
                            rhs.to_integer().try_into().map_err(|_| {
                                TypeCheckError::OverflowInConstExpr(expr.full_span())
                            })?,
                        )
                        .ok_or_else(|| TypeCheckError::OverflowInConstExpr(expr.full_span()))?)
                    } else {
                        Err(TypeCheckError::UnsupportedConstEvalExpression(
                            e.full_span(),
                            "exponentiation with non-integer exponent",
                        ))
                    }
                }
                typed_ast::BinaryOperator::ConvertTo => Err(
                    TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "conversion"),
                ),
                typed_ast::BinaryOperator::LessThan
                | typed_ast::BinaryOperator::GreaterThan
                | typed_ast::BinaryOperator::LessOrEqual
                | typed_ast::BinaryOperator::GreaterOrEqual
                | typed_ast::BinaryOperator::Equal
                | typed_ast::BinaryOperator::NotEqual => Err(
                    TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "comparison"),
                ),
                typed_ast::BinaryOperator::LogicalAnd | typed_ast::BinaryOperator::LogicalOr => {
                    Err(TypeCheckError::UnsupportedConstEvalExpression(
                        e.full_span(),
                        "logical",
                    ))
                }
            }
        }
        e @ typed_ast::Expression::Identifier(..) => Err(
            TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "variable"),
        ),
        e @ typed_ast::Expression::UnitIdentifier(..) => Err(
            TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "unit identifier"),
        ),
        e @ typed_ast::Expression::FunctionCall(_, _, _, _, _) => Err(
            TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "function call"),
        ),
        e @ &typed_ast::Expression::CallableCall(_, _, _, _) => Err(
            TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "function call"),
        ),
        e @ typed_ast::Expression::Boolean(_, _) => Err(
            TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "Boolean value"),
        ),
        e @ typed_ast::Expression::String(_, _) => Err(
            TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "String"),
        ),
        e @ typed_ast::Expression::Condition(..) => Err(
            TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "Conditional"),
        ),
        e @ Expression::BinaryOperatorForDate(..) => {
            Err(TypeCheckError::UnsupportedConstEvalExpression(
                e.full_span(),
                "binary operator for datetimes",
            ))
        }
        e @ typed_ast::Expression::InstantiateStruct(_, _, _) => Err(
            TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "instantiate struct"),
        ),
        e @ typed_ast::Expression::AccessField(_, _, _, _, _, _) => Err(
            TypeCheckError::UnsupportedConstEvalExpression(e.full_span(), "access field of struct"),
        ),
    }
}

#[derive(Clone)]
pub struct FunctionSignature {
    definition_span: Span,
    pub type_parameters: Vec<(Span, String)>,
    pub parameter_types: Vec<(Span, String, Type)>,
    pub is_variadic: bool,
    pub return_type: Type,
}

#[derive(Clone, Debug)]
pub struct FunctionMetadata {
    pub name: Option<String>,
    pub url: Option<String>,
    pub description: Option<String>,
}

#[derive(Clone, Default)]
pub struct TypeChecker {
    identifiers: HashMap<String, (Type, Option<Span>)>,
    functions: HashMap<String, (FunctionSignature, FunctionMetadata)>,
    structs: HashMap<String, StructInfo>,
    registry: DimensionRegistry,

    type_namespace: Namespace,
    value_namespace: Namespace,
}

impl TypeChecker {
    fn identifier_type(&self, span: Span, name: &str) -> Result<Type> {
        let id = self
            .identifiers
            .get(name)
            .ok_or_else(|| {
                let suggestion = suggestion::did_you_mean(
                    self.identifiers
                        .keys()
                        .map(|k| k.to_string())
                        .chain(["true".into(), "false".into()]) // These are parsed as keywords, but can act like identifiers
                        .chain(self.functions.keys().cloned())
                        .chain(ffi::procedures().values().map(|p| p.name.clone())),
                    name,
                );
                TypeCheckError::UnknownIdentifier(span, name.into(), suggestion)
            })
            .map(|(type_, _)| type_)
            .cloned();

        if id.is_err() {
            if let Some((signature, _)) = self.functions.get(name) {
                if !signature.type_parameters.is_empty() {
                    return Err(TypeCheckError::NoFunctionReferenceToGenericFunction(span));
                }

                Ok(Type::Fn(
                    signature
                        .parameter_types
                        .iter()
                        .map(|(_, _, t)| t.clone())
                        .collect(),
                    Box::new(signature.return_type.clone()),
                ))
            } else {
                id
            }
        } else {
            id
        }
    }

    fn get_proper_function_reference(
        &self,
        expr: &ast::Expression,
    ) -> Option<(String, &FunctionSignature)> {
        match expr {
            ast::Expression::Identifier(_, name) => self
                .functions
                .get(name)
                .map(|(signature, _)| (name.clone(), signature)),
            _ => None,
        }
    }

    fn proper_function_call(
        &self,
        span: &Span,
        full_span: &Span,
        function_name: &str,
        signature: &FunctionSignature,
        arguments: Vec<typed_ast::Expression>,
        argument_types: Vec<Type>,
    ) -> Result<typed_ast::Expression> {
        let FunctionSignature {
            definition_span,
            type_parameters,
            parameter_types,
            is_variadic,
            return_type,
        } = signature;

        let arity_range = if *is_variadic {
            1..=usize::MAX
        } else {
            parameter_types.len()..=parameter_types.len()
        };

        if !arity_range.contains(&arguments.len()) {
            return Err(TypeCheckError::WrongArity {
                callable_span: *span,
                callable_name: function_name.into(),
                callable_definition_span: Some(*definition_span),
                arity: arity_range,
                num_args: arguments.len(),
            });
        }

        let mut substitutions: Vec<(String, DType)> = vec![];

        let substitute = |substitutions: &[(String, DType)], type_: &DType| -> DType {
            let mut result_type = type_.clone();
            for (name, substituted_type) in substitutions {
                if let Some(factor @ BaseRepresentationFactor(_, exp)) = type_
                    .clone() // TODO: remove this .clone() somehow?
                    .iter()
                    .find(|BaseRepresentationFactor(n, _)| n == name)
                {
                    result_type = result_type / DType::from_factor((*factor).clone())
                        * substituted_type.clone().power(*exp);
                }
            }
            result_type
        };

        let mut parameter_types = parameter_types.clone();

        if *is_variadic {
            // For a variadic function, we simply duplicate the parameter type
            // N times, where N is the number of arguments given.
            debug_assert!(parameter_types.len() == 1);

            for _ in 1..argument_types.len() {
                parameter_types.push(parameter_types[0].clone());
            }
        }

        for (idx, ((parameter_span, _, parameter_type), argument_type)) in
            parameter_types.iter().zip(argument_types).enumerate()
        {
            match (parameter_type, argument_type) {
                (Type::Dimension(parameter_type), Type::Dimension(argument_type)) => {
                    let mut parameter_type = substitute(&substitutions, parameter_type);

                    let remaining_generic_subtypes: Vec<_> = parameter_type
                        .iter()
                        .filter(|BaseRepresentationFactor(name, _)| {
                            type_parameters.iter().any(|(_, n)| name == n)
                        })
                        .collect();

                    if remaining_generic_subtypes.len() > 1 {
                        return Err(TypeCheckError::MultipleUnresolvedTypeParameters(
                            *span,
                            *parameter_span,
                        ));
                    }

                    if let Some(&generic_subtype_factor) = remaining_generic_subtypes.first() {
                        let generic_subtype = DType::from_factor(generic_subtype_factor.clone());

                        // The type of the idx-th parameter of the called function has a generic type
                        // parameter inside. We can now instantiate that generic parameter by solving
                        // the equation "parameter_type == argument_type" for the generic parameter.
                        // In order to do this, let's assume `generic_subtype = D^alpha`, then we have
                        //
                        //                                parameter_type == argument_type
                        //    parameter_type / generic_subtype * D^alpha == argument_type
                        //                                       D^alpha == argument_type / (parameter_type / generic_subtype)
                        //                                             D == [argument_type / (parameter_type / generic_subtype)]^(1/alpha)
                        //

                        let alpha = Rational::from_integer(1) / generic_subtype_factor.1;
                        let d = (argument_type.clone()
                            / (parameter_type.clone() / generic_subtype))
                            .power(alpha);

                        // We can now substitute that generic parameter in all subsequent expressions
                        substitutions.push((generic_subtype_factor.0.clone(), d));

                        parameter_type = substitute(&substitutions, &parameter_type);
                    }

                    if parameter_type != argument_type {
                        return Err(TypeCheckError::IncompatibleDimensions(
                            IncompatibleDimensionsError {
                                span_operation: *span,
                                operation: format!(
                                    "argument {num} of function call to '{name}'",
                                    num = idx + 1,
                                    name = function_name
                                ),
                                span_expected: parameter_types[idx].0,
                                expected_name: "parameter type",
                                expected_dimensions: self
                                    .registry
                                    .get_derived_entry_names_for(&parameter_type),
                                expected_type: parameter_type,
                                span_actual: arguments[idx].full_span(),
                                actual_name: " argument type",
                                actual_name_for_fix: "function argument",
                                actual_dimensions: self
                                    .registry
                                    .get_derived_entry_names_for(&argument_type),
                                actual_type: argument_type,
                            },
                        ));
                    }
                }
                (parameter_type, argument_type) => {
                    if !argument_type.is_subtype_of(parameter_type) {
                        return Err(TypeCheckError::IncompatibleTypesInFunctionCall(
                            Some(*parameter_span),
                            parameter_type.clone(),
                            arguments[idx].full_span(),
                            argument_type.clone(),
                        ));
                    }
                }
            }
        }

        if substitutions.len() != type_parameters.len() {
            let parameters: HashSet<String> = type_parameters
                .iter()
                .map(|(_, name)| name)
                .cloned()
                .collect();
            let inferred_parameters: HashSet<String> =
                substitutions.iter().map(|t| t.0.clone()).collect();

            let remaining: Vec<_> = (&parameters - &inferred_parameters)
                .iter()
                .cloned()
                .collect();

            return Err(TypeCheckError::CanNotInferTypeParameters(
                *span,
                *definition_span,
                function_name.into(),
                remaining.join(", "),
            ));
        }

        fn apply_substitutions(
            t: &Type,
            substitute: impl Fn(&[(String, DType)], &DType) -> DType + Copy,
            substitutions: &[(String, DType)],
        ) -> Type {
            match t {
                Type::Dimension(d) => Type::Dimension(substitute(substitutions, d)),
                // The following case is not needed at the moment, but might be interesting
                // in the future if we add support for generic structs.
                Type::Struct(StructInfo {
                    definition_span,
                    name,
                    fields,
                }) => Type::Struct(StructInfo {
                    definition_span: *definition_span,
                    name: name.clone(),
                    fields: fields
                        .into_iter()
                        .map(|(n, (s, t))| {
                            (
                                n.to_owned(),
                                (s.clone(), apply_substitutions(t, substitute, substitutions)),
                            )
                        })
                        .collect(),
                }),
                type_ => type_.clone(),
            }
        }

        let return_type = apply_substitutions(return_type, substitute, &substitutions);

        Ok(typed_ast::Expression::FunctionCall(
            *span,
            *full_span,
            function_name.into(),
            arguments,
            return_type,
        ))
    }

    pub(crate) fn check_expression(&self, ast: &ast::Expression) -> Result<typed_ast::Expression> {
        Ok(match ast {
            ast::Expression::Scalar(span, n) => typed_ast::Expression::Scalar(*span, *n),
            ast::Expression::Identifier(span, name) => {
                let type_ = self.identifier_type(*span, name)?.clone();

                typed_ast::Expression::Identifier(*span, name.clone(), type_)
            }
            ast::Expression::UnitIdentifier(span, prefix, name, full_name) => {
                let type_ = self.identifier_type(*span, name)?.clone();

                typed_ast::Expression::UnitIdentifier(
                    *span,
                    *prefix,
                    name.clone(),
                    full_name.clone(),
                    type_,
                )
            }
            ast::Expression::UnaryOperator { op, expr, span_op } => {
                let checked_expr = self.check_expression(expr)?;
                let type_ = checked_expr.get_type();
                match (&type_, op) {
                    (Type::Never, _) => {}
                    (Type::Dimension(dtype), ast::UnaryOperator::Factorial) => {
                        if !dtype.is_scalar() {
                            return Err(TypeCheckError::NonScalarFactorialArgument(
                                expr.full_span(),
                                dtype.clone(),
                            ));
                        }
                    }
                    (Type::Dimension(_), ast::UnaryOperator::Negate) => (),
                    (Type::Boolean, ast::UnaryOperator::LogicalNeg) => (),
                    (_, ast::UnaryOperator::LogicalNeg) => {
                        return Err(TypeCheckError::ExpectedBool(expr.full_span()))
                    }
                    _ => {
                        return Err(TypeCheckError::ExpectedDimensionType(
                            checked_expr.full_span(),
                            type_.clone(),
                        ));
                    }
                };

                typed_ast::Expression::UnaryOperator(*span_op, *op, Box::new(checked_expr), type_)
            }
            ast::Expression::BinaryOperator {
                op,
                lhs,
                rhs,
                span_op,
            } => {
                let lhs_checked = self.check_expression(lhs)?;
                let rhs_checked = self.check_expression(rhs)?;

                let lhs_type = lhs_checked.get_type();
                let rhs_type = rhs_checked.get_type();

                if lhs_type.is_never() {
                    return Ok(typed_ast::Expression::BinaryOperator(
                        *span_op,
                        *op,
                        Box::new(lhs_checked),
                        Box::new(rhs_checked),
                        rhs_type,
                    ));
                } else if rhs_type.is_never() {
                    return Ok(typed_ast::Expression::BinaryOperator(
                        *span_op,
                        *op,
                        Box::new(lhs_checked),
                        Box::new(rhs_checked),
                        lhs_type,
                    ));
                } else if rhs_type.is_fn_type() && op == &BinaryOperator::ConvertTo {
                    let (parameter_types, return_type) = match rhs_type {
                        Type::Fn(p, r) => (p, r),
                        _ => unreachable!(),
                    };
                    // make sure that there is just one paramter (return arity error otherwise)
                    if parameter_types.len() != 1 {
                        return Err(TypeCheckError::WrongArity {
                            callable_span: rhs.full_span(),
                            callable_name: "function".into(),
                            callable_definition_span: None,
                            arity: 1..=1,
                            num_args: parameter_types.len(),
                        });
                    }

                    if !parameter_types[0].is_subtype_of(&lhs_type) {
                        return Err(TypeCheckError::IncompatibleTypesInFunctionCall(
                            None,
                            parameter_types[0].clone(),
                            lhs.full_span(),
                            lhs_type,
                        ));
                    }

                    typed_ast::Expression::CallableCall(
                        lhs.full_span(),
                        Box::new(rhs_checked),
                        vec![lhs_checked],
                        *return_type,
                    )
                } else if lhs_type == Type::DateTime {
                    // DateTime types need special handling here, since they're not scalars with dimensions,
                    // yet some select binary operators can be applied to them

                    let rhs_is_time = dtype(&rhs_checked)
                        .ok()
                        .map(|t| t.is_time_dimension())
                        .unwrap_or(false);
                    let rhs_is_datetime = rhs_type == Type::DateTime;

                    if *op == BinaryOperator::Sub && rhs_is_datetime {
                        let time = self
                            .registry
                            .get_base_representation_for_name("Time")
                            .map_err(|_| {
                                TypeCheckError::MissingDimension(ast.full_span(), "Time".into())
                            })?;

                        // TODO make sure the "second" unit exists

                        typed_ast::Expression::BinaryOperatorForDate(
                            *span_op,
                            *op,
                            Box::new(lhs_checked),
                            Box::new(rhs_checked),
                            Type::Dimension(time),
                        )
                    } else if (*op == BinaryOperator::Add || *op == BinaryOperator::Sub)
                        && rhs_is_time
                    {
                        typed_ast::Expression::BinaryOperatorForDate(
                            *span_op,
                            *op,
                            Box::new(lhs_checked),
                            Box::new(rhs_checked),
                            Type::DateTime,
                        )
                    } else {
                        return Err(TypeCheckError::IncompatibleTypesInOperator(
                            span_op.unwrap_or_else(|| {
                                ast::Expression::BinaryOperator {
                                    op: *op,
                                    lhs: lhs.clone(),
                                    rhs: rhs.clone(),
                                    span_op: *span_op,
                                }
                                .full_span()
                            }),
                            *op,
                            lhs_type,
                            lhs.full_span(),
                            rhs_type,
                            rhs.full_span(),
                        ));
                    }
                } else {
                    let get_type_and_assert_equality = || {
                        let lhs_type = dtype(&lhs_checked)?;
                        let rhs_type = dtype(&rhs_checked)?;
                        if lhs_type != rhs_type {
                            let full_span = ast::Expression::BinaryOperator {
                                op: *op,
                                lhs: lhs.clone(),
                                rhs: rhs.clone(),
                                span_op: *span_op,
                            }
                            .full_span();
                            Err(TypeCheckError::IncompatibleDimensions(
                                IncompatibleDimensionsError {
                                    span_operation: span_op.unwrap_or(full_span),
                                    operation: match op {
                                        typed_ast::BinaryOperator::Add => "addition".into(),
                                        typed_ast::BinaryOperator::Sub => "subtraction".into(),
                                        typed_ast::BinaryOperator::Mul => "multiplication".into(),
                                        typed_ast::BinaryOperator::Div => "division".into(),
                                        typed_ast::BinaryOperator::Power => "exponentiation".into(),
                                        typed_ast::BinaryOperator::ConvertTo => {
                                            "unit conversion".into()
                                        }
                                        typed_ast::BinaryOperator::LessThan
                                        | typed_ast::BinaryOperator::GreaterThan
                                        | typed_ast::BinaryOperator::LessOrEqual
                                        | typed_ast::BinaryOperator::GreaterOrEqual
                                        | typed_ast::BinaryOperator::Equal
                                        | typed_ast::BinaryOperator::NotEqual => {
                                            "comparison".into()
                                        }
                                        typed_ast::BinaryOperator::LogicalAnd => "and".into(),
                                        typed_ast::BinaryOperator::LogicalOr => "or".into(),
                                    },
                                    span_expected: lhs.full_span(),
                                    expected_name: " left hand side",
                                    expected_dimensions: self
                                        .registry
                                        .get_derived_entry_names_for(&lhs_type),
                                    expected_type: lhs_type,
                                    span_actual: rhs.full_span(),
                                    actual_name: "right hand side",
                                    actual_name_for_fix: "expression on the right hand side",
                                    actual_dimensions: self
                                        .registry
                                        .get_derived_entry_names_for(&rhs_type),
                                    actual_type: rhs_type,
                                },
                            ))
                        } else {
                            Ok(Type::Dimension(lhs_type))
                        }
                    };

                    let type_ = match op {
                        typed_ast::BinaryOperator::Add => get_type_and_assert_equality()?,
                        typed_ast::BinaryOperator::Sub => get_type_and_assert_equality()?,
                        typed_ast::BinaryOperator::Mul => {
                            Type::Dimension(dtype(&lhs_checked)? * dtype(&rhs_checked)?)
                        }
                        typed_ast::BinaryOperator::Div => {
                            Type::Dimension(dtype(&lhs_checked)? / dtype(&rhs_checked)?)
                        }
                        typed_ast::BinaryOperator::Power => {
                            let exponent_type = dtype(&rhs_checked)?;
                            if !exponent_type.is_scalar() {
                                return Err(TypeCheckError::NonScalarExponent(
                                    rhs.full_span(),
                                    exponent_type,
                                ));
                            }

                            let base_type = dtype(&lhs_checked)?;
                            if base_type.is_scalar() {
                                // Skip evaluating the exponent if the lhs is a scalar. This allows
                                // for arbitrary (decimal) exponents, if the base is a scalar.

                                Type::Dimension(base_type)
                            } else {
                                let exponent = evaluate_const_expr(&rhs_checked)?;
                                Type::Dimension(base_type.power(exponent))
                            }
                        }
                        typed_ast::BinaryOperator::ConvertTo => get_type_and_assert_equality()?,
                        typed_ast::BinaryOperator::LessThan
                        | typed_ast::BinaryOperator::GreaterThan
                        | typed_ast::BinaryOperator::LessOrEqual
                        | typed_ast::BinaryOperator::GreaterOrEqual => {
                            let _ = get_type_and_assert_equality()?;
                            Type::Boolean
                        }
                        typed_ast::BinaryOperator::Equal | typed_ast::BinaryOperator::NotEqual => {
                            if lhs_type.is_dtype() || rhs_type.is_dtype() {
                                let _ = get_type_and_assert_equality()?;
                            } else if lhs_type != rhs_type
                                || lhs_type.is_fn_type()
                                || rhs_type.is_fn_type()
                            {
                                return Err(TypeCheckError::IncompatibleTypesInComparison(
                                    span_op.unwrap(),
                                    lhs_type,
                                    lhs.full_span(),
                                    rhs_type,
                                    rhs.full_span(),
                                ));
                            }

                            Type::Boolean
                        }
                        typed_ast::BinaryOperator::LogicalAnd
                        | typed_ast::BinaryOperator::LogicalOr => {
                            if lhs_type != Type::Boolean {
                                return Err(TypeCheckError::ExpectedBool(lhs.full_span()));
                            } else if rhs_type != Type::Boolean {
                                return Err(TypeCheckError::ExpectedBool(rhs.full_span()));
                            }

                            Type::Boolean
                        }
                    };

                    typed_ast::Expression::BinaryOperator(
                        *span_op,
                        *op,
                        Box::new(lhs_checked),
                        Box::new(rhs_checked),
                        type_,
                    )
                }
            }
            ast::Expression::FunctionCall(span, full_span, callable, args) => {
                let arguments_checked = args
                    .iter()
                    .map(|a| self.check_expression(a))
                    .collect::<Result<Vec<_>>>()?;
                let argument_types = arguments_checked
                    .iter()
                    .map(|e| e.get_type())
                    .collect::<Vec<Type>>();

                // There are two options here. The 'callable' can either be a direct reference
                // to a (proper) function, or it can be an arbitrary complicated expression
                // that evaluates to a function "pointer".

                if let Some((name, signature)) = self.get_proper_function_reference(callable) {
                    self.proper_function_call(
                        span,
                        full_span,
                        &name,
                        signature,
                        arguments_checked,
                        argument_types,
                    )?
                } else {
                    let callable_checked = self.check_expression(callable)?;
                    let callable_type = callable_checked.get_type();

                    match callable_type {
                        Type::Fn(parameters_types, return_type) => {
                            let num_parameters = parameters_types.len();
                            let num_arguments = arguments_checked.len();

                            if num_parameters != num_arguments {
                                return Err(TypeCheckError::WrongArity {
                                    callable_span: *span,
                                    callable_name: "function".into(),
                                    callable_definition_span: None,
                                    arity: num_parameters..=num_parameters,
                                    num_args: num_arguments,
                                });
                            }

                            for (param_type, arg_checked) in
                                parameters_types.iter().zip(&arguments_checked)
                            {
                                if !arg_checked.get_type().is_subtype_of(param_type) {
                                    return Err(TypeCheckError::IncompatibleTypesInFunctionCall(
                                        None,
                                        param_type.clone(),
                                        arg_checked.full_span(),
                                        arg_checked.get_type(),
                                    ));
                                }
                            }

                            typed_ast::Expression::CallableCall(
                                *full_span,
                                Box::new(callable_checked),
                                arguments_checked,
                                *return_type,
                            )
                        }
                        _ => {
                            return Err(TypeCheckError::OnlyFunctionsAndReferencesCanBeCalled(
                                callable.full_span(),
                            ));
                        }
                    }
                }
            }
            ast::Expression::Boolean(span, val) => typed_ast::Expression::Boolean(*span, *val),
            ast::Expression::String(span, parts) => typed_ast::Expression::String(
                *span,
                parts
                    .iter()
                    .map(|p| match p {
                        StringPart::Fixed(s) => Ok(typed_ast::StringPart::Fixed(s.clone())),
                        StringPart::Interpolation {
                            span,
                            expr,
                            format_specifiers,
                        } => Ok(typed_ast::StringPart::Interpolation {
                            span: *span,
                            format_specifiers: format_specifiers.clone(),
                            expr: Box::new(self.check_expression(expr)?),
                        }),
                    })
                    .collect::<Result<_>>()?,
            ),
            ast::Expression::Condition(span, condition, then, else_) => {
                let condition = self.check_expression(condition)?;
                if condition.get_type() != Type::Boolean {
                    return Err(TypeCheckError::ExpectedBool(condition.full_span()));
                }

                let then = self.check_expression(then)?;
                let else_ = self.check_expression(else_)?;

                let then_type = then.get_type();
                let else_type = else_.get_type();

                if then_type.is_never() || else_type.is_never() {
                    // This case is fine. We use the type of the *other* branch in those cases.
                    // For example:
                    //
                    //   if <some precondition>
                    //     then X
                    //     else error("please make sure <some precondition> is met")
                    //
                    // Here, we simply use the type of `X` as the type of the whole expression.
                } else if then_type != else_type {
                    return Err(TypeCheckError::IncompatibleTypesInCondition(
                        *span,
                        then_type,
                        then.full_span(),
                        else_type,
                        else_.full_span(),
                    ));
                }

                typed_ast::Expression::Condition(
                    *span,
                    Box::new(condition),
                    Box::new(then),
                    Box::new(else_),
                )
            }
            ast::Expression::InstantiateStruct {
                full_span,
                ident_span,
                name,
                fields,
            } => {
                let fields_checked = fields
                    .iter()
                    .map(|(_, n, v)| Ok((n.to_string(), self.check_expression(v)?)))
                    .collect::<Result<Vec<_>>>()?;

                let Some(struct_info) = self.structs.get(name) else {
                    return Err(TypeCheckError::UnknownStruct(*ident_span, name.clone()));
                };

                let mut seen_fields = HashMap::new();

                for ((field, expr), span) in
                    fields_checked.iter().zip(fields.iter().map(|(s, _, _)| s))
                {
                    if let Some(other_span) = seen_fields.get(field) {
                        return Err(TypeCheckError::DuplicateFieldInStructInstantiation(
                            *span,
                            *other_span,
                            field.to_string(),
                        ));
                    }

                    let Some((expected_field_span, expected_type)) = struct_info.fields.get(field)
                    else {
                        return Err(TypeCheckError::UnknownFieldInStructInstantiation(
                            *span,
                            struct_info.definition_span,
                            field.clone(),
                            struct_info.name.clone(),
                        ));
                    };

                    let found_type = &expr.get_type();
                    if !found_type.is_subtype_of(expected_type) {
                        return Err(TypeCheckError::IncompatibleTypesForStructField(
                            *expected_field_span,
                            expected_type.clone(),
                            expr.full_span(),
                            found_type.clone(),
                        ));
                    }

                    seen_fields.insert(field, *span);
                }

                let missing_fields = {
                    let mut fields = struct_info.fields.clone();
                    fields.retain(|f, _| !seen_fields.contains_key(f));
                    fields.into_iter().map(|(n, (_, t))| (n, t)).collect_vec()
                };

                if !missing_fields.is_empty() {
                    return Err(TypeCheckError::MissingFieldsInStructInstantiation(
                        *full_span,
                        struct_info.definition_span,
                        missing_fields,
                    ));
                }

                typed_ast::Expression::InstantiateStruct(
                    *full_span,
                    fields_checked,
                    struct_info.clone(),
                )
            }
            ast::Expression::AccessField(full_span, ident_span, expr, attr) => {
                let expr_checked = self.check_expression(expr)?;

                let type_ = expr_checked.get_type();

                let Type::Struct(struct_info) = type_.clone() else {
                    return Err(TypeCheckError::FieldAccessOfNonStructType(
                        *ident_span,
                        expr.full_span(),
                        attr.to_string(),
                        type_.clone(),
                    ));
                };

                let Some((_, ret_ty)) = struct_info.fields.get(attr) else {
                    return Err(TypeCheckError::UnknownFieldAccess(
                        *ident_span,
                        expr.full_span(),
                        attr.to_string(),
                        type_.clone(),
                    ));
                };

                let ret_ty = ret_ty.to_owned();

                Expression::AccessField(
                    *ident_span,
                    *full_span,
                    Box::new(expr_checked),
                    attr.to_owned(),
                    struct_info,
                    ret_ty,
                )
            }
        })
    }

    pub fn check_statement(&mut self, ast: &ast::Statement) -> Result<typed_ast::Statement> {
        Ok(match ast {
            ast::Statement::Expression(expr) => {
                let checked_expr = self.check_expression(expr)?;
                for &identifier in LAST_RESULT_IDENTIFIERS {
                    self.identifiers
                        .insert(identifier.into(), (checked_expr.get_type(), None));
                }
                typed_ast::Statement::Expression(checked_expr)
            }
            ast::Statement::DefineVariable {
                identifier_span,
                identifier,
                expr,
                type_annotation,
                decorators,
            } => {
                let expr_checked = self.check_expression(expr)?;
                let type_deduced = expr_checked.get_type();

                if let Some(ref type_annotation) = type_annotation {
                    let type_annotated = self.type_from_annotation(type_annotation)?;

                    match (&type_deduced, type_annotated) {
                        (Type::Dimension(dexpr_deduced), Type::Dimension(dexpr_specified)) => {
                            if dexpr_deduced != &dexpr_specified {
                                return Err(TypeCheckError::IncompatibleDimensions(
                                    IncompatibleDimensionsError {
                                        span_operation: *identifier_span,
                                        operation: "variable definition".into(),
                                        span_expected: type_annotation.full_span(),
                                        expected_name: "specified dimension",
                                        expected_dimensions: self
                                            .registry
                                            .get_derived_entry_names_for(&dexpr_specified),
                                        expected_type: dexpr_specified,
                                        span_actual: expr.full_span(),
                                        actual_name: "   actual dimension",
                                        actual_name_for_fix: "right hand side expression",
                                        actual_dimensions: self
                                            .registry
                                            .get_derived_entry_names_for(dexpr_deduced),
                                        actual_type: dexpr_deduced.clone(),
                                    },
                                ));
                            }
                        }
                        (deduced, annotated) => {
                            if !deduced.is_subtype_of(&annotated) {
                                return Err(TypeCheckError::IncompatibleTypesInAnnotation(
                                    "definition".into(),
                                    *identifier_span,
                                    annotated,
                                    type_annotation.full_span(),
                                    deduced.clone(),
                                    expr_checked.full_span(),
                                ));
                            }
                        }
                    }
                }

                for (name, _) in decorator::name_and_aliases(identifier, decorators) {
                    self.identifiers
                        .insert(name.clone(), (type_deduced.clone(), Some(*identifier_span)));

                    self.value_namespace.add_identifier_allow_override(
                        name.clone(),
                        *identifier_span,
                        "constant".to_owned(),
                    )?;
                }

                typed_ast::Statement::DefineVariable(
                    identifier.clone(),
                    decorators.clone(),
                    expr_checked,
                    type_annotation
                        .as_ref()
                        .map(|d| d.pretty_print())
                        .unwrap_or_else(|| type_deduced.to_readable_type(&self.registry)),
                    type_deduced,
                )
            }
            ast::Statement::DefineBaseUnit(span, unit_name, type_annotation, decorators) => {
                let type_specified = if let Some(dexpr) = type_annotation {
                    let base_representation = self
                        .registry
                        .get_base_representation(dexpr)
                        .map_err(TypeCheckError::RegistryError)?;

                    if base_representation.is_scalar() {
                        return Err(TypeCheckError::NoDimensionlessBaseUnit(
                            *span,
                            unit_name.into(),
                        ));
                    }

                    base_representation
                } else {
                    use heck::ToUpperCamelCase;
                    // In a unit definition like 'unit pixel' without a specified type,
                    // we add a new type for the user
                    let type_name = unit_name.to_upper_camel_case();
                    self.registry
                        .add_base_dimension(&type_name)
                        .map_err(TypeCheckError::RegistryError)?
                };
                for (name, _) in decorator::name_and_aliases(unit_name, decorators) {
                    self.identifiers.insert(
                        name.clone(),
                        (Type::Dimension(type_specified.clone()), Some(*span)),
                    );
                }
                typed_ast::Statement::DefineBaseUnit(
                    unit_name.clone(),
                    decorators.clone(),
                    type_annotation
                        .as_ref()
                        .map(|d| d.pretty_print())
                        .unwrap_or_else(|| type_specified.to_readable_type(&self.registry)),
                    Type::Dimension(type_specified),
                )
            }
            ast::Statement::DefineDerivedUnit {
                identifier_span,
                identifier,
                expr,
                type_annotation_span,
                type_annotation,
                decorators,
            } => {
                // TODO: this is the *exact same code* that we have above for
                // variable definitions => deduplicate this somehow
                let expr_checked = self.check_expression(expr)?;
                let type_deduced = dtype(&expr_checked)?;

                if let Some(ref dexpr) = type_annotation {
                    let type_specified = self
                        .registry
                        .get_base_representation(dexpr)
                        .map_err(TypeCheckError::RegistryError)?;
                    if type_deduced != type_specified {
                        return Err(TypeCheckError::IncompatibleDimensions(
                            IncompatibleDimensionsError {
                                span_operation: *identifier_span,
                                operation: "unit definition".into(),
                                span_expected: type_annotation_span.unwrap(),
                                expected_name: "specified dimension",
                                expected_dimensions: self
                                    .registry
                                    .get_derived_entry_names_for(&type_specified),
                                expected_type: type_specified,
                                span_actual: expr.full_span(),
                                actual_name: "   actual dimension",
                                actual_name_for_fix: "right hand side expression",
                                actual_dimensions: self
                                    .registry
                                    .get_derived_entry_names_for(&type_deduced),
                                actual_type: type_deduced,
                            },
                        ));
                    }
                }
                for (name, _) in decorator::name_and_aliases(identifier, decorators) {
                    self.identifiers.insert(
                        name.clone(),
                        (
                            Type::Dimension(type_deduced.clone()),
                            Some(*identifier_span),
                        ),
                    );
                }
                typed_ast::Statement::DefineDerivedUnit(
                    identifier.clone(),
                    expr_checked,
                    decorators.clone(),
                    type_annotation
                        .as_ref()
                        .map(|d| d.pretty_print())
                        .unwrap_or_else(|| type_deduced.to_readable_type(&self.registry)),
                    Type::Dimension(type_deduced),
                )
            }
            ast::Statement::DefineFunction {
                function_name_span,
                function_name,
                type_parameters,
                parameters,
                body,
                return_type_annotation_span,
                return_type_annotation,
                decorators,
            } => {
                if body.is_none() {
                    self.value_namespace.add_identifier(
                        function_name.clone(),
                        *function_name_span,
                        "foreign function".to_owned(),
                    )?;
                } else {
                    self.value_namespace.add_identifier_allow_override(
                        function_name.clone(),
                        *function_name_span,
                        "function".to_owned(),
                    )?;
                }

                let mut typechecker_fn = self.clone();
                let is_ffi_function = body.is_none();
                let mut type_parameters = type_parameters.clone();

                for (span, type_parameter) in &type_parameters {
                    if typechecker_fn.type_namespace.has_identifier(type_parameter) {
                        return Err(TypeCheckError::TypeParameterNameClash(
                            *span,
                            type_parameter.clone(),
                        ));
                    }

                    match typechecker_fn.registry.add_base_dimension(type_parameter) {
                        Err(RegistryError::EntryExists(name)) => {
                            return Err(TypeCheckError::TypeParameterNameClash(*span, name))
                        }
                        Err(err) => return Err(TypeCheckError::RegistryError(err)),
                        _ => {}
                    }
                }

                let mut typed_parameters = vec![];
                let mut is_variadic = false;
                let mut free_type_parameters = vec![];
                for (parameter_span, parameter, type_annotation, p_is_variadic) in parameters {
                    let parameter_type = if let Some(type_annotation) = type_annotation {
                        typechecker_fn.type_from_annotation(type_annotation)?
                    } else if is_ffi_function {
                        return Err(TypeCheckError::ForeignFunctionNeedsTypeAnnotations(
                            *function_name_span,
                            function_name.clone(),
                        ));
                    } else {
                        let mut free_type_parameter = "".into();
                        for i in 0.. {
                            free_type_parameter = format!("T{i}");
                            if !typechecker_fn.registry.contains(&free_type_parameter) {
                                break;
                            }
                        }

                        free_type_parameters.push((parameter.clone(), free_type_parameter.clone()));

                        typechecker_fn
                            .registry
                            .add_base_dimension(&free_type_parameter)
                            .expect("we selected a name that is free");
                        type_parameters.push((*parameter_span, free_type_parameter.clone()));
                        Type::Dimension(
                            typechecker_fn
                                .registry
                                .get_base_representation(&TypeExpression::TypeIdentifier(
                                    *parameter_span,
                                    free_type_parameter,
                                ))
                                .map_err(TypeCheckError::RegistryError)?,
                        )
                    };

                    typechecker_fn.identifiers.insert(
                        parameter.clone(),
                        (parameter_type.clone(), Some(*parameter_span)),
                    );
                    typed_parameters.push((
                        *parameter_span,
                        parameter.clone(),
                        *p_is_variadic,
                        type_annotation
                            .as_ref()
                            .map(|d| d.pretty_print())
                            .unwrap_or_else(|| parameter_type.to_readable_type(&self.registry)),
                        parameter_type,
                    ));

                    is_variadic |= p_is_variadic;
                }

                if !free_type_parameters.is_empty() {
                    // TODO: Perform type inference
                }

                let return_type_specified = return_type_annotation
                    .as_ref()
                    .map(|annotation| typechecker_fn.type_from_annotation(annotation))
                    .transpose()?;

                let add_function_signature = |tc: &mut TypeChecker, return_type: Type| {
                    let parameter_types = typed_parameters
                        .iter()
                        .map(|(span, name, _, _, t)| (*span, name.clone(), t.clone()))
                        .collect();
                    tc.functions.insert(
                        function_name.clone(),
                        (
                            FunctionSignature {
                                definition_span: *function_name_span,
                                type_parameters: type_parameters.clone(),
                                parameter_types,
                                is_variadic,
                                return_type,
                            },
                            FunctionMetadata {
                                name: crate::decorator::name(decorators),
                                url: crate::decorator::url(decorators),
                                description: crate::decorator::description(decorators),
                            },
                        ),
                    );
                };

                if let Some(ref return_type_specified) = return_type_specified {
                    // This is needed for recursive functions. If the return type
                    // has been specified, we can already provide a function
                    // signature before we check the body of the function. This
                    // way, the 'typechecker_fn' can resolve the recursive call.
                    add_function_signature(&mut typechecker_fn, return_type_specified.clone());
                }

                let body_checked = body
                    .clone()
                    .map(|expr| typechecker_fn.check_expression(&expr))
                    .transpose()?;

                let return_type = if let Some(ref expr) = body_checked {
                    let type_deduced = expr.get_type();

                    if let Some(return_type_specified) = return_type_specified {
                        match (type_deduced, return_type_specified) {
                            (Type::Dimension(dtype_deduced), Type::Dimension(dtype_specified)) => {
                                if dtype_deduced != dtype_specified {
                                    return Err(TypeCheckError::IncompatibleDimensions(
                                        IncompatibleDimensionsError {
                                            span_operation: *function_name_span,
                                            operation: "function return type".into(),
                                            span_expected: return_type_annotation_span.unwrap(),
                                            expected_name: "specified return type",
                                            expected_dimensions: self
                                                .registry
                                                .get_derived_entry_names_for(&dtype_specified),
                                            expected_type: dtype_specified,
                                            span_actual: body
                                                .as_ref()
                                                .map(|b| b.full_span())
                                                .unwrap(),
                                            actual_name: "   actual return type",
                                            actual_name_for_fix: "expression in the function body",
                                            actual_dimensions: self
                                                .registry
                                                .get_derived_entry_names_for(&dtype_deduced),
                                            actual_type: dtype_deduced,
                                        },
                                    ));
                                }

                                Type::Dimension(dtype_deduced)
                            }
                            (type_deduced, type_specified) => {
                                if !type_deduced.is_subtype_of(&type_specified) {
                                    return Err(TypeCheckError::IncompatibleTypesInAnnotation(
                                        "function definition".into(),
                                        *function_name_span,
                                        type_specified,
                                        return_type_annotation_span.unwrap(),
                                        type_deduced,
                                        body.as_ref().map(|b| b.full_span()).unwrap(),
                                    ));
                                }
                                type_specified
                            }
                        }
                    } else {
                        type_deduced
                    }
                } else {
                    if !ffi::functions().contains_key(function_name.as_str()) {
                        return Err(TypeCheckError::UnknownForeignFunction(
                            *function_name_span,
                            function_name.clone(),
                        ));
                    }

                    return_type_specified.ok_or_else(|| {
                        TypeCheckError::ForeignFunctionNeedsTypeAnnotations(
                            *function_name_span,
                            function_name.clone(),
                        )
                    })?
                };

                add_function_signature(self, return_type.clone());

                typed_ast::Statement::DefineFunction(
                    function_name.clone(),
                    decorators.clone(),
                    type_parameters
                        .iter()
                        .map(|(_, name)| name.clone())
                        .collect(),
                    typed_parameters,
                    body_checked,
                    return_type_annotation
                        .as_ref()
                        .map(|d| d.pretty_print())
                        .unwrap_or_else(|| return_type.to_readable_type(&self.registry)),
                    return_type,
                )
            }
            ast::Statement::DefineDimension(name_span, name, dexprs) => {
                self.type_namespace.add_identifier(
                    name.clone(),
                    *name_span,
                    "dimension".to_owned(),
                )?;

                if let Some(dexpr) = dexprs.first() {
                    self.registry
                        .add_derived_dimension(name, dexpr)
                        .map_err(TypeCheckError::RegistryError)?;

                    let base_representation = self
                        .registry
                        .get_base_representation_for_name(name)
                        .expect("we just inserted it");

                    for alternative_expr in &dexprs[1..] {
                        let alternative_base_representation = self
                            .registry
                            .get_base_representation(alternative_expr)
                            .map_err(TypeCheckError::RegistryError)?;
                        if alternative_base_representation != base_representation {
                            return Err(
                                TypeCheckError::IncompatibleAlternativeDimensionExpression(
                                    name.clone(),
                                    dexpr.full_span(),
                                    base_representation,
                                    alternative_expr.full_span(),
                                    alternative_base_representation,
                                ),
                            );
                        }
                    }
                } else {
                    self.registry
                        .add_base_dimension(name)
                        .map_err(TypeCheckError::RegistryError)?;
                }
                typed_ast::Statement::DefineDimension(name.clone(), dexprs.clone())
            }
            ast::Statement::ProcedureCall(span, kind @ ProcedureKind::Type, args) => {
                if args.len() != 1 {
                    return Err(TypeCheckError::WrongArity {
                        callable_span: *span,
                        callable_name: "type".into(),
                        callable_definition_span: None,
                        arity: 1..=1,
                        num_args: args.len(),
                    });
                }

                let checked_args = args
                    .iter()
                    .map(|e| self.check_expression(e))
                    .collect::<Result<Vec<_>>>()?;

                typed_ast::Statement::ProcedureCall(kind.clone(), checked_args)
            }
            ast::Statement::ProcedureCall(span, kind, args) => {
                let procedure = ffi::procedures().get(kind).unwrap();
                if !procedure.arity.contains(&args.len()) {
                    return Err(TypeCheckError::WrongArity {
                        callable_span: *span,
                        callable_name: procedure.name.clone(),
                        callable_definition_span: None,
                        arity: procedure.arity.clone(),
                        num_args: args.len(),
                    });
                }

                let checked_args = args
                    .iter()
                    .map(|e| self.check_expression(e))
                    .collect::<Result<Vec<_>>>()?;

                match kind {
                    ProcedureKind::Print => {
                        // no argument type checks required, everything can be printed
                    }
                    ProcedureKind::Assert => {
                        if checked_args[0].get_type() != Type::Boolean {
                            return Err(TypeCheckError::IncompatibleTypeInAssert(
                                *span,
                                checked_args[0].get_type(),
                                checked_args[0].full_span(),
                            ));
                        }
                    }
                    ProcedureKind::AssertEq => {
                        let type_first = dtype(&checked_args[0])?;
                        for arg in &checked_args[1..] {
                            let type_arg = dtype(arg)?;
                            if type_arg != type_first {
                                return Err(TypeCheckError::IncompatibleTypesInAssertEq(
                                    *span,
                                    checked_args[0].get_type(),
                                    checked_args[0].full_span(),
                                    arg.get_type(),
                                    arg.full_span(),
                                ));
                            }
                        }
                    }
                    ProcedureKind::Type => {
                        unreachable!("type() calls have a special handling above")
                    }
                }

                typed_ast::Statement::ProcedureCall(kind.clone(), checked_args)
            }
            ast::Statement::ModuleImport(_, _) => {
                unreachable!("Modules should have been inlined by now")
            }
            ast::Statement::DefineStruct {
                struct_name_span,
                struct_name,
                fields,
            } => {
                self.type_namespace.add_identifier(
                    struct_name.clone(),
                    *struct_name_span,
                    "struct".to_owned(),
                )?;

                let mut seen_fields = HashMap::new();

                for (span, field, _) in fields {
                    if let Some(other_span) = seen_fields.get(field) {
                        return Err(TypeCheckError::DuplicateFieldInStructDefinition(
                            *span,
                            *other_span,
                            field.to_string(),
                        ));
                    }

                    seen_fields.insert(field, *span);
                }

                let struct_info = StructInfo {
                    definition_span: *struct_name_span,
                    name: struct_name.clone(),
                    fields: fields
                        .iter()
                        .map(|(span, name, type_)| {
                            Ok((name.clone(), (*span, self.type_from_annotation(type_)?)))
                        })
                        .collect::<Result<_>>()?,
                };
                self.structs
                    .insert(struct_name.clone(), struct_info.clone());

                typed_ast::Statement::DefineStruct(struct_info)
            }
        })
    }

    pub fn check_statements(
        &mut self,
        statements: impl IntoIterator<Item = ast::Statement>,
    ) -> Result<Vec<typed_ast::Statement>> {
        let mut statements_checked = vec![];

        for statement in statements.into_iter() {
            statements_checked.push(self.check_statement(&statement)?);
        }
        Ok(statements_checked)
    }

    pub(crate) fn registry(&self) -> &DimensionRegistry {
        &self.registry
    }

    fn type_from_annotation(&self, annotation: &TypeAnnotation) -> Result<Type> {
        match annotation {
            TypeAnnotation::Never(_) => Ok(Type::Never),
            TypeAnnotation::TypeExpression(dexpr) => {
                if let TypeExpression::TypeIdentifier(_, name) = dexpr {
                    if let Some(info) = self.structs.get(name) {
                        // if we see a struct name here, it's safe to assume it
                        // isn't accidentally clashing with a dimension, we
                        // check that earlier.
                        return Ok(Type::Struct(info.clone()));
                    }
                }

                self.registry
                    .get_base_representation(dexpr)
                    .map(Type::Dimension)
                    .map_err(TypeCheckError::RegistryError)
            }
            TypeAnnotation::Bool(_) => Ok(Type::Boolean),
            TypeAnnotation::String(_) => Ok(Type::String),
            TypeAnnotation::DateTime(_) => Ok(Type::DateTime),
            TypeAnnotation::Fn(_, param_types, return_type) => Ok(Type::Fn(
                param_types
                    .iter()
                    .map(|p| self.type_from_annotation(p))
                    .collect::<Result<Vec<_>>>()?,
                Box::new(self.type_from_annotation(return_type)?),
            )),
        }
    }

    pub fn lookup_function(&self, name: &str) -> Option<&(FunctionSignature, FunctionMetadata)> {
        self.functions.get(name)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::parse;
    use crate::prefix_transformer::Transformer;

    use super::*;

    const TEST_PRELUDE: &str = "
    dimension Scalar = 1
    dimension A
    dimension B
    dimension C = A * B
    unit a: A
    unit b: B
    unit c: C = a * b

    fn returns_a() -> A = a
    fn takes_a_returns_a(x: A) -> A = x
    fn takes_a_returns_b(x: A) -> B = b
    fn takes_a_and_b_returns_c(x: A, y: B) -> C = x * y

    fn error(m: String) -> !
    fn returns_never() -> ! = error(\"…\")
    fn takes_never_returns_a(x: !) -> A = a

    struct SomeStruct { a: A, b: B }

    let callable = takes_a_returns_b
    ";

    fn base_type(name: &str) -> BaseRepresentation {
        BaseRepresentation::from_factor(BaseRepresentationFactor(
            name.into(),
            Rational::from_integer(1),
        ))
    }

    fn type_a() -> BaseRepresentation {
        base_type("A")
    }

    fn type_b() -> BaseRepresentation {
        base_type("B")
    }

    fn type_c() -> BaseRepresentation {
        type_a() * type_b()
    }

    fn run_typecheck(input: &str) -> Result<typed_ast::Statement> {
        let code = &format!("{prelude}\n{input}", prelude = TEST_PRELUDE, input = input);
        let statements = parse(code, 0).expect("No parse errors for inputs in this test suite");
        let transformed_statements = Transformer::new().transform(statements)?;

        TypeChecker::default()
            .check_statements(transformed_statements)
            .map(|mut statements_checked| statements_checked.pop().unwrap())
    }

    fn assert_successful_typecheck(input: &str) {
        if let Err(err) = dbg!(run_typecheck(input)) {
            panic!("Input was expected to typecheck successfully, but failed with: {err:?}")
        }
    }

    fn get_typecheck_error(input: &str) -> TypeCheckError {
        if let Err(err) = dbg!(run_typecheck(input)) {
            err
        } else {
            panic!("Input was expected to yield a type check error");
        }
    }

    #[test]
    fn basic_arithmetic() {
        assert_successful_typecheck("2 a + a");
        assert_successful_typecheck("2 a - a");

        assert_successful_typecheck("a * b");
        assert_successful_typecheck("a / b");

        assert_successful_typecheck("a * b + 2 c");
        assert_successful_typecheck("c / a + b");

        assert!(matches!(
            get_typecheck_error("a + b"),
            TypeCheckError::IncompatibleDimensions(IncompatibleDimensionsError {expected_type, actual_type, ..}) if expected_type == type_a() && actual_type == type_b()
        ));
    }

    #[test]
    fn power_operator_with_scalar_base() {
        assert_successful_typecheck("2^2");
        assert_successful_typecheck("2^(2^2)");

        assert!(matches!(
            get_typecheck_error("2^a"),
            TypeCheckError::NonScalarExponent(_, t) if t == type_a()
        ));
        assert!(matches!(
            get_typecheck_error("2^(c/b)"),
            TypeCheckError::NonScalarExponent(_, t) if t == type_a()
        ));
    }

    #[test]
    fn power_operator_with_dimensionful_base() {
        assert_successful_typecheck("a^2");
        assert_successful_typecheck("a^(2+3)");
        assert_successful_typecheck("a^(2-3)");
        assert_successful_typecheck("a^(2*3)");
        assert_successful_typecheck("a^(2/3)");
        assert_successful_typecheck("a^(2^3)");

        assert!(matches!(
            get_typecheck_error("a^b"),
            TypeCheckError::NonScalarExponent(_, t) if t == type_b()
        ));

        // TODO: if we add ("constexpr") constants later, it would be great to support those in exponents.
        assert!(matches!(
            get_typecheck_error("let x=2
                                 a^x"),
            TypeCheckError::UnsupportedConstEvalExpression(_, desc) if desc == "variable"
        ));

        assert!(matches!(
            get_typecheck_error("a^(3/(1-1))"),
            TypeCheckError::DivisionByZeroInConstEvalExpression(_)
        ));
    }

    #[test]
    fn comparisons() {
        assert_successful_typecheck("2 a > a");
        assert_successful_typecheck("2 a / (3 a) > 3");

        assert!(matches!(
            get_typecheck_error("a > b"),
            TypeCheckError::IncompatibleDimensions(..)
        ));
    }

    #[test]
    fn variable_definitions() {
        assert_successful_typecheck(
            "let x: A = a
             let y: B = b",
        );
        assert_successful_typecheck("let x: C = a * b");
        assert_successful_typecheck("let x: C = 2 * a * b^2 / b");
        assert_successful_typecheck("let x: A^3 = a^20 * a^(-17)");

        assert_successful_typecheck("let x: A = c / b");

        assert_successful_typecheck("let x: Bool = true");
        assert_successful_typecheck("let x: String = \"hello\"");

        assert!(matches!(
            get_typecheck_error("let x: A = b"),
            TypeCheckError::IncompatibleDimensions(IncompatibleDimensionsError {expected_type, actual_type, ..}) if expected_type == type_a() && actual_type == type_b()
        ));
        assert!(matches!(
            get_typecheck_error("let x: A = true"),
            TypeCheckError::IncompatibleTypesInAnnotation(_, _, annotated_type, _, actual_type, _) if annotated_type == Type::Dimension(type_a()) && actual_type == Type::Boolean
        ));
        assert!(matches!(
            get_typecheck_error("let x: A = \"foo\""),
            TypeCheckError::IncompatibleTypesInAnnotation(_, _, annotated_type, _, actual_type, _) if annotated_type == Type::Dimension(type_a()) && actual_type == Type::String
        ));
        assert!(matches!(
            get_typecheck_error("let x: Bool = a"),
            TypeCheckError::IncompatibleTypesInAnnotation(_, _, annotated_type, _, actual_type, _) if annotated_type == Type::Boolean && actual_type == Type::Dimension(type_a())
        ));
        assert!(matches!(
            get_typecheck_error("let x: String = true"),
            TypeCheckError::IncompatibleTypesInAnnotation(_, _, annotated_type, _, actual_type, _) if annotated_type == Type::String && actual_type == Type::Boolean
        ));
    }

    #[test]
    fn unit_definitions() {
        assert_successful_typecheck("unit my_c: C = a * b");
        assert_successful_typecheck("unit foo: A*B^2 = a b^2");

        assert!(matches!(
            get_typecheck_error("unit my_c: C = a"),
            TypeCheckError::IncompatibleDimensions(IncompatibleDimensionsError {expected_type, actual_type, ..}) if expected_type == type_c() && actual_type == type_a()
        ));
    }

    #[test]
    fn function_definitions() {
        assert_successful_typecheck("fn f(x: A) -> A = x");
        assert_successful_typecheck("fn f(x: A) -> A·B = 2 * x * b");
        assert_successful_typecheck("fn f(x: A, y: B) -> C = x * y");

        assert_successful_typecheck("fn f(x: A) = x");

        assert!(matches!(
            get_typecheck_error("fn f(x: A, y: B) -> C = x / y"),
            TypeCheckError::IncompatibleDimensions(IncompatibleDimensionsError {expected_type, actual_type, ..}) if expected_type == type_c() && actual_type == type_a() / type_b()
        ));

        assert!(matches!(
            get_typecheck_error("fn f(x: A) -> A = a\n\
                                 f(b)"),
            TypeCheckError::IncompatibleDimensions(IncompatibleDimensionsError {expected_type, actual_type, ..}) if expected_type == type_a() && actual_type == type_b()
        ));
    }

    #[test]
    fn recursive_functions() {
        assert_successful_typecheck("fn f(x: Scalar) -> Scalar = if x < 0 then f(-x) else x");
        assert_successful_typecheck(
            "fn factorial(n: Scalar) -> Scalar = if n < 0 then 1 else factorial(n - 1) * n",
        );

        assert!(matches!(
            get_typecheck_error("fn f(x: Scalar) -> A = if x < 0 then f(-x) else 2 b"),
            TypeCheckError::IncompatibleTypesInCondition(_, lhs, _, rhs, _) if lhs == Type::Dimension(type_a()) && rhs == Type::Dimension(type_b())
        ));
    }

    #[test]
    fn generics_basic() {
        assert_successful_typecheck(
            "
            fn f<D>(x: D) -> D = x
            f(2)
            f(2 a)
            ",
        );
        assert_successful_typecheck(
            "
            fn f<D>(x: D) -> D^2 = x*x
            f(2)
            f(2 a)
            ",
        );
        assert_successful_typecheck(
            "
            fn f<D0, D1>(x: D0, y: D1) -> D0/D1^2 = x/y^2
            f(2, 3)
            f(2 a, 2 b)
            ",
        );

        assert!(matches!(
            get_typecheck_error("fn f<T1, T2>(x: T1, y: T2) -> T2/T1 = x/y"),
            TypeCheckError::IncompatibleDimensions(IncompatibleDimensionsError {expected_type, actual_type, ..})
                if expected_type == base_type("T2") / base_type("T1") &&
                actual_type == base_type("T1") / base_type("T2")
        ));
    }

    #[test]
    fn generics_multiple_unresolved_type_parameters() {
        assert!(matches!(
            get_typecheck_error(
                "
                fn foo<D1, D2>(x: D1*D2) = 1
                foo(2)
            "
            ),
            TypeCheckError::MultipleUnresolvedTypeParameters(..)
        ));
    }

    #[test]
    fn generics_unused_type_parameter() {
        assert!(matches!(
            get_typecheck_error("
                fn foo<D0>(x: Scalar) -> Scalar = 1
                foo(2)
            "),
            TypeCheckError::CanNotInferTypeParameters(_, _, function_name, parameters) if function_name == "foo" && parameters == "D0"
        ));

        assert!(matches!(
            get_typecheck_error("
                fn foo<D0, D1>(x: D0, y: D0) -> Scalar = 1
                foo(2, 3)
            "),
            TypeCheckError::CanNotInferTypeParameters(_, _, function_name, parameters) if function_name == "foo" && parameters == "D1"
        ));

        assert!(matches!(
            get_typecheck_error("
                fn foo<D0, D1>(x: Scalar, y: Scalar) -> Scalar = 1
                foo(2, 3)
            "),
            TypeCheckError::CanNotInferTypeParameters(_, _, function_name, parameters) if function_name == "foo" && (parameters == "D1, D0" || parameters == "D0, D1")
        ));
    }

    #[test]
    fn generics_type_parameter_name_clash() {
        assert!(matches!(
            get_typecheck_error("
                dimension Existing
                fn f<Existing>(x: Existing) = 1
            "),
            TypeCheckError::TypeParameterNameClash(_, name) if name == "Existing"
        ));

        assert!(matches!(
            get_typecheck_error("
                struct Existing {}
                fn f<Existing>(x: Existing) = 1
            "),
            TypeCheckError::TypeParameterNameClash(_, name) if name == "Existing"
        ));
    }

    #[test]
    fn unknown_identifier() {
        assert!(matches!(
            get_typecheck_error("a + d"),
            TypeCheckError::UnknownIdentifier(_, ident, _) if ident == "d"
        ));
    }

    #[test]
    fn unknown_function() {
        assert!(matches!(
            get_typecheck_error("foo(2)"),
            TypeCheckError::UnknownIdentifier(_, name, _) if name == "foo"
        ));
    }

    #[test]
    fn incompatible_alternative_dimension_expression() {
        assert!(matches!(
            get_typecheck_error(
                "# wrong alternative expression: C / B^2
                 dimension D = A / B = C / B^3"
            ),
            TypeCheckError::IncompatibleAlternativeDimensionExpression(t, ..) if t == "D",
        ));
    }

    #[test]
    fn wrong_arity() {
        assert!(matches!(
            get_typecheck_error("
                fn f() = 1
                f(1)
            "),
            TypeCheckError::WrongArity{callable_span:_, callable_name, callable_definition_span: _, arity, num_args: 1} if arity == (0..=0) && callable_name == "f"
        ));

        assert!(matches!(
            get_typecheck_error("
                fn f(x: Scalar) = x
                f()
            "),
            TypeCheckError::WrongArity{callable_span:_, callable_name, callable_definition_span: _,  arity, num_args: 0} if arity == (1..=1) && callable_name == "f"
        ));

        assert!(matches!(
            get_typecheck_error("
                fn f(x: Scalar) = x
                f(2, 3)
            "),
            TypeCheckError::WrongArity{callable_span:_, callable_name, callable_definition_span: _,  arity, num_args: 2} if arity == (1..=1) && callable_name == "f"
        ));

        assert!(matches!(
            get_typecheck_error("
                fn mean<D>(xs: D…) -> D
                mean()
            "),
            TypeCheckError::WrongArity{callable_span:_, callable_name, callable_definition_span: _,  arity, num_args: 0} if arity == (1..=usize::MAX) && callable_name == "mean"
        ));
    }

    #[test]
    fn variadic_functions() {
        assert!(matches!(
            get_typecheck_error(
                "
                fn mean<D>(xs: D…) -> D
                mean(1 a, 1 b)
            "
            ),
            TypeCheckError::IncompatibleDimensions { .. }
        ));
    }

    #[test]
    fn foreign_function_with_missing_return_type() {
        assert!(matches!(
            get_typecheck_error("fn sin(x: Scalar)"),
            TypeCheckError::ForeignFunctionNeedsTypeAnnotations(_, name) if name == "sin"
        ));
    }

    #[test]
    fn unknown_foreign_function() {
        assert!(matches!(
            get_typecheck_error("fn foo(x: Scalar) -> Scalar"),
            TypeCheckError::UnknownForeignFunction(_, name) if name == "foo"
        ));
    }

    #[test]
    fn arity_checks_in_procedure_calls() {
        assert!(matches!(
            get_typecheck_error("assert_eq(1)"),
            TypeCheckError::WrongArity{callable_span:_, callable_name, callable_definition_span: _,  arity, num_args: 1} if arity == (2..=3) && callable_name == "assert_eq"
        ));
        assert_successful_typecheck("assert_eq(1,2)");
        assert_successful_typecheck("assert_eq(1,2,3)");
        assert!(matches!(
            get_typecheck_error("assert_eq(1,2,3,4)"),
            TypeCheckError::WrongArity{callable_span:_, callable_name, callable_definition_span: _,  arity, num_args: 4} if arity == (2..=3) && callable_name == "assert_eq"
        ));
    }

    #[test]
    fn boolean_values() {
        assert!(matches!(
            get_typecheck_error("-true"),
            TypeCheckError::ExpectedDimensionType(_, _)
        ));
    }

    #[test]
    fn conditionals() {
        assert_successful_typecheck("if true then 1 else 2");
        assert_successful_typecheck("if true then true else false");

        assert!(matches!(
            get_typecheck_error("if 1 then 2 else 3"),
            TypeCheckError::ExpectedBool(_)
        ));

        assert!(matches!(
            get_typecheck_error("if true then a else b"),
            TypeCheckError::IncompatibleTypesInCondition(_, t1, _, t2, _) if t1 == Type::Dimension(base_type("A")) && t2 == Type::Dimension(base_type("B"))
        ));

        assert!(matches!(
            get_typecheck_error("if true then true else a"),
            TypeCheckError::IncompatibleTypesInCondition(_, t1, _, t2, _) if t1 == Type::Boolean && t2 == Type::Dimension(base_type("A"))
        ));
    }

    #[test]
    fn non_dtype_return_types() {
        assert!(matches!(
            get_typecheck_error("fn f() -> String = 1"),
            TypeCheckError::IncompatibleTypesInAnnotation(..)
        ));
        assert!(matches!(
            get_typecheck_error("fn f() -> Scalar = \"test\""),
            TypeCheckError::IncompatibleTypesInAnnotation(..)
        ));

        assert!(matches!(
            get_typecheck_error("fn f() -> Bool = 1"),
            TypeCheckError::IncompatibleTypesInAnnotation(..)
        ));
        assert!(matches!(
            get_typecheck_error("fn f() -> Scalar = true"),
            TypeCheckError::IncompatibleTypesInAnnotation(..)
        ));

        assert!(matches!(
            get_typecheck_error("fn f() -> String = true"),
            TypeCheckError::IncompatibleTypesInAnnotation(..)
        ));
        assert!(matches!(
            get_typecheck_error("fn f() -> Bool = \"test\""),
            TypeCheckError::IncompatibleTypesInAnnotation(..)
        ));
    }

    #[test]
    fn function_types_basic() {
        assert_successful_typecheck(
            "
            let returns_a_ref1 = returns_a
            let returns_a_ref2: Fn[() -> A] = returns_a

            let takes_a_returns_a_ref1 = takes_a_returns_a
            let takes_a_returns_a_ref2: Fn[(A) -> A] = takes_a_returns_a

            let takes_a_returns_b_ref1 = takes_a_returns_b
            let takes_a_returns_b_ref2: Fn[(A) -> B] = takes_a_returns_b

            let takes_a_and_b_returns_C_ref1 = takes_a_and_b_returns_c
            let takes_a_and_b_returns_C_ref2: Fn[(A, B) -> C] = takes_a_and_b_returns_c
            let takes_a_and_b_returns_C_ref3: Fn[(A, B) -> A × B] = takes_a_and_b_returns_c
            ",
        );

        assert!(matches!(
            get_typecheck_error("let wrong_return_type: Fn[() -> B] = returns_a"),
            TypeCheckError::IncompatibleTypesInAnnotation(..)
        ));

        assert!(matches!(
            get_typecheck_error("let wrong_argument_type: Fn[(B) -> A] = takes_a_returns_a"),
            TypeCheckError::IncompatibleTypesInAnnotation(..)
        ));

        assert!(matches!(
            get_typecheck_error("let wrong_argument_count: Fn[(A, B) -> C] = takes_a_returns_a"),
            TypeCheckError::IncompatibleTypesInAnnotation(..)
        ));
    }

    #[test]
    fn function_types_in_return_position() {
        assert_successful_typecheck(
            "
            fn returns_fn1() -> Fn[() -> A] = returns_a
            fn returns_fn2() -> Fn[(A) -> A] = takes_a_returns_a
            fn returns_fn3() -> Fn[(A) -> B] = takes_a_returns_b
            fn returns_fn4() -> Fn[(A, B) -> C] = takes_a_and_b_returns_c
            ",
        );

        assert!(matches!(
            get_typecheck_error("fn returns_fn5() -> Fn[() -> B] = returns_a"),
            TypeCheckError::IncompatibleTypesInAnnotation(..)
        ));
    }

    #[test]
    fn function_types_in_argument_position() {
        assert_successful_typecheck(
            "
            fn takes_fn1(f: Fn[() -> A]) -> A = f()
            fn takes_fn2(f: Fn[(A) -> A]) -> A = f(a)
            fn takes_fn3(f: Fn[(A) -> B]) -> B = f(a)
            fn takes_fn4(f: Fn[(A, B) -> C]) -> C = f(a, b)

            takes_fn1(returns_a)
            takes_fn2(takes_a_returns_a)
            takes_fn3(takes_a_returns_b)
            takes_fn4(takes_a_and_b_returns_c)
            ",
        );

        assert!(matches!(
            get_typecheck_error(
                "
                fn wrong_arity(f: Fn[(A) -> B]) -> B = f()
                "
            ),
            TypeCheckError::WrongArity { .. }
        ));

        assert!(matches!(
            get_typecheck_error(
                "
                fn wrong_argument_type(f: Fn[(A) -> B]) -> B = f(b)
                "
            ),
            TypeCheckError::IncompatibleTypesInFunctionCall(..)
        ));

        assert!(matches!(
            get_typecheck_error(
                "
                fn wrong_return_type(f: Fn[() -> A]) -> B = f()
                "
            ),
            TypeCheckError::IncompatibleDimensions(..)
        ));

        assert!(matches!(
            get_typecheck_error(
                "
                fn argument_mismatch(f: Fn[() -> A]) -> A = f()
                argument_mismatch(takes_a_returns_a)
                "
            ),
            TypeCheckError::IncompatibleTypesInFunctionCall(..)
        ));
    }

    #[test]
    fn no_dimensionless_base_units() {
        assert!(matches!(
            get_typecheck_error(
                "
                unit page: Scalar
                "
            ),
            TypeCheckError::NoDimensionlessBaseUnit { .. }
        ));
    }

    #[test]
    fn never_type() {
        // Expressions
        assert_successful_typecheck("2 + returns_never()");
        assert_successful_typecheck("a + returns_never()");
        assert_successful_typecheck("(a + returns_never()) + a");
        assert_successful_typecheck("returns_never() + returns_never()");
        assert!(matches!(
            get_typecheck_error("(a + returns_never()) + b"),
            TypeCheckError::IncompatibleDimensions(..)
        ));

        // Variable assignments
        assert_successful_typecheck("let x: ! = returns_never()");
        assert_successful_typecheck("let x: A = returns_never()");

        // Conditionals
        assert_successful_typecheck("(if true then a else returns_never()) -> a");
        assert_successful_typecheck("(if true then returns_never() else a) -> a");
        assert!(matches!(
            get_typecheck_error("(if true then returns_never() else a) -> b"),
            TypeCheckError::IncompatibleDimensions(..)
        ));
        assert!(matches!(
            get_typecheck_error("let x: A = if true then returns_never() else b"),
            TypeCheckError::IncompatibleDimensions(..)
        ));
        assert_successful_typecheck("let x: ! = if true then returns_never() else returns_never()");

        // Function calls
        assert_successful_typecheck("let x: A = takes_a_returns_a(returns_never())");
        assert_successful_typecheck(
            "let x: C = takes_a_and_b_returns_c(returns_never(), returns_never())",
        );
        assert_successful_typecheck("let x: A = takes_never_returns_a(returns_never())");
        assert!(matches!(
            get_typecheck_error("takes_never_returns_a(a)"),
            TypeCheckError::IncompatibleTypesInFunctionCall(..)
        ));

        // Function definitions
        assert_successful_typecheck("fn my_returns_never() -> ! = returns_never()");
        assert_successful_typecheck("fn my_takes_never_returns_a(x: !) -> A = a");
        assert!(matches!(
            get_typecheck_error("fn attempts_to_return_never() -> ! = a"),
            TypeCheckError::IncompatibleTypesInAnnotation(..)
        ));

        // Generic functions:
        assert_successful_typecheck(
            "
            fn absurd<T>(x: !) -> A = returns_never()
            ",
        );
        assert_successful_typecheck(
            "
            fn check_and_return<T>(precondition: Bool, t: T) -> T =
              if precondition then t else error(\"precondition failed\")
            ",
        );
    }

    #[test]
    fn callables() {
        assert_successful_typecheck("callable(a)");
        assert_successful_typecheck("a -> callable");
        assert!(matches!(
            get_typecheck_error("callable(b)"),
            TypeCheckError::IncompatibleTypesInFunctionCall(..)
        ));
        assert!(matches!(
            get_typecheck_error("callable()"),
            TypeCheckError::WrongArity { .. }
        ));
        assert!(matches!(
            get_typecheck_error("callable(a, a)"),
            TypeCheckError::WrongArity { .. }
        ));

        assert!(matches!(
            get_typecheck_error("a + callable"),
            TypeCheckError::ExpectedDimensionType { .. }
        ));
        assert!(matches!(
            get_typecheck_error("callable == callable"),
            TypeCheckError::IncompatibleTypesInComparison { .. }
        ));
    }

    #[test]
    fn structs() {
        assert_successful_typecheck(
            "
          struct Foo {
            foo: A,
            bar: C
          }

          let s = Foo {
            foo: 1a,
            bar: 2c
          }

          let foo: A = s.foo
          let bar: C = s.bar
          ",
        );

        assert!(matches!(
            get_typecheck_error("SomeStruct {a: 1, b: 1b}"),
            TypeCheckError::IncompatibleTypesForStructField(..)
        ));

        assert!(matches!(
            get_typecheck_error("NotAStruct {}"),
            TypeCheckError::UnknownStruct(_, name) if name == "NotAStruct"
        ));

        assert!(matches!(
            get_typecheck_error("SomeStruct {not_a_field: 1}"),
            TypeCheckError::UnknownFieldInStructInstantiation(_, _, field, _) if field == "not_a_field"
        ));

        assert!(matches!(
            get_typecheck_error("struct Foo { foo: A, foo: A }"),
            TypeCheckError::DuplicateFieldInStructDefinition(_, _, field) if field == "foo"
        ));

        assert!(matches!(
            get_typecheck_error("SomeStruct {a: 1a, a: 1a, b: 2b}"),
            TypeCheckError::DuplicateFieldInStructInstantiation(_, _, field) if field == "a"
        ));

        assert!(matches!(
            get_typecheck_error("SomeStruct {a: 1a, b: 1b}.foo"),
            TypeCheckError::UnknownFieldAccess(_, _, field, _) if field == "foo"
        ));

        assert!(matches!(
            get_typecheck_error("(1).foo"),
            TypeCheckError::FieldAccessOfNonStructType(_, _, field, _) if field == "foo"
        ));

        assert!(matches!(
            get_typecheck_error("SomeStruct {}"),
            TypeCheckError::MissingFieldsInStructInstantiation(..)
        ));
    }

    #[test]
    fn name_resolution() {
        assert!(matches!(
            get_typecheck_error(
                "
                dimension Foo
                struct Foo {}
                "
            ),
            TypeCheckError::NameResolutionError(NameResolutionError::IdentifierClash { .. })
        ));

        assert!(matches!(
            get_typecheck_error(
                "
                struct Foo {}
                dimension Foo
                "
            ),
            TypeCheckError::NameResolutionError(NameResolutionError::IdentifierClash { .. })
        ));

        assert!(matches!(
            get_typecheck_error(
                "
                fn foo() -> Scalar = 1
                let foo = 1
                "
            ),
            TypeCheckError::NameResolutionError(NameResolutionError::IdentifierClash { .. })
        ));

        assert!(matches!(
            get_typecheck_error(
                "
                fn foo() -> Scalar = 1
                unit foo: Scalar
                "
            ),
            TypeCheckError::NameResolutionError(NameResolutionError::IdentifierClash { .. })
        ));

        assert_successful_typecheck(
            "
            let Foo = 1
            dimension Foo
            ",
        );

        assert_successful_typecheck(
            "
            fn Foo() -> Scalar = 1
            dimension Foo
            ",
        );

        assert_successful_typecheck(
            "
            fn Foo() -> Scalar = 1
            struct Foo {}
            ",
        );

        assert_successful_typecheck(
            "
            fn Foo() -> Scalar = 1
            fn Foo() -> Scalar = 2
            ",
        );
    }
}
