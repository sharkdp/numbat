#[cfg(test)]
mod tests;

mod const_evaluation;
mod constraints;
mod environment;
mod error;
mod incompatible_dimensions;
pub mod map_stack;
mod name_generator;
pub mod qualified_type;
mod substitutions;
pub mod type_scheme;

use std::collections::HashMap;
use std::ops::Deref;
use std::sync::Arc;

use crate::arithmetic::Exponent;
use crate::ast::{
    self, BinaryOperator, DefineVariable, ProcedureKind, StringPart, TypeAnnotation,
    TypeExpression, TypeParameterBound,
};
use crate::dimension::DimensionRegistry;
use crate::name_resolution::Namespace;
use crate::name_resolution::LAST_RESULT_IDENTIFIERS;
use crate::pretty_print::PrettyPrint;
use crate::span::Span;
use crate::type_variable::TypeVariable;
use crate::typed_ast::{self, DType, DTypeFactor, Expression, StructInfo, Type};
use crate::{decorator, ffi, suggestion};

use compact_str::{format_compact, CompactString, ToCompactString};
use const_evaluation::evaluate_const_expr;
use constraints::{Constraint, ConstraintSet, ConstraintSolverError, TrivialResolution};
use environment::{Environment, FunctionMetadata, FunctionSignature};
use itertools::Itertools;
use name_generator::NameGenerator;
use num_traits::Zero;

pub use error::{Result, TypeCheckError};
pub use incompatible_dimensions::IncompatibleDimensionsError;
use qualified_type::Bound;
use substitutions::{ApplySubstitution, Substitution};
use type_scheme::TypeScheme;

fn dtype(e: &Expression) -> Result<DType> {
    match e.get_type() {
        Type::Dimension(dtype) => Ok(dtype),
        t => Err(Box::new(TypeCheckError::ExpectedDimensionType(
            e.full_span(),
            t,
        ))),
    }
}

struct ProperFunctionCallArgs<'a, 'b> {
    registry: &'b DimensionRegistry,
    constraints: &'b mut ConstraintSet,
    name_generator: &'b mut NameGenerator,
    span: &'b Span,
    full_span: &'b Span,
    function_name: &'a str,
    signature: &'b FunctionSignature,
    arguments: Vec<typed_ast::Expression<'a>>,
    argument_types: Vec<Type>,
}

fn proper_function_call<'a>(
    ProperFunctionCallArgs {
        registry,
        constraints,
        name_generator,
        span,
        full_span,
        function_name,
        signature,
        arguments,
        argument_types,
    }: ProperFunctionCallArgs<'a, '_>,
) -> Result<typed_ast::Expression<'a>> {
    let FunctionSignature {
        name: _,
        definition_span,
        type_parameters: _,
        parameters,
        return_type_annotation: _,
        fn_type,
    } = signature;

    let fn_type = match fn_type {
        TypeScheme::Concrete(t) => {
            // This branch is needed for recursive functions, where the type of the function
            // is not yet known (and not yet quantified).
            t.clone()
        }
        TypeScheme::Quantified(_, _) => {
            let qt = fn_type.instantiate(name_generator);

            for Bound::IsDim(t) in qt.bounds.iter() {
                constraints.add_dtype_constraint(t).ok();
            }

            qt.inner
        }
    };

    let Type::Fn(parameter_types, return_type) = fn_type else {
        unreachable!("Expected function type, got {:#?}", fn_type);
    };

    let arity_range = parameters.len()..=parameters.len();

    if !arity_range.contains(&arguments.len()) {
        return Err(Box::new(TypeCheckError::WrongArity {
            callable_span: *span,
            callable_name: function_name.to_owned(),
            callable_definition_span: Some(*definition_span),
            arity: arity_range,
            num_args: arguments.len(),
        }));
    }

    for (idx, ((parameter_span, parameter_type), argument_type)) in parameters
        .iter()
        .map(|p| p.0)
        .zip(parameter_types.iter())
        .zip(argument_types)
        .enumerate()
    {
        if constraints
            .add_equal_constraint(parameter_type, &argument_type)
            .is_trivially_violated()
        {
            match (parameter_type, &argument_type) {
                (Type::Dimension(parameter_dtype), Type::Dimension(argument_dtype)) => {
                    return Err(Box::new(TypeCheckError::IncompatibleDimensions(
                        IncompatibleDimensionsError {
                            span_operation: *span,
                            operation: format_compact!(
                                "argument {num} of function call to '{name}'",
                                num = idx + 1,
                                name = function_name
                            ),
                            span_expected: parameter_span,
                            expected_name: "parameter type",
                            expected_dimensions: registry.get_derived_entry_names_for(
                                &parameter_dtype.to_base_representation(),
                            ),
                            expected_type: parameter_dtype.to_base_representation(),
                            span_actual: arguments[idx].full_span(),
                            actual_name: " argument type",
                            actual_name_for_fix: "function argument",
                            actual_dimensions: registry.get_derived_entry_names_for(
                                &argument_dtype.to_base_representation(),
                            ),
                            actual_type: argument_dtype.to_base_representation(),
                        },
                    )));
                }
                _ => {
                    return Err(Box::new(TypeCheckError::IncompatibleTypesInFunctionCall(
                        Some(parameter_span),
                        parameter_type.clone(),
                        arguments[idx].full_span(),
                        argument_type.clone(),
                    )));
                }
            }
        }
    }

    Ok(typed_ast::Expression::FunctionCall(
        *span,
        *full_span,
        function_name,
        arguments,
        TypeScheme::concrete(return_type.as_ref().clone()),
    ))
}

#[derive(Clone, Default)]
pub struct TypeChecker {
    structs: HashMap<CompactString, StructInfo>,
    registry: DimensionRegistry,

    type_namespace: Namespace,
    value_namespace: Namespace,
    env: Environment,

    name_generator: NameGenerator,
    constraints: ConstraintSet,
}

struct ElaborationDefinitionArgs<'a, 'b> {
    identifier_span: Span,
    expr: &'b ast::Expression<'a>,
    type_annotation_span: Option<Span>,
    type_annotation: Option<&'b TypeAnnotation>,
    operation: &'b str,
    expected_name: &'static str,
    actual_name: &'static str,
    actual_name_for_fix: &'static str,
    elaboration_kind: &'b str,
}

impl TypeChecker {
    fn fresh_type_variable(&mut self) -> Type {
        Type::TVar(self.name_generator.fresh_type_variable())
    }

    fn add_equal_constraint(&mut self, lhs: &Type, rhs: &Type) -> TrivialResolution {
        self.constraints.add_equal_constraint(lhs, rhs)
    }

    fn add_dtype_constraint(&mut self, type_: &Type) -> TrivialResolution {
        self.constraints.add_dtype_constraint(type_)
    }

    fn enforce_dtype(&mut self, type_: &Type, span: Span) -> Result<()> {
        if self
            .constraints
            .add(Constraint::IsDType(type_.clone()))
            .is_trivially_violated()
        {
            return Err(Box::new(TypeCheckError::ExpectedDimensionType(
                span,
                type_.clone(),
            )));
        }

        Ok(())
    }

    fn type_from_annotation(&self, annotation: &TypeAnnotation) -> Result<Type> {
        match annotation {
            TypeAnnotation::TypeExpression(dexpr) => {
                if let TypeExpression::TypeIdentifier(_, name) = dexpr {
                    if let Some(info) = self.structs.get(name) {
                        // if we see a struct name here, it's safe to assume it
                        // isn't accidentally clashing with a dimension, we
                        // check that earlier.
                        return Ok(Type::Struct(Box::new(info.clone())));
                    }
                }

                let mut factors = self
                    .registry
                    .get_base_representation(dexpr)
                    .map(DType::from)
                    .map_err(TypeCheckError::RegistryError)?
                    .into_factors();

                // Replace BaseDimension("D") with TVar("D") for all type parameters
                for (factor, _) in Arc::make_mut(&mut factors) {
                    *factor = match factor {
                        DTypeFactor::BaseDimension(ref n)
                            if self
                                .registry
                                .introduced_type_parameters
                                .iter()
                                .any(|(_, name, _)| name == n) =>
                        {
                            DTypeFactor::TPar(n.clone())
                        }
                        ref f => f.deref().clone(),
                    }
                }

                Ok(Type::Dimension(DType::from_factors(factors)))
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
            TypeAnnotation::List(_, element_type) => Ok(Type::List(Box::new(
                self.type_from_annotation(element_type)?,
            ))),
        }
    }

    fn identifier_type(&self, span: Span, name: &str) -> Result<TypeScheme> {
        Ok(self.env.get_identifier_type(name).ok_or_else(|| {
            let suggestion = suggestion::did_you_mean(
                self.env
                    .iter_identifiers()
                    .map(|k| k.as_str())
                    .chain(["true", "false"]) // These are parsed as keywords, but can act like identifiers
                    .chain(ffi::procedures().values().map(|p| p.name)),
                name,
            );
            TypeCheckError::UnknownIdentifier(span, name.into(), suggestion)
        })?)
    }

    fn elaborate_expression<'a>(
        &mut self,
        ast: &ast::Expression<'a>,
    ) -> Result<typed_ast::Expression<'a>> {
        Ok(match ast {
            ast::Expression::Scalar(span, n)
                if n.to_f64().is_zero() || n.to_f64().is_infinite() || n.to_f64().is_nan() =>
            {
                let polymorphic_zero_type = self.fresh_type_variable();
                self.add_dtype_constraint(&polymorphic_zero_type).ok();
                typed_ast::Expression::Scalar(
                    *span,
                    *n,
                    TypeScheme::concrete(polymorphic_zero_type),
                )
            }
            ast::Expression::Scalar(span, n) => {
                typed_ast::Expression::Scalar(*span, *n, TypeScheme::concrete(Type::scalar()))
            }
            ast::Expression::Identifier(span, name) => {
                let type_scheme = self.identifier_type(*span, name)?.clone();

                let ty = match type_scheme {
                    TypeScheme::Concrete(ty) => ty,
                    TypeScheme::Quantified(_, _) => {
                        let qt = type_scheme.instantiate(&mut self.name_generator);

                        for Bound::IsDim(t) in qt.bounds.iter() {
                            self.constraints.add(Constraint::IsDType(t.clone())).ok();
                        }
                        qt.inner
                    }
                };

                typed_ast::Expression::Identifier(*span, name, TypeScheme::concrete(ty))
            }
            ast::Expression::UnitIdentifier(span, prefix, name, full_name) => {
                let type_scheme = self.identifier_type(*span, name)?.clone();

                let qt = type_scheme.instantiate(&mut self.name_generator);

                for Bound::IsDim(t) in qt.bounds.iter() {
                    self.constraints.add(Constraint::IsDType(t.clone())).ok();
                }

                typed_ast::Expression::UnitIdentifier(
                    *span,
                    *prefix,
                    name.clone(),
                    full_name.clone(),
                    TypeScheme::concrete(qt.inner),
                )
            }
            ast::Expression::UnaryOperator { op, expr, span_op } => {
                let checked_expr = self.elaborate_expression(expr)?;
                let type_ = checked_expr.get_type();

                match op {
                    ast::UnaryOperator::Factorial(_order) => {
                        if self
                            .add_equal_constraint(&type_, &Type::scalar())
                            .is_trivially_violated()
                        {
                            return Err(Box::new(TypeCheckError::NonScalarFactorialArgument(
                                expr.full_span(),
                                type_,
                            )));
                        }
                    }
                    ast::UnaryOperator::Negate => {
                        self.enforce_dtype(&type_, expr.full_span())?;
                    }
                    ast::UnaryOperator::LogicalNeg => {
                        if self
                            .add_equal_constraint(&type_, &Type::Boolean)
                            .is_trivially_violated()
                        {
                            return Err(Box::new(TypeCheckError::ExpectedBool(expr.full_span())));
                        }
                    }
                }

                typed_ast::Expression::UnaryOperator(
                    *span_op,
                    *op,
                    Box::new(checked_expr),
                    TypeScheme::concrete(type_),
                )
            }
            ast::Expression::BinaryOperator {
                op,
                lhs,
                rhs,
                span_op,
            } => {
                let lhs_checked = self.elaborate_expression(lhs)?;
                let rhs_checked = self.elaborate_expression(rhs)?;

                let lhs_type = lhs_checked.get_type();
                let rhs_type = rhs_checked.get_type();

                if rhs_type.is_fn_type() && op == &BinaryOperator::ConvertTo {
                    let (parameter_types, return_type) = match rhs_type {
                        Type::Fn(p, r) => (p, r),
                        _ => unreachable!(),
                    };
                    // make sure that there is just one paramter (return arity error otherwise)
                    if parameter_types.len() != 1 {
                        return Err(Box::new(TypeCheckError::WrongArity {
                            callable_span: rhs.full_span(),
                            callable_name: "function".into(),
                            callable_definition_span: None,
                            arity: 1..=1,
                            num_args: parameter_types.len(),
                        }));
                    }

                    if self
                        .add_equal_constraint(&lhs_type, &parameter_types[0])
                        .is_trivially_violated()
                    {
                        return Err(Box::new(TypeCheckError::IncompatibleTypesInFunctionCall(
                            None,
                            parameter_types[0].clone(),
                            lhs.full_span(),
                            lhs_type,
                        )));
                    }

                    typed_ast::Expression::CallableCall(
                        lhs.full_span(),
                        Box::new(rhs_checked),
                        vec![lhs_checked],
                        TypeScheme::concrete(*return_type),
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
                        let time = DType::base_dimension("Time"); // TODO: error handling
                                                                  // TODO make sure the "second" unit exists

                        typed_ast::Expression::BinaryOperatorForDate(
                            *span_op,
                            *op,
                            Box::new(lhs_checked),
                            Box::new(rhs_checked),
                            TypeScheme::concrete(Type::Dimension(time)),
                        )
                    } else if (*op == BinaryOperator::Add || *op == BinaryOperator::Sub)
                        && rhs_is_time
                    {
                        typed_ast::Expression::BinaryOperatorForDate(
                            *span_op,
                            *op,
                            Box::new(lhs_checked),
                            Box::new(rhs_checked),
                            TypeScheme::concrete(Type::DateTime),
                        )
                    } else {
                        return Err(Box::new(TypeCheckError::IncompatibleTypesInOperator(
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
                        )));
                    }
                } else {
                    let mut get_type_and_assert_equal_dtypes = || -> Result<Type> {
                        let lhs_type = lhs_checked.get_type();
                        let rhs_type = rhs_checked.get_type();

                        if self
                            .add_equal_constraint(&lhs_type, &rhs_type)
                            .is_trivially_violated()
                        {
                            let lhs_dtype = dtype(&lhs_checked)?;
                            let rhs_dtype = dtype(&rhs_checked)?;
                            let full_span = ast::Expression::BinaryOperator {
                                op: *op,
                                lhs: lhs.clone(),
                                rhs: rhs.clone(),
                                span_op: *span_op,
                            }
                            .full_span();
                            return Err(Box::new(TypeCheckError::IncompatibleDimensions(
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
                                    expected_dimensions: self.registry.get_derived_entry_names_for(
                                        &lhs_dtype.to_base_representation(),
                                    ),
                                    expected_type: lhs_dtype.to_base_representation(),
                                    span_actual: rhs.full_span(),
                                    actual_name: "right hand side",
                                    actual_name_for_fix: "expression on the right hand side",
                                    actual_dimensions: self.registry.get_derived_entry_names_for(
                                        &rhs_dtype.to_base_representation(),
                                    ),
                                    actual_type: rhs_dtype.to_base_representation(),
                                },
                            )));
                        }

                        self.enforce_dtype(&lhs_type, lhs.full_span())?;
                        self.enforce_dtype(&rhs_type, rhs.full_span())?;

                        Ok(lhs_type)
                    };

                    let type_ = match op {
                        typed_ast::BinaryOperator::Add => get_type_and_assert_equal_dtypes()?,
                        typed_ast::BinaryOperator::Sub => get_type_and_assert_equal_dtypes()?,
                        typed_ast::BinaryOperator::Mul | typed_ast::BinaryOperator::Div => {
                            let type_lhs = lhs_checked.get_type();
                            let type_rhs = rhs_checked.get_type();

                            if type_lhs.is_closed() && type_rhs.is_closed() {
                                let lhs_dtype = dtype(&lhs_checked)?;
                                let rhs_dtype = dtype(&rhs_checked)?;

                                match op {
                                    typed_ast::BinaryOperator::Mul => {
                                        Type::Dimension(lhs_dtype.multiply(&rhs_dtype))
                                    }
                                    typed_ast::BinaryOperator::Div => {
                                        Type::Dimension(lhs_dtype.divide(&rhs_dtype))
                                    }
                                    _ => unreachable!(),
                                }
                            } else {
                                self.enforce_dtype(&type_lhs, lhs_checked.full_span())?;
                                self.enforce_dtype(&type_rhs, rhs_checked.full_span())?;

                                // We first introduce a fresh type variable for the result
                                let tv_result = self.name_generator.fresh_type_variable();
                                let type_result = Type::TVar(tv_result.clone());

                                // … and make sure that it is a dimension type
                                self.add_dtype_constraint(&type_result).ok();

                                // We can't use type_lhs/type_rhs directly in a dimension expression, because
                                // only DTypes can be used there. But we don't know if type_lhs/type_rhs are
                                // indeed dimension types. So we make up new type variables tv_lhs/tv_rhs, and
                                // add contraints type_lhs ~ type(tv_lhs), type_rhs ~ type(tv_rhs). We can then
                                // use those type variables inside the dimension expression constraint.

                                let tv_lhs = self.name_generator.fresh_type_variable();
                                let tv_rhs = self.name_generator.fresh_type_variable();

                                self.constraints
                                    .add(Constraint::Equal(type_lhs, Type::TVar(tv_lhs.clone())))
                                    .ok();
                                self.constraints
                                    .add(Constraint::Equal(type_rhs, Type::TVar(tv_rhs.clone())))
                                    .ok();

                                // we also need dtype constraints for those new type variables
                                self.add_dtype_constraint(&Type::TVar(tv_lhs.clone())).ok();
                                self.add_dtype_constraint(&Type::TVar(tv_rhs.clone())).ok();

                                // Finally, we add the constraint that the result is the product of the two,
                                // which we write as
                                //
                                //     dtype_lhs × dtype_rhs × dtype_result^-1 ~ Scalar
                                //
                                // Or for division:
                                //
                                //     dtype_lhs × dtype_rhs^-1 × dtype_result ~ Scalar
                                //
                                let dtype_lhs = DType::from_type_variable(tv_lhs);
                                let dtype_rhs = DType::from_type_variable(tv_rhs);
                                let dtype_result = DType::from_type_variable(tv_result);

                                match op {
                                    typed_ast::BinaryOperator::Mul => {
                                        self.constraints
                                            .add(Constraint::EqualScalar(
                                                dtype_lhs
                                                    .multiply(&dtype_rhs)
                                                    .multiply(&dtype_result.inverse()),
                                            ))
                                            .ok();
                                    }
                                    typed_ast::BinaryOperator::Div => {
                                        self.constraints
                                            .add(Constraint::EqualScalar(
                                                (dtype_lhs.divide(&dtype_rhs))
                                                    .multiply(&dtype_result.inverse()),
                                            ))
                                            .ok();
                                    }
                                    _ => unreachable!(),
                                }

                                type_result
                            }
                        }
                        typed_ast::BinaryOperator::Power => {
                            let type_base_inferred = lhs_type;
                            let type_exponent_inferred = rhs_type;

                            self.enforce_dtype(&type_base_inferred, lhs.full_span())?;
                            self.enforce_dtype(&type_exponent_inferred, rhs.full_span())?;

                            match type_base_inferred {
                                Type::Dimension(base_dtype) if base_dtype.is_scalar() => {
                                    // Skip evaluating the exponent if the lhs is a scalar. This allows
                                    // for arbitrary (decimal) exponents, if the base is a scalar.

                                    if self
                                        .add_equal_constraint(
                                            &type_exponent_inferred,
                                            &Type::scalar(),
                                        )
                                        .is_trivially_violated()
                                    {
                                        return Err(Box::new(TypeCheckError::NonScalarExponent(
                                            rhs.full_span(),
                                            type_exponent_inferred,
                                        )));
                                    }

                                    Type::Dimension(base_dtype)
                                }
                                Type::Dimension(base_dtype) => {
                                    let exponent = evaluate_const_expr(&rhs_checked)?;
                                    Type::Dimension(base_dtype.power(exponent))
                                }
                                _ => {
                                    if let Ok(exponent) = evaluate_const_expr(&rhs_checked) {
                                        // Type inference in this case follows a similar pattern to multiplication/division. See
                                        // there for an explanation

                                        let tv_result = self.name_generator.fresh_type_variable();
                                        let type_result = Type::TVar(tv_result.clone());
                                        let dtype_result = DType::from_type_variable(tv_result);
                                        self.add_dtype_constraint(&type_result).ok();

                                        let tv_base = self.name_generator.fresh_type_variable();
                                        let type_base = Type::TVar(tv_base.clone());
                                        let dtype_base = DType::from_type_variable(tv_base);
                                        self.add_dtype_constraint(&type_base).ok();

                                        self.add_equal_constraint(&type_base, &type_base_inferred)
                                            .ok();

                                        self.constraints
                                            .add(Constraint::EqualScalar(
                                                dtype_result.multiply(&dtype_base.power(-exponent)),
                                            ))
                                            .ok();

                                        type_result
                                    } else {
                                        return Err(Box::new(
                                            TypeCheckError::ExponentiationNeedsTypeAnnotation(
                                                lhs_checked
                                                    .full_span()
                                                    .extend(&rhs_checked.full_span()),
                                            ),
                                        ));
                                    }
                                }
                            }
                        }
                        typed_ast::BinaryOperator::ConvertTo => get_type_and_assert_equal_dtypes()?,
                        typed_ast::BinaryOperator::LessThan
                        | typed_ast::BinaryOperator::GreaterThan
                        | typed_ast::BinaryOperator::LessOrEqual
                        | typed_ast::BinaryOperator::GreaterOrEqual => {
                            let _ = get_type_and_assert_equal_dtypes()?;
                            Type::Boolean
                        }
                        typed_ast::BinaryOperator::Equal | typed_ast::BinaryOperator::NotEqual => {
                            if lhs_type.is_closed() && rhs_type.is_closed() {
                                if lhs_type.is_dtype() && rhs_type.is_dtype() {
                                    let _ = get_type_and_assert_equal_dtypes()?;
                                } else if lhs_type != rhs_type
                                    || lhs_type.is_fn_type()
                                    || rhs_type.is_fn_type()
                                {
                                    return Err(Box::new(
                                        TypeCheckError::IncompatibleTypesInComparison(
                                            span_op.unwrap(),
                                            lhs_type,
                                            lhs.full_span(),
                                            rhs_type,
                                            rhs.full_span(),
                                        ),
                                    ));
                                }
                            } else {
                                self.add_equal_constraint(&lhs_type, &rhs_type).ok();
                            }

                            Type::Boolean
                        }
                        typed_ast::BinaryOperator::LogicalAnd
                        | typed_ast::BinaryOperator::LogicalOr => {
                            if self
                                .add_equal_constraint(&lhs_type, &Type::Boolean)
                                .is_trivially_violated()
                            {
                                return Err(Box::new(TypeCheckError::ExpectedBool(
                                    lhs.full_span(),
                                )));
                            }
                            if self
                                .add_equal_constraint(&rhs_type, &Type::Boolean)
                                .is_trivially_violated()
                            {
                                return Err(Box::new(TypeCheckError::ExpectedBool(
                                    rhs.full_span(),
                                )));
                            }

                            Type::Boolean
                        }
                    };

                    typed_ast::Expression::BinaryOperator(
                        *span_op,
                        *op,
                        Box::new(lhs_checked),
                        Box::new(rhs_checked),
                        TypeScheme::concrete(type_),
                    )
                }
            }
            ast::Expression::FunctionCall(span, full_span, callable, args) => {
                let arguments_checked = args
                    .iter()
                    .map(|a| self.elaborate_expression(a))
                    .collect::<Result<Vec<_>>>()?;
                let argument_types = arguments_checked
                    .iter()
                    .map(|e| e.get_type())
                    .collect::<Vec<Type>>();

                // There are two options here. The 'callable' can either be a direct reference
                // to a (proper) function, or it can be an arbitrary complicated expression
                // that evaluates to a function "pointer".

                if let Some((function_name, signature)) =
                    self.env.get_proper_function_reference(callable)
                {
                    proper_function_call(ProperFunctionCallArgs {
                        registry: &mut self.registry,
                        constraints: &mut self.constraints,
                        name_generator: &mut self.name_generator,
                        span,
                        full_span,
                        function_name,
                        signature,
                        arguments: arguments_checked,
                        argument_types,
                    })?
                } else {
                    let callable_checked = self.elaborate_expression(callable)?;
                    let callable_type = callable_checked.get_type();

                    let parameter_types = (0..arguments_checked.len())
                        .map(|_| self.fresh_type_variable())
                        .collect::<Vec<_>>();
                    let return_type = self.fresh_type_variable();

                    if self
                        .add_equal_constraint(
                            &callable_type,
                            &Type::Fn(parameter_types.clone(), Box::new(return_type.clone())),
                        )
                        .is_trivially_violated()
                    {
                        match callable_type {
                            Type::Fn(concrete_parameter_types, _) => {
                                let num_parameters = concrete_parameter_types.len();
                                let num_arguments = arguments_checked.len();

                                if num_parameters != num_arguments {
                                    return Err(Box::new(TypeCheckError::WrongArity {
                                        callable_span: *span,
                                        callable_name: "function".into(),
                                        callable_definition_span: None,
                                        arity: num_parameters..=num_parameters,
                                        num_args: num_arguments,
                                    }));
                                }

                                // for (param_type, arg_checked) in
                                //     concrete_parameter_types.iter().zip(&arguments_checked)
                                // {
                                //     if &arg_checked.get_type() != param_type {
                                //         return Err(
                                //             TypeCheckError::IncompatibleTypesInFunctionCall(
                                //                 None,
                                //                 param_type.clone(),
                                //                 arg_checked.full_span(),
                                //                 arg_checked.get_type(),
                                //             ),
                                //         );
                                //     }
                                // }
                            }
                            _ => {
                                return Err(Box::new(
                                    TypeCheckError::OnlyFunctionsAndReferencesCanBeCalled(
                                        callable.full_span(),
                                    ),
                                ));
                            }
                        }
                    }

                    for ((argument_type, arguments_checked), parameter_type) in argument_types
                        .iter()
                        .zip(&arguments_checked)
                        .zip(&parameter_types)
                    {
                        if self
                            .add_equal_constraint(argument_type, parameter_type)
                            .is_trivially_violated()
                        {
                            return Err(Box::new(TypeCheckError::IncompatibleTypesInFunctionCall(
                                Some(arguments_checked.full_span()),
                                argument_type.clone(),
                                callable.full_span(),
                                parameter_type.clone(),
                            )));
                        }
                    }

                    typed_ast::Expression::CallableCall(
                        *full_span,
                        Box::new(callable_checked),
                        arguments_checked,
                        TypeScheme::concrete(return_type),
                    )
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
                            format_specifiers: format_specifiers.as_ref().copied(),
                            expr: Box::new(self.elaborate_expression(expr)?),
                        }),
                    })
                    .collect::<Result<_>>()?,
            ),
            ast::Expression::Condition(span, condition, then, else_) => {
                let condition = self.elaborate_expression(condition)?;

                if self
                    .add_equal_constraint(&condition.get_type(), &Type::Boolean)
                    .is_trivially_violated()
                {
                    return Err(Box::new(TypeCheckError::ExpectedBool(
                        condition.full_span(),
                    )));
                }

                let then = self.elaborate_expression(then)?;
                let else_ = self.elaborate_expression(else_)?;

                let then_type = then.get_type();
                let else_type = else_.get_type();

                if self
                    .add_equal_constraint(&then_type, &else_type)
                    .is_trivially_violated()
                {
                    return Err(Box::new(TypeCheckError::IncompatibleTypesInCondition(
                        *span,
                        then_type,
                        then.full_span(),
                        else_type,
                        else_.full_span(),
                    )));
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
                let name = *name;
                let fields_checked = fields
                    .iter()
                    .map(|(_, n, v)| Ok((*n, self.elaborate_expression(v)?)))
                    .collect::<Result<Vec<_>>>()?;

                let Some(struct_info) = self.structs.get(name).cloned() else {
                    return Err(Box::new(TypeCheckError::UnknownStruct(
                        *ident_span,
                        name.to_owned(),
                    )));
                };

                let mut seen_fields = HashMap::new();

                for ((field, expr), span) in
                    fields_checked.iter().zip(fields.iter().map(|(s, _, _)| s))
                {
                    if let Some(other_span) = seen_fields.get(field) {
                        return Err(Box::new(
                            TypeCheckError::DuplicateFieldInStructInstantiation(
                                *span,
                                *other_span,
                                field.to_string(),
                            ),
                        ));
                    }

                    let Some((expected_field_span, expected_type)) = struct_info.fields.get(*field)
                    else {
                        return Err(Box::new(TypeCheckError::UnknownFieldInStructInstantiation(
                            *span,
                            struct_info.definition_span,
                            field.to_string(),
                            struct_info.name.to_string(),
                        )));
                    };

                    let found_type = &expr.get_type();
                    if self
                        .add_equal_constraint(found_type, expected_type)
                        .is_trivially_violated()
                    {
                        return Err(Box::new(TypeCheckError::IncompatibleTypesForStructField(
                            *expected_field_span,
                            expected_type.clone(),
                            expr.full_span(),
                            found_type.clone(),
                        )));
                    }

                    seen_fields.insert(field, *span);
                }

                let missing_fields = {
                    let mut fields = struct_info.fields.clone();
                    fields.retain(|f, _| !seen_fields.contains_key(&f.as_str()));
                    fields.into_iter().map(|(n, (_, t))| (n, t)).collect_vec()
                };

                if !missing_fields.is_empty() {
                    return Err(Box::new(
                        TypeCheckError::MissingFieldsInStructInstantiation(
                            *full_span,
                            struct_info.definition_span,
                            missing_fields,
                        ),
                    ));
                }

                typed_ast::Expression::InstantiateStruct(
                    *full_span,
                    fields_checked,
                    struct_info.clone(),
                )
            }
            ast::Expression::AccessField(full_span, ident_span, expr, field_name) => {
                let field_name = *field_name;
                let expr_checked = self.elaborate_expression(expr)?;

                let type_ = expr_checked.get_type();

                let field_type = if type_.is_closed() {
                    let Type::Struct(ref struct_info) = type_ else {
                        return Err(Box::new(TypeCheckError::FieldAccessOfNonStructType(
                            *ident_span,
                            expr.full_span(),
                            field_name.to_string(),
                            type_.clone(),
                        )));
                    };

                    let Some((_, field_type)) = struct_info.fields.get(field_name) else {
                        return Err(Box::new(TypeCheckError::UnknownFieldAccess(
                            *ident_span,
                            expr.full_span(),
                            field_name.to_string(),
                            type_.clone(),
                        )));
                    };

                    field_type.clone()
                } else {
                    let field_type = self.fresh_type_variable();

                    self.constraints
                        .add(Constraint::HasField(
                            type_.clone(),
                            field_name.to_compact_string(),
                            field_type.clone(),
                        ))
                        .ok();

                    field_type
                };

                Expression::AccessField(
                    *ident_span,
                    *full_span,
                    Box::new(expr_checked),
                    field_name,
                    TypeScheme::concrete(type_),
                    TypeScheme::concrete(field_type),
                )
            }
            ast::Expression::List(span, elements) => {
                let elements_checked = elements
                    .iter()
                    .map(|e| self.elaborate_expression(e))
                    .collect::<Result<Vec<_>>>()?;

                let element_types: Vec<Type> =
                    elements_checked.iter().map(|e| e.get_type()).collect();

                let result_element_type = if element_types.is_empty() {
                    self.fresh_type_variable()
                } else if element_types[0].is_closed() {
                    element_types[0].clone()
                } else {
                    let type_ = self.fresh_type_variable();
                    self.add_equal_constraint(&element_types[0], &type_).ok();
                    type_
                };

                if !element_types.is_empty() {
                    for (subsequent_element, type_of_subsequent_element) in
                        elements_checked.iter().zip(element_types.iter()).skip(1)
                    {
                        if self
                            .add_equal_constraint(&result_element_type, type_of_subsequent_element)
                            .is_trivially_violated()
                        {
                            return Err(Box::new(TypeCheckError::IncompatibleTypesInList(
                                elements_checked[0].full_span(),
                                result_element_type.clone(),
                                subsequent_element.full_span(),
                                type_of_subsequent_element.clone(),
                            )));
                        }
                    }
                }

                typed_ast::Expression::List(
                    *span,
                    elements_checked,
                    TypeScheme::concrete(result_element_type),
                )
            }
            ast::Expression::TypedHole(span) => {
                let type_ = self.fresh_type_variable();
                typed_ast::Expression::TypedHole(*span, TypeScheme::concrete(type_))
            }
        })
    }

    fn _elaborate_inner<'a>(
        &mut self,
        definition: ElaborationDefinitionArgs<'a, '_>,
    ) -> Result<(typed_ast::Expression<'a>, typed_ast::Type)> {
        let ElaborationDefinitionArgs {
            identifier_span,
            expr,
            type_annotation_span,
            type_annotation,
            operation,
            expected_name,
            actual_name,
            actual_name_for_fix,
            elaboration_kind,
        } = definition;

        let expr_checked = self.elaborate_expression(expr)?;
        let type_deduced = expr_checked.get_type();

        if let Some(type_annotation) = type_annotation {
            let type_annotated = self.type_from_annotation(type_annotation)?;

            let type_annotation_span =
                type_annotation_span.unwrap_or_else(|| type_annotation.full_span());

            match (&type_deduced, &type_annotated) {
                (Type::Dimension(dexpr_deduced), Type::Dimension(dexpr_specified))
                    if type_deduced.is_closed() && type_annotated.is_closed() =>
                {
                    if dexpr_deduced != dexpr_specified {
                        return Err(Box::new(TypeCheckError::IncompatibleDimensions(
                            IncompatibleDimensionsError {
                                span_operation: identifier_span,
                                operation: operation.into(),
                                span_expected: type_annotation_span,
                                expected_name,
                                expected_dimensions: self.registry.get_derived_entry_names_for(
                                    &dexpr_specified.to_base_representation(),
                                ),
                                expected_type: dexpr_specified.to_base_representation(),
                                span_actual: expr.full_span(),
                                actual_name,
                                actual_name_for_fix,
                                actual_dimensions: self.registry.get_derived_entry_names_for(
                                    &dexpr_deduced.to_base_representation(),
                                ),
                                actual_type: dexpr_deduced.to_base_representation(),
                            },
                        )));
                    }
                }
                (deduced, annotated) => {
                    if self
                        .add_equal_constraint(deduced, annotated)
                        .is_trivially_violated()
                    {
                        return Err(Box::new(TypeCheckError::IncompatibleTypesInAnnotation(
                            elaboration_kind.into(),
                            identifier_span,
                            annotated.clone(),
                            type_annotation.full_span(),
                            deduced.clone(),
                            expr_checked.full_span(),
                        )));
                    }
                }
            }
        }

        Ok((expr_checked, type_deduced))
    }

    fn elaborate_define_variable<'a>(
        &mut self,
        define_variable: &ast::DefineVariable<'a>,
    ) -> Result<typed_ast::DefineVariable<'a>> {
        let DefineVariable {
            identifier_span,
            identifier,
            expr,
            type_annotation,
            decorators,
        } = define_variable;

        let (expr_checked, type_deduced) = self._elaborate_inner(ElaborationDefinitionArgs {
            identifier_span: *identifier_span,
            expr,
            type_annotation_span: None,
            type_annotation: type_annotation.as_ref(),
            operation: "variable definition",
            expected_name: "specified dimension",
            actual_name: "   actual dimension",
            actual_name_for_fix: "right hand side expression",
            elaboration_kind: "definition",
        })?;

        for (name, _) in decorator::name_and_aliases(identifier, decorators) {
            self.env.add(
                name.to_compact_string(),
                type_deduced.clone(),
                *identifier_span,
                false,
            );

            self.value_namespace
                .add_identifier_allow_override(
                    name.to_compact_string(),
                    *identifier_span,
                    CompactString::const_new("constant"),
                )
                .map_err(|err| Box::new(err.into()))?;
        }

        Ok(typed_ast::DefineVariable(
            identifier,
            decorators.clone(),
            expr_checked,
            type_annotation.clone(),
            TypeScheme::concrete(type_deduced),
            crate::markup::empty(),
        ))
    }

    fn elaborate_statement<'a>(
        &mut self,
        ast: &ast::Statement<'a>,
    ) -> Result<typed_ast::Statement<'a>> {
        Ok(match ast {
            ast::Statement::Expression(expr) => {
                let checked_expr = self.elaborate_expression(expr)?;
                for &identifier in LAST_RESULT_IDENTIFIERS {
                    self.env.add_predefined(
                        identifier.into(),
                        TypeScheme::concrete(checked_expr.get_type()),
                    );
                }
                typed_ast::Statement::Expression(checked_expr)
            }
            ast::Statement::DefineVariable(define_variable) => {
                typed_ast::Statement::DefineVariable(
                    self.elaborate_define_variable(define_variable)?,
                )
            }
            ast::Statement::DefineBaseUnit(span, unit_name, type_annotation, decorators) => {
                let type_specified = if let Some(dexpr) = type_annotation {
                    let dtype: DType = self
                        .registry
                        .get_base_representation(dexpr)
                        .map_err(TypeCheckError::RegistryError)?
                        .into();

                    if dtype.is_scalar() {
                        return Err(Box::new(TypeCheckError::NoDimensionlessBaseUnit(
                            *span,
                            unit_name.to_string(),
                        )));
                    }

                    dtype
                } else {
                    use heck::ToUpperCamelCase;
                    // In a unit definition like 'unit pixel' without a specified type,
                    // we add a new type for the user
                    let type_name = unit_name.to_upper_camel_case();
                    self.registry
                        .add_base_dimension(&type_name)
                        .map_err(TypeCheckError::RegistryError)?
                        .into()
                };
                for (name, _) in decorator::name_and_aliases(unit_name, decorators) {
                    self.env.add(
                        name.to_compact_string(),
                        Type::Dimension(type_specified.clone()),
                        *span,
                        true,
                    );
                }

                typed_ast::Statement::DefineBaseUnit(
                    unit_name,
                    decorators.clone(),
                    type_annotation.clone().map(TypeAnnotation::TypeExpression),
                    TypeScheme::concrete(Type::Dimension(type_specified)),
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
                let (expr_checked, type_deduced) =
                    self._elaborate_inner(ElaborationDefinitionArgs {
                        identifier_span: *identifier_span,
                        expr,
                        type_annotation_span: type_annotation_span.as_ref().copied(),
                        type_annotation: type_annotation.as_ref(),
                        operation: "unit definition",
                        expected_name: "specified dimension",
                        actual_name: "   actual dimension",
                        actual_name_for_fix: "right hand side expression",
                        elaboration_kind: "unit definition",
                    })?;

                for (name, _) in decorator::name_and_aliases(identifier, decorators) {
                    self.env.add(
                        name.to_compact_string(),
                        type_deduced.clone(),
                        *identifier_span,
                        true,
                    );
                }
                typed_ast::Statement::DefineDerivedUnit(
                    identifier,
                    expr_checked,
                    decorators.clone(),
                    type_annotation.clone(),
                    TypeScheme::Concrete(type_deduced),
                    crate::markup::empty(),
                )
            }
            ast::Statement::DefineFunction {
                function_name_span,
                function_name,
                type_parameters,
                parameters,
                body,
                local_variables,
                return_type_annotation,
                decorators,
            } => {
                if body.is_none() {
                    self.value_namespace
                        .add_identifier(
                            function_name.to_compact_string(),
                            *function_name_span,
                            CompactString::const_new("foreign function"),
                        )
                        .map_err(|err| Box::new(err.into()))?;
                } else {
                    self.value_namespace
                        .add_identifier_allow_override(
                            function_name.to_compact_string(),
                            *function_name_span,
                            CompactString::const_new("function"),
                        )
                        .map_err(|err| Box::new(err.into()))?;
                }

                // Save the environment and namespaces to avoid polluting
                // their parents with the locals of this function
                self.env.save();
                self.type_namespace.save();
                self.value_namespace.save();

                let is_ffi_function = body.is_none();

                for (span, type_parameter, bound) in type_parameters {
                    if self.type_namespace.has_identifier(type_parameter) {
                        return Err(Box::new(TypeCheckError::TypeParameterNameClash(
                            *span,
                            type_parameter.to_string(),
                        )));
                    }

                    self.type_namespace
                        .add_identifier(
                            type_parameter.to_compact_string(),
                            *span,
                            CompactString::const_new("type parameter"),
                        )
                        .ok(); // TODO: is this call even correct?

                    self.registry.introduced_type_parameters.push((
                        *span,
                        type_parameter.to_compact_string(),
                        bound.clone(),
                    ));

                    match bound {
                        Some(TypeParameterBound::Dim) => {
                            self.add_dtype_constraint(&Type::TPar(
                                type_parameter.to_compact_string(),
                            ))
                            .ok();
                        }
                        None => {}
                    }
                }

                let mut typed_parameters = vec![];
                for (parameter_span, parameter, type_annotation) in parameters {
                    let annotated_type = type_annotation
                        .as_ref()
                        .map(|a| self.type_from_annotation(a))
                        .transpose()?;

                    let parameter_type = match &annotated_type {
                        Some(annotated_type) => annotated_type.clone(),
                        None => self.fresh_type_variable(),
                    };

                    if is_ffi_function && annotated_type.is_none() {
                        return Err(Box::new(
                            TypeCheckError::ForeignFunctionNeedsTypeAnnotations(
                                *parameter_span,
                                parameter.to_string(),
                            ),
                        ));
                    }

                    self.env.add_scheme(
                        parameter.to_compact_string(),
                        TypeScheme::make_quantified(parameter_type.clone()),
                        *parameter_span,
                        false,
                    );
                    typed_parameters.push((
                        *parameter_span,
                        *parameter,
                        parameter_type,
                        type_annotation,
                    ));
                }

                let annotated_return_type = return_type_annotation
                    .as_ref()
                    .map(|annotation| self.type_from_annotation(annotation))
                    .transpose()?;

                let return_type = match &annotated_return_type {
                    Some(annotated_return_type) => annotated_return_type.clone(),
                    None => self.fresh_type_variable(),
                };

                // Add the function to the environment, so it can be called recursively

                let parameters: Vec<_> = typed_parameters
                    .iter()
                    .map(|(span, name, _, annotation)| (*span, name, (*annotation).clone()))
                    .collect();
                let parameter_types = typed_parameters
                    .iter()
                    .map(|(_, _, type_, _)| type_.clone())
                    .collect();

                let fn_type =
                    TypeScheme::Concrete(Type::Fn(parameter_types, Box::new(return_type.clone())));

                self.env.add_function(
                    function_name.to_compact_string(),
                    FunctionSignature {
                        name: function_name.to_compact_string(),
                        definition_span: *function_name_span,
                        type_parameters: type_parameters
                            .iter()
                            .map(|(span, name, tpb)| {
                                (*span, name.to_compact_string(), tpb.clone()).clone()
                            })
                            .collect(),
                        parameters: parameters
                            .into_iter()
                            .map(|(span, s, o)| (span, s.to_compact_string(), o))
                            .collect(),
                        return_type_annotation: return_type_annotation.clone(),
                        fn_type: fn_type.clone(),
                    },
                    FunctionMetadata {
                        name: crate::decorator::name(decorators).map(CompactString::from),
                        url: crate::decorator::url(decorators).map(CompactString::from),
                        description: crate::decorator::description(decorators),
                        examples: crate::decorator::examples(decorators),
                    },
                );

                let mut typed_local_variables = vec![];
                for local_variable in local_variables {
                    typed_local_variables.push(self.elaborate_define_variable(local_variable)?);
                }

                let body_checked = body
                    .as_ref()
                    .map(|expr| self.elaborate_expression(expr))
                    .transpose()?;

                let return_type_inferred = if let Some(ref expr) = body_checked {
                    let return_type_inferred = expr.get_type();

                    if self
                        .add_equal_constraint(&return_type_inferred, &return_type)
                        .is_trivially_violated()
                    {
                        if let Some(annotated_return_type) = annotated_return_type {
                            match (&return_type_inferred, annotated_return_type) {
                                (
                                    Type::Dimension(dtype_deduced),
                                    Type::Dimension(dtype_specified),
                                ) => {
                                    return Err(Box::new(TypeCheckError::IncompatibleDimensions(
                                        IncompatibleDimensionsError {
                                            span_operation: *function_name_span,
                                            operation: "function return type".into(),
                                            span_expected: return_type_annotation
                                                .as_ref()
                                                .unwrap()
                                                .full_span(),
                                            expected_name: "specified return type",
                                            expected_dimensions: self
                                                .registry
                                                .get_derived_entry_names_for(
                                                    &dtype_specified.to_base_representation(),
                                                ),
                                            expected_type: dtype_specified.to_base_representation(),
                                            span_actual: body
                                                .as_ref()
                                                .map(|b| b.full_span())
                                                .unwrap(),
                                            actual_name: "   actual return type",
                                            actual_name_for_fix: "expression in the function body",
                                            actual_dimensions: self
                                                .registry
                                                .get_derived_entry_names_for(
                                                    &dtype_deduced.to_base_representation(),
                                                ),
                                            actual_type: dtype_deduced.to_base_representation(),
                                        },
                                    )));
                                }
                                (return_type_inferred, type_specified) => {
                                    return Err(Box::new(
                                        TypeCheckError::IncompatibleTypesInAnnotation(
                                            "function definition".into(),
                                            *function_name_span,
                                            type_specified,
                                            return_type_annotation.as_ref().unwrap().full_span(),
                                            return_type_inferred.clone(),
                                            body.as_ref().map(|b| b.full_span()).unwrap(),
                                        ),
                                    ));
                                }
                            }
                        }
                    }
                    return_type_inferred
                } else {
                    if !ffi::functions().contains_key(*function_name) {
                        return Err(Box::new(TypeCheckError::UnknownForeignFunction(
                            *function_name_span,
                            function_name.to_string(),
                        )));
                    }

                    annotated_return_type.ok_or_else(|| {
                        TypeCheckError::ForeignFunctionNeedsTypeAnnotations(
                            *function_name_span,
                            function_name.to_string(),
                        )
                    })?
                };

                self.add_equal_constraint(&return_type_inferred, &return_type)
                    .ok();

                // Copy identifier for the new function into local env:
                let (signature, metadata) = self.env.get_function_info(function_name).unwrap();
                let signature = signature.clone();
                let metadata = metadata.clone();

                // Restore the environment and namespaces before exiting and
                // add the function name to the environment
                self.value_namespace.restore();
                self.type_namespace.restore();
                self.env.restore();
                self.env.add_function(
                    function_name.to_compact_string(),
                    signature.clone(),
                    metadata.clone(),
                );

                typed_ast::Statement::DefineFunction(
                    function_name,
                    decorators.clone(),
                    type_parameters
                        .iter()
                        .map(|(_, name, bound)| (*name, bound.clone()))
                        .collect(),
                    typed_parameters
                        .iter()
                        .map(|(span, name, _, type_annotation)| {
                            (
                                *span,
                                *name,
                                (*type_annotation).clone(),
                                crate::markup::empty(),
                            )
                        })
                        .collect(),
                    body_checked,
                    typed_local_variables,
                    fn_type,
                    return_type_annotation.clone(),
                    crate::markup::empty(),
                )
            }
            ast::Statement::DefineDimension(name_span, name, dexprs) => {
                self.type_namespace
                    .add_identifier(
                        name.to_compact_string(),
                        *name_span,
                        CompactString::const_new("dimension"),
                    )
                    .map_err(|err| Box::new(err.into()))?;

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
                            return Err(Box::new(
                                TypeCheckError::IncompatibleAlternativeDimensionExpression(
                                    name.to_string(),
                                    dexpr.full_span(),
                                    base_representation,
                                    alternative_expr.full_span(),
                                    alternative_base_representation,
                                ),
                            ));
                        }
                    }
                } else {
                    self.registry
                        .add_base_dimension(name)
                        .map_err(TypeCheckError::RegistryError)?;
                }
                typed_ast::Statement::DefineDimension(name, dexprs.clone())
            }
            ast::Statement::ProcedureCall(span, kind @ ProcedureKind::Type, args) => {
                if args.len() != 1 {
                    return Err(Box::new(TypeCheckError::WrongArity {
                        callable_span: *span,
                        callable_name: "type".into(),
                        callable_definition_span: None,
                        arity: 1..=1,
                        num_args: args.len(),
                    }));
                }

                let checked_args = args
                    .iter()
                    .map(|e| self.elaborate_expression(e))
                    .collect::<Result<Vec<_>>>()?;

                typed_ast::Statement::ProcedureCall(kind.clone(), checked_args)
            }
            ast::Statement::ProcedureCall(span, kind, args) => {
                let procedure = ffi::procedures().get(kind).unwrap();
                if !procedure.arity.contains(&args.len()) {
                    return Err(Box::new(TypeCheckError::WrongArity {
                        callable_span: *span,
                        callable_name: procedure.name.to_owned(),
                        callable_definition_span: None,
                        arity: procedure.arity.clone(),
                        num_args: args.len(),
                    }));
                }

                let checked_args = args
                    .iter()
                    .map(|e| self.elaborate_expression(e))
                    .collect::<Result<Vec<_>>>()?;

                match kind {
                    ProcedureKind::Print => {
                        // no argument type checks required, everything can be printed
                    }
                    ProcedureKind::Assert => {
                        if self
                            .add_equal_constraint(&checked_args[0].get_type(), &Type::Boolean)
                            .is_trivially_violated()
                        {
                            return Err(Box::new(TypeCheckError::IncompatibleTypeInAssert(
                                *span,
                                checked_args[0].get_type(),
                                checked_args[0].full_span(),
                            )));
                        }
                    }
                    ProcedureKind::AssertEq => {
                        // The three-argument version of assert_eq requires dtypes as inputs:
                        let needs_dtypes = checked_args.len() == 3;

                        let type_first = &checked_args[0].get_type();
                        if needs_dtypes {
                            self.enforce_dtype(type_first, checked_args[0].full_span())?;
                        }

                        for arg in &checked_args[1..] {
                            let type_arg = arg.get_type();
                            if needs_dtypes {
                                self.enforce_dtype(&type_arg, arg.full_span())?;
                            }

                            if self
                                .add_equal_constraint(type_first, &type_arg)
                                .is_trivially_violated()
                            {
                                return Err(Box::new(TypeCheckError::IncompatibleTypesInAssertEq(
                                    *span,
                                    checked_args[0].get_type(),
                                    checked_args[0].full_span(),
                                    arg.get_type(),
                                    arg.full_span(),
                                )));
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
                self.type_namespace
                    .add_identifier(
                        struct_name.to_compact_string(),
                        *struct_name_span,
                        CompactString::const_new("struct"),
                    )
                    .map_err(|err| Box::new(err.into()))?;

                let mut seen_fields = HashMap::new();

                for (span, field, _) in fields {
                    if let Some(other_span) = seen_fields.get(field) {
                        return Err(Box::new(TypeCheckError::DuplicateFieldInStructDefinition(
                            *span,
                            *other_span,
                            field.to_string(),
                        )));
                    }

                    seen_fields.insert(field, *span);
                }

                let struct_info = StructInfo {
                    definition_span: *struct_name_span,
                    name: struct_name.to_compact_string(),
                    fields: fields
                        .iter()
                        .map(|(span, name, type_)| {
                            Ok((
                                name.to_compact_string(),
                                (*span, self.type_from_annotation(type_)?),
                            ))
                        })
                        .collect::<Result<_>>()?,
                };
                self.structs
                    .insert(struct_name.to_compact_string(), struct_info.clone());

                typed_ast::Statement::DefineStruct(struct_info)
            }
        })
    }

    fn check_statement<'a>(
        &mut self,
        statement: &ast::Statement<'a>,
    ) -> Result<typed_ast::Statement<'a>> {
        self.constraints.clear();
        self.registry.introduced_type_parameters.clear();

        // Elaborate the program/statement: turn the AST into a typed AST, possibly
        // with unification variables, i.e. type variables that will only later be
        // filled in after the constraints have been solved.
        let mut elaborated_statement = self.elaborate_statement(statement)?;

        // Solve constraints
        let (substitution, dtype_variables) =
            self.constraints.solve().map_err(|inner| match inner {
                ConstraintSolverError::CouldNotSolve(constraints) => {
                    TypeCheckError::ConstraintSolverError(
                        constraints,
                        elaborated_statement.pretty_print().to_string(),
                    )
                }
                ConstraintSolverError::SubstitutionError(inner) => {
                    TypeCheckError::SubstitutionError(
                        elaborated_statement.pretty_print().to_string(),
                        inner,
                    )
                }
            })?;

        elaborated_statement.apply(&substitution).map_err(|e| {
            TypeCheckError::SubstitutionError(elaborated_statement.pretty_print().to_string(), e)
        })?;

        self.env.apply(&substitution).map_err(|e| {
            TypeCheckError::SubstitutionError(elaborated_statement.pretty_print().to_string(), e)
        })?;

        if let typed_ast::Statement::DefineDerivedUnit(_, expr, _, _annotation, type_, _) =
            &elaborated_statement
        {
            if !type_.unsafe_as_concrete().is_closed() {
                return Err(Box::new(
                    TypeCheckError::DerivedUnitDefinitionMustNotBeGeneric(expr.full_span()),
                ));
            }
        }

        // Make sure that the user-specified type parameter bounds are properly reflected:
        for (span, type_parameter, bound) in &self.registry.introduced_type_parameters {
            match bound {
                Some(TypeParameterBound::Dim) => {
                    // The type parameter might be over-constrained, but that's okay
                }
                None => {
                    // Make sure that the type parameter is not part of dtype_variables.
                    // Otherwise, a `Dim` bound is missing.
                    if dtype_variables.iter().any(|tv| match tv {
                        TypeVariable::Named(name) => name == type_parameter,
                        _ => false,
                    }) {
                        return Err(Box::new(TypeCheckError::MissingDimBound(*span)));
                    }
                }
            }
        }

        // For all dimension type variables that are still free, check all of their occurences
        // within type_, and then multiply the corresponding exponents with the least common
        // multiple of the denominators of the exponents. For example, this will turn
        // T0^(1/3) -> T0^(1/5) -> T0 into T0^5 -> T0^3 -> T0^15.
        for tv in &dtype_variables {
            let exponents = elaborated_statement.exponents_for(tv);
            let lcm = exponents
                .iter()
                .fold(1, |acc, e| num_integer::lcm(acc, *e.denom()));

            if lcm != 1 {
                let s = Substitution::single(
                    tv.clone(),
                    Type::Dimension(
                        DType::from_type_variable(tv.clone()).power(Exponent::from_integer(lcm)),
                    ),
                );

                elaborated_statement.apply(&s).unwrap();
            }
        }

        elaborated_statement.generalize_types(&dtype_variables);

        elaborated_statement.update_readable_types(&self.registry);

        self.env.generalize_types(&dtype_variables);

        // Check if there is a typed hole in the statement
        if let Some((span, type_of_hole)) = elaborated_statement.find_typed_hole()? {
            return Err(Box::new(TypeCheckError::TypedHoleInStatement(
                span,
                type_of_hole
                    .to_readable_type(&self.registry, true)
                    .to_string(),
                elaborated_statement.pretty_print().to_string(),
                self.env
                    .iter_relevant_matches()
                    .filter(|(_, t)| t == &type_of_hole)
                    .take(10)
                    .map(|(n, _)| n.to_string())
                    .collect(),
            )));
        }

        Ok(elaborated_statement)
    }

    pub fn check<'a>(
        &mut self,
        statements: &[ast::Statement<'a>],
    ) -> Result<Vec<typed_ast::Statement<'a>>> {
        let mut checked_statements = vec![];

        for statement in statements {
            checked_statements.push(self.check_statement(statement)?);
        }

        Ok(checked_statements)
    }

    pub(crate) fn registry(&self) -> &DimensionRegistry {
        &self.registry
    }

    pub fn lookup_function(&self, name: &str) -> Option<(&FunctionSignature, &FunctionMetadata)> {
        self.env.get_function_info(name)
    }
}
