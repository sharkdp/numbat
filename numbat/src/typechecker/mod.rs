#[cfg(test)]
mod tests;

mod const_evaluation;
mod constraints;
mod environment;
mod error;
mod incompatible_dimensions;
mod name_generator;
mod qualified_type;
mod substitutions;
pub mod type_scheme;

use std::collections::HashMap;
use std::ops::Deref;

use crate::ast::{self, BinaryOperator, ProcedureKind, StringPart, TypeAnnotation, TypeExpression};
use crate::dimension::DimensionRegistry;
use crate::name_resolution::Namespace;
use crate::name_resolution::LAST_RESULT_IDENTIFIERS;
use crate::pretty_print::PrettyPrint;
use crate::registry::RegistryError;
use crate::span::Span;
use crate::type_variable::TypeVariable;
use crate::typed_ast::{self, DType, DTypeFactor, Expression, StructInfo, Type};
use crate::{decorator, ffi, suggestion};

use const_evaluation::evaluate_const_expr;
use constraints::{Constraint, ConstraintSet, TrivialResultion};
use environment::{Environment, FunctionMetadata, FunctionSignature};
use itertools::Itertools;
use log::info;
use name_generator::NameGenerator;
use num_traits::Zero;

pub use error::{Result, TypeCheckError};
pub use incompatible_dimensions::IncompatibleDimensionsError;
use qualified_type::{Bound, QualifiedType};
use substitutions::ApplySubstitution;
use type_scheme::TypeScheme;

fn dtype(e: &Expression) -> Result<DType> {
    // TODO: This function should probably be removed. But we can think about adding something similar that adds a DType constraint and checks a trivial violation
    match e.get_type() {
        Type::Dimension(dtype) => Ok(dtype),
        t => Err(TypeCheckError::ExpectedDimensionType(e.full_span(), t)),
    }
}

#[derive(Clone, Default)]
pub struct TypeChecker {
    structs: HashMap<String, StructInfo>,
    registry: DimensionRegistry,

    type_namespace: Namespace,
    value_namespace: Namespace,

    env: Environment,
    name_generator: NameGenerator,
    constraints: ConstraintSet,
}

impl TypeChecker {
    fn fresh_type_variable(&mut self) -> Type {
        Type::TVar(self.name_generator.fresh_type_variable())
    }

    fn add_equal_constraint(&mut self, lhs: &Type, rhs: &Type) -> TrivialResultion {
        self.constraints
            .add(Constraint::Equal(lhs.clone(), rhs.clone()))
    }

    fn add_dtype_constraint(&mut self, type_: &Type) -> TrivialResultion {
        self.constraints.add(Constraint::IsDType(type_.clone()))
    }

    fn type_from_annotation(&self, annotation: &TypeAnnotation) -> Result<Type> {
        match annotation {
            TypeAnnotation::TypeExpression(dexpr) => {
                if let TypeExpression::TypeIdentifier(_, name) = dexpr {
                    if let Some(info) = self.structs.get(name) {
                        // if we see a struct name here, it's safe to assume it
                        // isn't accidentally clashing with a dimension, we
                        // check that earlier.
                        return Ok(Type::Struct(info.clone()));
                    }
                }

                let mut dtype: DType = self
                    .registry
                    .get_base_representation(dexpr)
                    .map(|br| br.into())
                    .map_err(TypeCheckError::RegistryError)?;

                // Replace BaseDimension("D") with TVar("D") for all type parameters
                for (factor, _) in dtype.factors.iter_mut() {
                    *factor = match factor {
                        DTypeFactor::BaseDimension(ref n)
                            if self.registry.introduced_type_parameters.contains(n) =>
                        {
                            DTypeFactor::TVar(TypeVariable::new(n))
                        }
                        ref f => f.deref().clone(),
                    }
                }

                Ok(Type::Dimension(dtype))
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
        self.env.get_identifier_type(name).ok_or_else(|| {
            let suggestion = suggestion::did_you_mean(
                self.env
                    .iter_identifiers()
                    .map(|k| k.to_string())
                    .chain(["true".into(), "false".into()]) // These are parsed as keywords, but can act like identifiers
                    .chain(ffi::procedures().values().map(|p| p.name.clone())),
                name,
            );
            TypeCheckError::UnknownIdentifier(span, name.into(), suggestion)
        })
    }

    fn get_proper_function_reference(
        &self,
        expr: &ast::Expression,
    ) -> Option<(String, &FunctionSignature)> {
        match expr {
            ast::Expression::Identifier(_, name) => self
                .env
                .get_function_info(name)
                .map(|(signature, _)| (name.clone(), signature)),
            _ => None,
        }
    }

    fn proper_function_call(
        &mut self,
        span: &Span,
        full_span: &Span,
        function_name: &str,
        signature: &FunctionSignature,
        arguments: Vec<typed_ast::Expression>,
        argument_types: Vec<Type>,
    ) -> Result<typed_ast::Expression> {
        let FunctionSignature {
            definition_span,
            type_parameters: _,
            parameters,
            fn_type,
        } = signature;

        let fn_type = match fn_type {
            TypeScheme::Concrete(t) => {
                // TODO: we take this branch for recursive functions,
                // because then we haven't yet generalized the function type.
                // Make sure that this doesn't cause any problems.
                t.clone()
            }
            TypeScheme::Quantified(_, _) => {
                let qt = fn_type.instantiate(&mut self.name_generator);
                qt.inner
            }
        };

        let Type::Fn(parameter_types, return_type) = fn_type else {
            unreachable!("Expected function type, got {:#?}", fn_type);
        };

        // TODO: what about the bounds on qt?

        let arity_range = parameters.len()..=parameters.len();

        if !arity_range.contains(&arguments.len()) {
            return Err(TypeCheckError::WrongArity {
                callable_span: *span,
                callable_name: function_name.into(),
                callable_definition_span: Some(*definition_span),
                arity: arity_range,
                num_args: arguments.len(),
            });
        }

        for (idx, ((parameter_span, parameter_type), argument_type)) in parameters
            .iter()
            .map(|p| p.0)
            .zip(parameter_types.iter())
            .zip(argument_types)
            .enumerate()
        {
            if self
                .add_equal_constraint(parameter_type, &argument_type)
                .is_trivially_violated()
            {
                match (parameter_type, &argument_type) {
                    (Type::Dimension(parameter_dtype), Type::Dimension(argument_dtype)) => {
                        return Err(TypeCheckError::IncompatibleDimensions(
                            IncompatibleDimensionsError {
                                span_operation: *span,
                                operation: format!(
                                    "argument {num} of function call to '{name}'",
                                    num = idx + 1,
                                    name = function_name
                                ),
                                span_expected: parameter_span, // TODO: is this correct?
                                expected_name: "parameter type",
                                expected_dimensions: self.registry.get_derived_entry_names_for(
                                    &parameter_dtype.to_base_representation(),
                                ),
                                expected_type: parameter_dtype.to_base_representation(),
                                span_actual: arguments[idx].full_span(),
                                actual_name: " argument type",
                                actual_name_for_fix: "function argument",
                                actual_dimensions: self.registry.get_derived_entry_names_for(
                                    &argument_dtype.to_base_representation(),
                                ),
                                actual_type: argument_dtype.to_base_representation(),
                            },
                        ));
                    }
                    _ => {
                        return Err(TypeCheckError::IncompatibleTypesInFunctionCall(
                            Some(parameter_span),
                            parameter_type.clone(),
                            arguments[idx].full_span(),
                            argument_type.clone(),
                        ));
                    }
                }
            }
        }

        Ok(typed_ast::Expression::FunctionCall(
            *span,
            *full_span,
            function_name.into(),
            arguments,
            TypeScheme::concrete(return_type.as_ref().clone()),
        ))
    }

    fn elaborate_expression(&mut self, ast: &ast::Expression) -> Result<typed_ast::Expression> {
        Ok(match ast {
            ast::Expression::Scalar(span, n) if n.to_f64().is_zero() => {
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

                let qt = type_scheme.instantiate(&mut self.name_generator);

                for Bound::IsDim(t) in qt.bounds.iter() {
                    self.constraints.add(Constraint::IsDType(t.clone())).ok();
                }

                typed_ast::Expression::Identifier(
                    *span,
                    name.clone(),
                    TypeScheme::concrete(qt.inner),
                )
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
                // match (&type_, op) {
                //     (Type::Dimension(dtype), ast::UnaryOperator::Factorial) => {
                //         if !dtype.is_scalar() {
                //             return Err(TypeCheckError::NonScalarFactorialArgument(
                //                 expr.full_span(),
                //                 dtype.clone(),
                //             ));
                //         }
                //     }
                //     (Type::Dimension(_), ast::UnaryOperator::Negate) => (),
                //     (Type::Boolean, ast::UnaryOperator::LogicalNeg) => (),
                //     (_, ast::UnaryOperator::LogicalNeg) => {
                //         return Err(TypeCheckError::ExpectedBool(expr.full_span()))
                //     }
                //     _ => {
                //         // return Err(TypeCheckError::ExpectedDimensionType(
                //         //     checked_expr.full_span(),
                //         //     type_.clone(),
                //         // ));
                //     }
                // };
                match op {
                    ast::UnaryOperator::Factorial => {
                        if self
                            .add_equal_constraint(&type_, &Type::scalar())
                            .is_trivially_violated()
                        {
                            return Err(TypeCheckError::NonScalarFactorialArgument(
                                expr.full_span(),
                                type_,
                            ));
                        }
                    }
                    ast::UnaryOperator::Negate => {
                        if self.add_dtype_constraint(&type_).is_trivially_violated() {
                            return Err(TypeCheckError::ExpectedDimensionType(
                                expr.full_span(),
                                type_,
                            ));
                        }
                    }
                    ast::UnaryOperator::LogicalNeg => {
                        if self
                            .add_equal_constraint(&type_, &Type::Boolean)
                            .is_trivially_violated()
                        {
                            return Err(TypeCheckError::ExpectedBool(expr.full_span()));
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
                        return Err(TypeCheckError::WrongArity {
                            callable_span: rhs.full_span(),
                            callable_name: "function".into(),
                            callable_definition_span: None,
                            arity: 1..=1,
                            num_args: parameter_types.len(),
                        });
                    }

                    if self
                        .add_equal_constraint(&lhs_type, &parameter_types[0])
                        .is_trivially_violated()
                    {
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
                    let mut get_type_and_assert_equality = || -> Result<Type> {
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
                            return Err(TypeCheckError::IncompatibleDimensions(
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
                            ));
                        }

                        Ok(lhs_type)
                    };

                    let type_ = match op {
                        typed_ast::BinaryOperator::Add => get_type_and_assert_equality()?,
                        typed_ast::BinaryOperator::Sub => get_type_and_assert_equality()?,
                        typed_ast::BinaryOperator::Mul | typed_ast::BinaryOperator::Div => {
                            let type_lhs = lhs_checked.get_type();
                            let type_rhs = rhs_checked.get_type();

                            self.add_dtype_constraint(&type_lhs).ok(); // TODO: here we can fail immediately, if this constraint is trivially violated
                            self.add_dtype_constraint(&type_rhs).ok();

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
                        typed_ast::BinaryOperator::Power => {
                            // let exponent_type = dtype(&rhs_checked)?;
                            // if !exponent_type.is_scalar() {
                            //     return Err(TypeCheckError::NonScalarExponent(
                            //         rhs.full_span(),
                            //         Type::Dimension(exponent_type), // TODO
                            //     ));
                            // }

                            // let base_type = dtype(&lhs_checked)?;
                            // if base_type.is_scalar() {
                            //     // Skip evaluating the exponent if the lhs is a scalar. This allows
                            //     // for arbitrary (decimal) exponents, if the base is a scalar.

                            //     Type::Dimension(base_type)
                            // } else {
                            //     let exponent = evaluate_const_expr(&rhs_checked)?;
                            //     Type::Dimension(base_type.power(exponent))
                            // }

                            let type_base_inferred = lhs_type;
                            let type_exponent_inferred = rhs_type;

                            if self
                                .add_dtype_constraint(&type_base_inferred)
                                .is_trivially_violated()
                            {
                                return Err(TypeCheckError::ExpectedDimensionType(
                                    lhs.full_span(),
                                    type_base_inferred,
                                ));
                            }
                            if self
                                .add_dtype_constraint(&type_exponent_inferred)
                                .is_trivially_violated()
                            {
                                return Err(TypeCheckError::ExpectedDimensionType(
                                    rhs.full_span(),
                                    type_exponent_inferred,
                                ));
                            }

                            match type_base_inferred {
                                Type::Dimension(base_dtype) if base_dtype.is_scalar() => {
                                    // Skip evaluating the exponent if the lhs is a scalar. This allows
                                    // for arbitrary (decimal) exponents, if the base is a scalar.

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
                                        todo!("This case needs a type annotation?")
                                    }
                                }
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
                                // return Err(TypeCheckError::IncompatibleTypesInComparison(
                                //     span_op.unwrap(),
                                //     lhs_type,
                                //     lhs.full_span(),
                                //     rhs_type,
                                //     rhs.full_span(),
                                // ));
                            }

                            Type::Boolean
                        }
                        typed_ast::BinaryOperator::LogicalAnd
                        | typed_ast::BinaryOperator::LogicalOr => {
                            if self
                                .add_equal_constraint(&lhs_type, &Type::Boolean)
                                .is_trivially_violated()
                            {
                                return Err(TypeCheckError::ExpectedBool(lhs.full_span()));
                            }
                            if self
                                .add_equal_constraint(&rhs_type, &Type::Boolean)
                                .is_trivially_violated()
                            {
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

                if let Some((name, signature)) = self.get_proper_function_reference(callable) {
                    let name = name.clone(); // TODO: this is just a hack for now to get around a borrowing issue. not fixed properly since this will probably be removed anyways
                    let signature = signature.clone(); // TODO: same
                    self.proper_function_call(
                        span,
                        full_span,
                        &name,
                        &signature,
                        arguments_checked,
                        argument_types,
                    )?
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
                            &Type::Fn(parameter_types, Box::new(return_type.clone())),
                        )
                        .is_trivially_violated()
                    {
                        return Err(TypeCheckError::OnlyFunctionsAndReferencesCanBeCalled(
                            callable.full_span(),
                        ));
                    }

                    typed_ast::Expression::CallableCall(
                        *full_span,
                        Box::new(callable_checked),
                        arguments_checked,
                        TypeScheme::concrete(return_type),
                    )

                    // match callable_type {
                    //     Type::Fn(parameters_types, return_type) => {
                    //         let num_parameters = parameters_types.len();
                    //         let num_arguments = arguments_checked.len();

                    //         if num_parameters != num_arguments {
                    //             return Err(TypeCheckError::WrongArity {
                    //                 callable_span: *span,
                    //                 callable_name: "function".into(),
                    //                 callable_definition_span: None,
                    //                 arity: num_parameters..=num_parameters,
                    //                 num_args: num_arguments,
                    //             });
                    //         }

                    //         for (param_type, arg_checked) in
                    //             parameters_types.iter().zip(&arguments_checked)
                    //         {
                    //             if &arg_checked.get_type() != param_type {
                    //                 // return Err(TypeCheckError::IncompatibleTypesInFunctionCall(
                    //                 //     None,
                    //                 //     param_type.clone(),
                    //                 //     arg_checked.full_span(),
                    //                 //     arg_checked.get_type(),
                    //                 // ));
                    //             }
                    //         }

                    //         typed_ast::Expression::CallableCall(
                    //             *full_span,
                    //             Box::new(callable_checked),
                    //             arguments_checked,
                    //             TypeScheme::concrete(*return_type),
                    //         )
                    //     }
                    //     _ => {
                    //         return Err(TypeCheckError::OnlyFunctionsAndReferencesCanBeCalled(
                    //             callable.full_span(),
                    //         ));
                    //     }
                    // }
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
                    return Err(TypeCheckError::ExpectedBool(condition.full_span()));
                }

                let then = self.elaborate_expression(then)?;
                let else_ = self.elaborate_expression(else_)?;

                let then_type = then.get_type();
                let else_type = else_.get_type();

                if self
                    .add_equal_constraint(&then_type, &else_type)
                    .is_trivially_violated()
                {
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
                    .map(|(_, n, v)| Ok((n.to_string(), self.elaborate_expression(v)?)))
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
                    if found_type != expected_type {
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
                let expr_checked = self.elaborate_expression(expr)?;

                let type_ = expr_checked.get_type();

                let Type::Struct(struct_info) = type_.clone() else {
                    return Err(TypeCheckError::FieldAccessOfNonStructType(
                        *ident_span,
                        expr.full_span(),
                        attr.to_string(),
                        type_.clone(),
                    ));
                };

                let Some((_, result_type)) = struct_info.fields.get(attr) else {
                    return Err(TypeCheckError::UnknownFieldAccess(
                        *ident_span,
                        expr.full_span(),
                        attr.to_string(),
                        type_.clone(),
                    ));
                };

                let result_type = result_type.to_owned();

                Expression::AccessField(
                    *ident_span,
                    *full_span,
                    Box::new(expr_checked),
                    attr.to_owned(),
                    struct_info,
                    TypeScheme::concrete(result_type),
                )
            }
            ast::Expression::List(span, elements) => {
                let elements_checked = elements
                    .iter()
                    .map(|e| self.elaborate_expression(e))
                    .collect::<Result<Vec<_>>>()?;

                let element_types: Vec<Type> =
                    elements_checked.iter().map(|e| e.get_type()).collect();

                let result_element_type = self.fresh_type_variable();

                if !element_types.is_empty() {
                    let type_of_first_element = element_types[0].clone();
                    self.add_equal_constraint(&result_element_type, &type_of_first_element)
                        .ok(); // This can never be satisfied trivially, so ignore the result

                    for (subsequent_element, type_of_subsequent_element) in
                        elements_checked.iter().zip(element_types.iter()).skip(1)
                    {
                        if self
                            .add_equal_constraint(
                                &type_of_subsequent_element,
                                &type_of_first_element,
                            )
                            .is_trivially_violated()
                        {
                            return Err(TypeCheckError::IncompatibleTypesInList(
                                elements_checked[0].full_span(),
                                type_of_first_element.clone(),
                                subsequent_element.full_span(),
                                type_of_subsequent_element.clone(),
                            ));
                        }
                    }
                }

                typed_ast::Expression::List(
                    *span,
                    elements_checked,
                    TypeScheme::concrete(result_element_type),
                )
            }
        })
    }

    fn elaborate_statement(&mut self, ast: &ast::Statement) -> Result<typed_ast::Statement> {
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
            ast::Statement::DefineVariable {
                identifier_span,
                identifier,
                expr,
                type_annotation,
                decorators,
            } => {
                let expr_checked = self.elaborate_expression(expr)?;
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
                                            .get_derived_entry_names_for(
                                                &dexpr_specified.to_base_representation(),
                                            ),
                                        expected_type: dexpr_specified.to_base_representation(),
                                        span_actual: expr.full_span(),
                                        actual_name: "   actual dimension",
                                        actual_name_for_fix: "right hand side expression",
                                        actual_dimensions: self
                                            .registry
                                            .get_derived_entry_names_for(
                                                &dexpr_deduced.to_base_representation(),
                                            ),
                                        actual_type: dexpr_deduced.to_base_representation(),
                                    },
                                ));
                            }
                        }
                        (deduced, annotated) => {
                            if self
                                .add_equal_constraint(&deduced, &annotated)
                                .is_trivially_violated()
                            {
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
                    self.env
                        .add(name.clone(), type_deduced.clone(), *identifier_span);

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
                    TypeScheme::concrete(type_deduced),
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
                        return Err(TypeCheckError::NoDimensionlessBaseUnit(
                            *span,
                            unit_name.into(),
                        ));
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
                    self.env
                        .add(name.clone(), Type::Dimension(type_specified.clone()), *span);
                }
                typed_ast::Statement::DefineBaseUnit(
                    unit_name.clone(),
                    decorators.clone(),
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
                // TODO: this is the *exact same code* that we have above for
                // variable definitions => deduplicate this somehow
                let expr_checked = self.elaborate_expression(expr)?;
                let type_deduced = dtype(&expr_checked)?;

                if let Some(ref dexpr) = type_annotation {
                    let type_specified = self
                        .registry
                        .get_base_representation(dexpr)
                        .map_err(TypeCheckError::RegistryError)?
                        .into();
                    if type_deduced != type_specified {
                        return Err(TypeCheckError::IncompatibleDimensions(
                            IncompatibleDimensionsError {
                                span_operation: *identifier_span,
                                operation: "unit definition".into(),
                                span_expected: type_annotation_span.unwrap(),
                                expected_name: "specified dimension",
                                expected_dimensions: self.registry.get_derived_entry_names_for(
                                    &type_specified.to_base_representation(),
                                ),
                                expected_type: type_specified.to_base_representation(),
                                span_actual: expr.full_span(),
                                actual_name: "   actual dimension",
                                actual_name_for_fix: "right hand side expression",
                                actual_dimensions: self.registry.get_derived_entry_names_for(
                                    &type_deduced.to_base_representation(),
                                ),
                                actual_type: type_deduced.to_base_representation(),
                            },
                        ));
                    }
                }
                for (name, _) in decorator::name_and_aliases(identifier, decorators) {
                    self.env.add(
                        name.clone(),
                        Type::Dimension(type_deduced.clone()),
                        *identifier_span,
                    );
                }
                typed_ast::Statement::DefineDerivedUnit(
                    identifier.clone(),
                    expr_checked,
                    decorators.clone(),
                    TypeScheme::Concrete(Type::Dimension(type_deduced)),
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

                let mut typechecker_fn = self.clone(); // TODO: is this even needed?
                let is_ffi_function = body.is_none();

                for (span, type_parameter) in type_parameters {
                    if typechecker_fn.type_namespace.has_identifier(type_parameter) {
                        return Err(TypeCheckError::TypeParameterNameClash(
                            *span,
                            type_parameter.clone(),
                        ));
                    }

                    typechecker_fn
                        .type_namespace
                        .add_identifier(type_parameter.clone(), *span, "type parameter".to_owned())
                        .ok(); // TODO: is this call even correct?

                    typechecker_fn
                        .registry
                        .introduced_type_parameters
                        .push(type_parameter.clone());
                }

                let mut typed_parameters = vec![];
                for (parameter_span, parameter, type_annotation) in parameters {
                    let parameter_type = typechecker_fn.fresh_type_variable();

                    let annotated_type = type_annotation
                        .as_ref()
                        .map(|a| typechecker_fn.type_from_annotation(a))
                        .transpose()?;

                    if is_ffi_function && annotated_type.is_none() {
                        return Err(TypeCheckError::ForeignFunctionNeedsTypeAnnotations(
                            *parameter_span,
                            parameter.clone(),
                        ));
                    }

                    if let Some(annotated_type) = annotated_type {
                        typechecker_fn
                            .add_equal_constraint(&parameter_type, &annotated_type)
                            .ok();
                    }

                    typechecker_fn.env.add_scheme(
                        parameter.clone(),
                        TypeScheme::make_quantified(parameter_type.clone()),
                        *parameter_span,
                    );
                    typed_parameters.push((*parameter_span, parameter.clone(), parameter_type));
                }

                let annotated_return_type = return_type_annotation
                    .as_ref()
                    .map(|annotation| typechecker_fn.type_from_annotation(annotation))
                    .transpose()?;

                // Add the function to the environment, so it can be called recursively

                let parameters = typed_parameters
                    .iter()
                    .map(|(span, name, _)| (*span, name.clone()))
                    .collect();
                let parameter_types = typed_parameters
                    .iter()
                    .map(|(_, _, type_)| type_.clone())
                    .collect();
                let return_type = typechecker_fn.fresh_type_variable();

                info!(
                    "Adding function with name {} and parameter types {:?} and return type {:?}",
                    function_name, parameter_types, return_type
                );

                let fn_type =
                    TypeScheme::Concrete(Type::Fn(parameter_types, Box::new(return_type.clone())));

                typechecker_fn.env.add_function(
                    function_name.clone(),
                    FunctionSignature {
                        definition_span: *function_name_span,
                        type_parameters: type_parameters.clone(),
                        parameters,
                        fn_type: fn_type.clone(),
                    },
                    FunctionMetadata {
                        name: crate::decorator::name(decorators),
                        url: crate::decorator::url(decorators),
                        description: crate::decorator::description(decorators),
                    },
                );

                let body_checked = body
                    .as_ref()
                    .map(|expr| typechecker_fn.elaborate_expression(&expr))
                    .transpose()?;

                let return_type_inferred = if let Some(ref expr) = body_checked {
                    let return_type = expr.get_type();

                    if let Some(annotated_return_type) = annotated_return_type {
                        if typechecker_fn
                            .add_equal_constraint(&return_type, &annotated_return_type)
                            .is_trivially_violated()
                        {
                            match (&return_type, annotated_return_type) {
                                (
                                    Type::Dimension(dtype_deduced),
                                    Type::Dimension(dtype_specified),
                                ) => {
                                    return Err(TypeCheckError::IncompatibleDimensions(
                                        IncompatibleDimensionsError {
                                            span_operation: *function_name_span,
                                            operation: "function return type".into(),
                                            span_expected: return_type_annotation_span.unwrap(),
                                            expected_name: "specified return type",
                                            expected_dimensions: typechecker_fn
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
                                            actual_dimensions: typechecker_fn
                                                .registry
                                                .get_derived_entry_names_for(
                                                    &dtype_deduced.to_base_representation(),
                                                ),
                                            actual_type: dtype_deduced.to_base_representation(),
                                        },
                                    ));
                                }
                                (return_type, type_specified) => {
                                    return Err(TypeCheckError::IncompatibleTypesInAnnotation(
                                        "function definition".into(),
                                        *function_name_span,
                                        type_specified,
                                        return_type_annotation_span.unwrap(),
                                        return_type.clone(),
                                        body.as_ref().map(|b| b.full_span()).unwrap(),
                                    ));
                                }
                            }
                        }
                        return_type
                    } else {
                        return_type
                    }
                } else {
                    if !ffi::functions().contains_key(function_name.as_str()) {
                        return Err(TypeCheckError::UnknownForeignFunction(
                            *function_name_span,
                            function_name.clone(),
                        ));
                    }

                    annotated_return_type.ok_or_else(|| {
                        TypeCheckError::ForeignFunctionNeedsTypeAnnotations(
                            *function_name_span,
                            function_name.clone(),
                        )
                    })?
                };

                typechecker_fn
                    .add_equal_constraint(&return_type_inferred, &return_type)
                    .ok();

                self.constraints = typechecker_fn.constraints;
                self.name_generator = typechecker_fn.name_generator;
                self.env = typechecker_fn.env;
                // self.type_namespace = typechecker_fn.type_namespace;
                // self.value_namespace = typechecker_fn.value_namespace;

                typed_ast::Statement::DefineFunction(
                    function_name.clone(),
                    decorators.clone(),
                    type_parameters
                        .iter()
                        .map(|(_, name)| name.clone())
                        .collect(),
                    typed_parameters
                        .iter()
                        .map(|(span, name, _)| (*span, name.clone()))
                        .collect(),
                    body_checked,
                    fn_type,
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
                    .map(|e| self.elaborate_expression(e))
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
                    .map(|e| self.elaborate_expression(e))
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

    fn check_statement(&mut self, statement: &ast::Statement) -> Result<typed_ast::Statement> {
        self.constraints.clear();

        // Elaborate the program/statement: turn the AST into a typed AST, possibly
        // with "holes" inside, i.e. type variables that will only later be filled
        // in (after constraint solving).
        let mut elaborated_statement = self.elaborate_statement(&statement)?;

        info!("=========================================");
        info!("Elaborated statements:");
        info!("{}", elaborated_statement.pretty_print());
        info!("");

        info!("Constraints:");
        info!("{}", self.constraints.pretty_print(2));
        info!("");

        // Solve constraints
        let (substitution, dtype_variables) = self
            .constraints
            .solve()
            .map_err(TypeCheckError::ConstraintSolverError)?;

        elaborated_statement
            .apply(&substitution)
            .map_err(TypeCheckError::SubstitutionError)?;

        self.env.apply(&substitution)?;

        // For all dimension type variables that are still free, check all of their occurences
        // within type_, and then multiply the corresponding exponents with the least common
        // multiple of the denominators of the exponents. For example, this will turn
        // T0^(1/3) -> T0^(1/5) -> T0 into T0^5 -> T0^3 -> T0^15.
        // for tv in &dtype_variables {
        //     let exponents = elaborated_statement.exponents_for(&tv);
        //     let lcm = exponents
        //         .iter()
        //         .fold(1, |acc, e| num_integer::lcm(acc, *e.denom()));

        //     if lcm != 1 {
        //         let s = Substitution::single(
        //             tv.clone(),
        //             Type::Dimension(
        //                 DType::from_type_variable(tv.clone()).power(Exponent::from_integer(lcm)),
        //             ),
        //         );

        //         elaborated_statement.apply(&s).unwrap();
        //     }
        // }

        elaborated_statement.generalize_types(&dtype_variables);
        self.env.generalize_types(&dtype_variables);

        info!("Final statement:");
        info!("{}", elaborated_statement.pretty_print());

        Ok(elaborated_statement)
    }

    pub fn check(
        &mut self,
        statements: impl IntoIterator<Item = ast::Statement>,
    ) -> Result<Vec<typed_ast::Statement>> {
        let mut checked_statements = vec![];

        for statement in statements.into_iter() {
            checked_statements.push(self.check_statement(&statement)?);
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
