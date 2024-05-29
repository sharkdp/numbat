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
mod type_scheme;

use std::collections::{HashMap, HashSet};

use crate::arithmetic::{Power, Rational};
use crate::ast::{self, BinaryOperator, ProcedureKind, StringPart, TypeAnnotation, TypeExpression};
use crate::dimension::DimensionRegistry;
use crate::name_resolution::Namespace;
use crate::name_resolution::LAST_RESULT_IDENTIFIERS;
use crate::registry::{BaseRepresentationFactor, RegistryError};
use crate::span::Span;
use crate::typed_ast::{self, DType, Expression, StructInfo, Type};
use crate::{decorator, ffi, suggestion};

use const_evaluation::evaluate_const_expr;
use constraints::{Constraint, ConstraintSet, TrivialResultion};
use itertools::Itertools;
use name_generator::NameGenerator;
use num_traits::Zero;

pub use error::{Result, TypeCheckError};
pub use incompatible_dimensions::IncompatibleDimensionsError;
use substitutions::ApplySubstitution;

fn dtype(e: &Expression) -> Result<DType> {
    match e.get_type() {
        Type::Dimension(dtype) => Ok(dtype),
        t => Err(TypeCheckError::ExpectedDimensionType(e.full_span(), t)),
    }
}

#[derive(Clone)]
pub struct FunctionSignature {
    definition_span: Span,
    pub type_parameters: Vec<(Span, String)>,
    pub parameter_types: Vec<(Span, String, Type)>,
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
            TypeAnnotation::List(_, element_type) => Ok(Type::List(Box::new(
                self.type_from_annotation(element_type)?,
            ))),
        }
    }

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
            type_parameters,
            parameter_types,
            return_type,
        } = signature;

        let arity_range = parameter_types.len()..=parameter_types.len();

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

        for (idx, ((parameter_span, _, parameter_type), argument_type)) in
            parameter_types.iter().zip(argument_types).enumerate()
        {
            match (parameter_type, argument_type) {
                (parameter_type, argument_type)
                    if (parameter_type.is_dtype() && argument_type.is_dtype())
                        || (matches!(parameter_type, Type::List(inner) if inner.is_dtype())
                            && matches!(&argument_type, Type::List(inner) if inner.is_dtype())) =>
                {
                    // What we have above and below is horrible, but unfortunately, we do not
                    // have box patterns on stable Rust, so we need to do this in two steps.
                    let parameter_type = match parameter_type {
                        Type::List(element_type) => match element_type.as_ref() {
                            Type::Dimension(inner) => inner.clone(),
                            _ => unreachable!(),
                        },
                        Type::Dimension(inner) => inner.clone(),
                        _ => unreachable!(),
                    };
                    let argument_type = match argument_type {
                        Type::List(element_type) => match *element_type {
                            Type::Dimension(inner) => inner.clone(),
                            _ => unreachable!(),
                        },
                        Type::Dimension(inner) => inner.clone(),
                        _ => unreachable!(),
                    };

                    let mut parameter_type = substitute(&substitutions, &parameter_type);

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
                    if self
                        .add_equal_constraint(parameter_type, &argument_type)
                        .is_trivially_violated()
                    {
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
                Type::Dimension(d) => Type::Dimension(substitute(&substitutions, d)),
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
                Type::List(element_type) => Type::List(Box::new(apply_substitutions(
                    element_type,
                    substitute,
                    substitutions,
                ))),
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

    fn elaborate_expression(&mut self, ast: &ast::Expression) -> Result<typed_ast::Expression> {
        Ok(match ast {
            ast::Expression::Scalar(span, n) if n.to_f64().is_zero() => {
                let polymorphic_zero_type = self.fresh_type_variable();
                self.constraints
                    .add(Constraint::IsDType(polymorphic_zero_type.clone()))
                    .ok();
                typed_ast::Expression::Scalar(*span, *n, polymorphic_zero_type)
            }
            ast::Expression::Scalar(span, n) => {
                typed_ast::Expression::Scalar(*span, *n, Type::scalar())
            }
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

                typed_ast::Expression::UnaryOperator(*span_op, *op, Box::new(checked_expr), type_)
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
                                    expected_dimensions: self
                                        .registry
                                        .get_derived_entry_names_for(&lhs_dtype),
                                    expected_type: lhs_dtype,
                                    span_actual: rhs.full_span(),
                                    actual_name: "right hand side",
                                    actual_name_for_fix: "expression on the right hand side",
                                    actual_dimensions: self
                                        .registry
                                        .get_derived_entry_names_for(&rhs_dtype),
                                    actual_type: rhs_dtype,
                                },
                            ));
                        }

                        Ok(lhs_type)
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
                                    Type::Dimension(exponent_type), // TODO
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
                                if &arg_checked.get_type() != param_type {
                                    // return Err(TypeCheckError::IncompatibleTypesInFunctionCall(
                                    //     None,
                                    //     param_type.clone(),
                                    //     arg_checked.full_span(),
                                    //     arg_checked.get_type(),
                                    // ));
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

                typed_ast::Expression::List(*span, elements_checked, result_element_type)
            }
        })
    }

    fn elaborate_statement(&mut self, ast: &ast::Statement) -> Result<typed_ast::Statement> {
        Ok(match ast {
            ast::Statement::Expression(expr) => {
                let checked_expr = self.elaborate_expression(expr)?;
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
                let expr_checked = self.elaborate_expression(expr)?;
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
                let mut free_type_parameters = vec![];
                for (parameter_span, parameter, type_annotation) in parameters {
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
                    typed_parameters.push((*parameter_span, parameter.clone(), parameter_type));
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
                        .map(|(span, name, t)| (*span, name.clone(), t.clone()))
                        .collect();
                    tc.functions.insert(
                        function_name.clone(),
                        (
                            FunctionSignature {
                                definition_span: *function_name_span,
                                type_parameters: type_parameters.clone(),
                                parameter_types,
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
                    .map(|expr| typechecker_fn.elaborate_expression(&expr))
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
                                if self
                                    .add_equal_constraint(&type_deduced, &type_specified)
                                    .is_trivially_violated()
                                {
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

    pub fn check(
        &mut self,
        statements: impl IntoIterator<Item = ast::Statement>,
    ) -> Result<Vec<typed_ast::Statement>> {
        // Elaborate the program: turn the AST into a typed AST, possibly
        // with "holes" inside, i.e. type variables that will only later
        // be filled in (after constraint solving).
        let mut statements_elaborated = vec![];

        for statement in statements.into_iter() {
            statements_elaborated.push(self.elaborate_statement(&statement)?);
        }

        // Solve constraints
        let (substitution, _dtype_variables) = self
            .constraints
            .solve()
            .map_err(TypeCheckError::ConstraintSolverError)?;

        for statement in &mut statements_elaborated {
            statement
                .apply(&substitution)
                .map_err(TypeCheckError::SubstitutionError)?;
        }

        Ok(statements_elaborated)
    }

    pub(crate) fn registry(&self) -> &DimensionRegistry {
        &self.registry
    }

    pub fn lookup_function(&self, name: &str) -> Option<&(FunctionSignature, FunctionMetadata)> {
        self.functions.get(name)
    }
}
