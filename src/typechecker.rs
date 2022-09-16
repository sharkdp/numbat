use std::collections::HashMap;

use crate::arithmetic::Power;
use crate::ast;
use crate::dimension::DimensionRegistry;
use crate::registry::{BaseRepresentation, RegistryError};
use crate::typed_ast::{self, Type};

use thiserror::Error;

#[derive(Debug, Error, PartialEq, Eq)]
pub enum TypeCheckError {
    #[error("Unknown identifier '{0}'")]
    UnknownIdentifier(String),
    #[error("Incompatible dimensions in {0}:\n    {1}: {2}\n    {3}: {4}")]
    IncompatibleDimensions(
        &'static str,
        &'static str,
        BaseRepresentation,
        &'static str,
        BaseRepresentation,
    ),
    #[error("{0}")]
    RegistryError(RegistryError),
    #[error("Incompatible alternative expressions have been provided for dimension '{0}'")]
    IncompatibleAlternativeDimensionExpression(String),
}

type Result<T> = std::result::Result<T, TypeCheckError>;

#[derive(Clone)]
pub struct TypeChecker {
    types_for_identifier: HashMap<String, Type>,
    registry: DimensionRegistry,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            types_for_identifier: HashMap::default(),
            registry: DimensionRegistry::new(),
        }
    }

    fn type_for_identifier(&self, name: &str) -> Result<&Type> {
        self.types_for_identifier
            .get(name)
            .ok_or_else(|| TypeCheckError::UnknownIdentifier(name.into()))
    }

    pub(crate) fn check_expression(&self, ast: ast::Expression) -> Result<typed_ast::Expression> {
        Ok(match ast {
            ast::Expression::Scalar(n) => typed_ast::Expression::Scalar(n),
            ast::Expression::Identifier(name) => {
                let type_ = self.type_for_identifier(&name)?.clone();

                typed_ast::Expression::Identifier(name, type_)
            }
            ast::Expression::Negate(expr) => {
                let checked_expr = self.check_expression(*expr)?;
                let type_ = checked_expr.get_type();
                typed_ast::Expression::Negate(Box::new(checked_expr), type_)
            }
            ast::Expression::BinaryOperator(op, lhs, rhs) => {
                let lhs = self.check_expression(*lhs)?;
                let rhs = self.check_expression(*rhs)?;

                let get_type_and_assert_equality = || {
                    let lhs_type = lhs.get_type();
                    let rhs_type = rhs.get_type();
                    if lhs_type != rhs_type {
                        Err(TypeCheckError::IncompatibleDimensions(
                            "binary operator",
                            " left hand side",
                            lhs_type,
                            "right hand side",
                            rhs_type,
                        ))
                    } else {
                        Ok(lhs_type)
                    }
                };

                let _type = match op {
                    typed_ast::BinaryOperator::Add => get_type_and_assert_equality()?,
                    typed_ast::BinaryOperator::Sub => get_type_and_assert_equality()?,
                    typed_ast::BinaryOperator::Mul => lhs.get_type().multiply(rhs.get_type()),
                    typed_ast::BinaryOperator::Div => lhs.get_type().divide(rhs.get_type()),
                    typed_ast::BinaryOperator::Power => {
                        let exponent = match &rhs {
                            typed_ast::Expression::Scalar(n) => n.to_f64() as i32, // TODO!
                            _ => todo!(),
                        };
                        lhs.get_type().power(exponent)
                    }
                    typed_ast::BinaryOperator::ConvertTo => get_type_and_assert_equality()?,
                };

                typed_ast::Expression::BinaryOperator(op, Box::new(lhs), Box::new(rhs), _type)
            }
        })
    }

    pub fn check_statement(&mut self, ast: ast::Statement) -> Result<typed_ast::Statement> {
        Ok(match ast {
            ast::Statement::Expression(expr) => {
                typed_ast::Statement::Expression(self.check_expression(expr)?)
            }
            ast::Statement::DeclareVariable(name, expr, optional_dexpr) => {
                let expr = self.check_expression(expr)?;
                let type_deduced = expr.get_type();

                if let Some(ref dexpr) = optional_dexpr {
                    let type_specified = self
                        .registry
                        .get_base_representation(dexpr)
                        .map_err(TypeCheckError::RegistryError)?;
                    if type_deduced != type_specified {
                        return Err(TypeCheckError::IncompatibleDimensions(
                            "variable declaration",
                            "specified dimension",
                            type_specified,
                            "   actual dimension",
                            type_deduced,
                        ));
                    }
                }
                self.types_for_identifier
                    .insert(name.clone(), type_deduced.clone());
                typed_ast::Statement::DeclareVariable(name, expr, type_deduced)
            }
            ast::Statement::DeclareDerivedUnit(name, expr, optional_dexpr) => {
                // TODO: this is the *exact same code* that we have above for
                // variable declarations => deduplicate this somehow
                let expr = self.check_expression(expr)?;
                let type_deduced = expr.get_type();

                if let Some(ref dexpr) = optional_dexpr {
                    let type_specified = self
                        .registry
                        .get_base_representation(dexpr)
                        .map_err(TypeCheckError::RegistryError)?;
                    if type_deduced != type_specified {
                        return Err(TypeCheckError::IncompatibleDimensions(
                            "derived unit declaration",
                            "specified dimension",
                            type_specified,
                            "   actual dimension",
                            type_deduced,
                        ));
                    }
                }
                self.types_for_identifier
                    .insert(name.clone(), type_deduced.clone());
                typed_ast::Statement::DeclareDerivedUnit(name, expr, type_deduced)
            }
            ast::Statement::DeclareFunction(
                function_name,
                parameters,
                expr,
                optional_return_type_dexpr,
            ) => {
                let mut typechecker_fn = self.clone();
                let mut typed_parameters = vec![];
                for (parameter, optional_dexpr) in parameters {
                    let parameter_type = typechecker_fn
                        .registry
                        .get_base_representation(&optional_dexpr.unwrap())
                        .unwrap(); // TODO: error handling and parameter type deduction, in case it is not specified
                    typechecker_fn
                        .types_for_identifier
                        .insert(parameter.clone(), parameter_type.clone());
                    typed_parameters.push((parameter.clone(), parameter_type));
                }
                let expr = typechecker_fn.check_expression(expr).unwrap();

                let return_type_deduced = expr.get_type();
                if let Some(ref return_type_dexpr) = optional_return_type_dexpr {
                    let return_type_specified = typechecker_fn
                        .registry
                        .get_base_representation(return_type_dexpr)
                        .map_err(TypeCheckError::RegistryError)?;

                    if return_type_deduced != return_type_specified {
                        return Err(TypeCheckError::IncompatibleDimensions(
                            "function return type",
                            "specified return type",
                            return_type_specified,
                            "   actual return type",
                            return_type_deduced,
                        ));
                    }
                }

                typed_ast::Statement::DeclareFunction(
                    function_name,
                    typed_parameters,
                    expr,
                    return_type_deduced,
                )
            }
            ast::Statement::Command(command) => typed_ast::Statement::Command(command),
            ast::Statement::DeclareDimension(name, dexprs) => {
                if let Some(dexpr) = dexprs.first() {
                    self.registry
                        .add_derived_dimension(&name, dexpr)
                        .map_err(TypeCheckError::RegistryError)?;

                    let base_representation = self
                        .registry
                        .get_base_representation_for_name(&name)
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
                                ),
                            );
                        }
                    }
                } else {
                    self.registry
                        .add_base_dimension(&name)
                        .map_err(TypeCheckError::RegistryError)?;
                }
                typed_ast::Statement::DeclareDimension(name)
            }
            ast::Statement::DeclareBaseUnit(name, dexpr) => {
                let type_specified = self
                    .registry
                    .get_base_representation(&dexpr)
                    .map_err(TypeCheckError::RegistryError)?;
                self.types_for_identifier
                    .insert(name.clone(), type_specified.clone());
                typed_ast::Statement::DeclareBaseUnit(name, type_specified)
            }
        })
    }

    pub fn check_statements(
        &mut self,
        statements: impl IntoIterator<Item = ast::Statement>,
    ) -> Result<Vec<typed_ast::Statement>> {
        let mut statements_checked = vec![];

        for statement in statements.into_iter() {
            statements_checked.push(self.check_statement(statement)?);
        }
        Ok(statements_checked)
    }
}

pub fn typecheck(
    statements: impl IntoIterator<Item = ast::Statement>,
) -> Result<Vec<typed_ast::Statement>> {
    let mut typechecker = TypeChecker::new();
    typechecker.check_statements(statements)
}

#[cfg(test)]
fn run_typecheck(input: &str) -> Result<typed_ast::Statement> {
    let statements =
        crate::parser::parse(input).expect("No parse errors for inputs in this test suite");

    let mut typechecker = TypeChecker::new();
    typechecker
        .check_statements(statements)
        .map(|mut statements_checked| statements_checked.pop().unwrap())
}

#[cfg(test)]
fn assert_successful_typecheck(input: &str) {
    assert!(run_typecheck(input).is_ok());
}

#[cfg(test)]
fn assert_typecheck_error(input: &str, err: TypeCheckError) {
    assert!(run_typecheck(input) == Err(err));
}

#[test]
fn basic() {
    use crate::registry::BaseRepresentationFactor;

    let mini_prelude = "dimension A
                        dimension B
                        dimension C = A * B
                        unit a: A
                        unit b: B
                        unit c: C = a * b";

    assert_successful_typecheck(&format!(
        "{mini_prelude}
         let x: C = a * b",
        mini_prelude = mini_prelude
    ));

    assert_successful_typecheck(&format!(
        "{mini_prelude}
         let x: C = 2 * a * b^2 / b",
        mini_prelude = mini_prelude
    ));

    assert_typecheck_error(
        &format!(
            "{mini_prelude}
             a + b",
            mini_prelude = mini_prelude
        ),
        TypeCheckError::IncompatibleDimensions(
            "binary operator",
            " left hand side",
            BaseRepresentation::from_factor(BaseRepresentationFactor("A".into(), 1)),
            "right hand side",
            BaseRepresentation::from_factor(BaseRepresentationFactor("B".into(), 1)),
        ),
    );

    assert_typecheck_error(
        &format!(
            "{mini_prelude}
             # wrong alternative expression: C / B^2
             dimension D = A / B = C / B^3",
            mini_prelude = mini_prelude
        ),
        TypeCheckError::IncompatibleAlternativeDimensionExpression("D".into()),
    );
}
