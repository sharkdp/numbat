use std::collections::HashMap;

use crate::arithmetic::Power;
use crate::ast;
use crate::dimension::DimensionRegistry;
use crate::registry::BaseRepresentation;
use crate::typed_ast::{self, Type};

use thiserror::Error;

#[derive(Debug, Error, PartialEq, Eq)]
pub enum TypeCheckError {
    // #[error("Wrong dimension: unit '{0}' is not of dimension '{1}'")]
    // IncompatibleDimension(Unit, String),
    #[error("Incompatible dimensions: '{0}' and '{1}'")]
    IncompatibleDimensions(BaseRepresentation, BaseRepresentation),
}

type Result<T> = std::result::Result<T, TypeCheckError>;

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

    fn type_for_identifier(&self, name: &str) -> &Type {
        self.types_for_identifier.get(name).unwrap()
    }

    pub(crate) fn check_expression(&self, ast: ast::Expression) -> Result<typed_ast::Expression> {
        Ok(match ast {
            ast::Expression::Scalar(n) => typed_ast::Expression::Scalar(n),
            ast::Expression::Identifier(name) => {
                let type_ = self.type_for_identifier(&name).clone();

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
                        Err(TypeCheckError::IncompatibleDimensions(lhs_type, rhs_type))
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
                            typed_ast::Expression::Identifier(_, _) => todo!(),
                            typed_ast::Expression::Negate(_, _) => todo!(),
                            typed_ast::Expression::BinaryOperator(_, _, _, _) => todo!(),
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
            ast::Statement::DeclareVariable(name, expr, optional_dexpr) => {
                let expr = self.check_expression(expr)?;
                let inferred_type = expr.get_type();
                if let Some(ref dexpr) = optional_dexpr {
                    let specified_type = self.registry.get_base_representation(dexpr).unwrap();
                    if inferred_type != specified_type {
                        return Err(TypeCheckError::IncompatibleDimensions(
                            specified_type,
                            inferred_type,
                        ));
                    }
                }
                self.types_for_identifier
                    .insert(name.clone(), inferred_type);
                typed_ast::Statement::DeclareVariable(name, expr, optional_dexpr)
            }
            ast::Statement::Expression(expr) => {
                typed_ast::Statement::Expression(self.check_expression(expr)?)
            }
            ast::Statement::DeclareDerivedUnit(name, expr, dexpr) => {
                // TODO: check against dexpr
                let expr = self.check_expression(expr)?;
                let type_ = expr.get_type();
                self.types_for_identifier.insert(name.clone(), type_);
                typed_ast::Statement::DeclareDerivedUnit(name, expr, dexpr)
            }
            // Trivial cases:
            ast::Statement::Command(command) => typed_ast::Statement::Command(command),
            ast::Statement::DeclareDimension(name, dexprs) => {
                if let Some(dexpr) = dexprs.first() {
                    self.registry.add_derived_dimension(&name, dexpr).unwrap(); // TODO
                } else {
                    self.registry.add_base_dimension(&name).unwrap(); // TODO
                }
                typed_ast::Statement::DeclareDimension(name, dexprs)
            }
            ast::Statement::DeclareBaseUnit(name, dexpr) => {
                let type_ = self.registry.get_base_representation(&dexpr).unwrap(); // TODO
                self.types_for_identifier.insert(name.clone(), type_);
                typed_ast::Statement::DeclareBaseUnit(name, dexpr)
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
