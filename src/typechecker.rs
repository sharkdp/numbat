use crate::ast;
use crate::typed_ast;
use crate::unit::Unit;

use thiserror::Error;

#[derive(Debug, Error, PartialEq, Eq)]
pub enum TypeCheckError {
    #[error("Wrong dimension: unit '{0}' is not of dimension '{1}'")]
    IncompatibleDimension(Unit, String),
}

pub(crate) struct TypeChecker {}

impl TypeChecker {
    pub(crate) fn new() -> Self {
        Self {}
    }

    pub(crate) fn check_expression(&self, ast: ast::Expression) -> typed_ast::Expression {
        match ast {
            ast::Expression::Scalar(n) => typed_ast::Expression::Scalar(n),
            ast::Expression::Identifier(name) => typed_ast::Expression::Identifier(name),
            ast::Expression::Negate(expr) => {
                typed_ast::Expression::Negate(Box::new(self.check_expression(*expr)))
            }
            ast::Expression::BinaryOperator(op, lhs, rhs) => typed_ast::Expression::BinaryOperator(
                op,
                Box::new(self.check_expression(*lhs)),
                Box::new(self.check_expression(*rhs)),
            ),
        }
    }

    pub fn check_statement(&self, ast: ast::Statement) -> typed_ast::Statement {
        match ast {
            ast::Statement::DeclareVariable(name, expr, optional_dexpr) => {
                typed_ast::Statement::DeclareVariable(
                    name,
                    self.check_expression(expr),
                    optional_dexpr,
                )
            }
            ast::Statement::Expression(expr) => {
                typed_ast::Statement::Expression(self.check_expression(expr))
            }
            ast::Statement::DeclareDerivedUnit(name, expr, dexpr) => {
                typed_ast::Statement::DeclareDerivedUnit(name, self.check_expression(expr), dexpr)
            }
            // Trivial cases:
            ast::Statement::Command(command) => typed_ast::Statement::Command(command),
            ast::Statement::DeclareDimension(name, dexprs) => {
                typed_ast::Statement::DeclareDimension(name, dexprs)
            }
            ast::Statement::DeclareBaseUnit(name, dexpr) => {
                typed_ast::Statement::DeclareBaseUnit(name, dexpr)
            }
        }
    }
}

pub fn typecheck(
    statements: impl IntoIterator<Item = ast::Statement>,
) -> Vec<typed_ast::Statement> {
    let mut statements_checked = vec![];
    let typechecker = TypeChecker::new();
    for statement in statements.into_iter() {
        statements_checked.push(typechecker.check_statement(statement));
    }
    statements_checked
}
