use crate::ast::{BinaryOperator, Command, Expression, Statement};

use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
pub enum InterpreterError {
    #[error("Division by zero")]
    DivisionByZero,
}

pub enum NextAction {
    Continue,
    Quit,
}

type Result<T> = std::result::Result<T, InterpreterError>;

struct Interpreter<'a> {
    ast: &'a Statement,
}

impl<'a> Interpreter<'a> {
    fn new(ast: &'a Statement) -> Self {
        Self { ast }
    }

    fn evaluate_expression(&self, expr: &Expression) -> Result<f64> {
        match expr {
            Expression::Scalar(n) => Ok(n.to_f64()),
            Expression::Identifier(_) => todo!(),
            Expression::Negate(rhs) => self.evaluate_expression(rhs).map(|v| -v),
            Expression::BinaryOperator(op, lhs, rhs) => {
                let lhs = self.evaluate_expression(lhs)?;
                let rhs = self.evaluate_expression(rhs)?;

                match op {
                    BinaryOperator::Add => Ok(lhs + rhs),
                    BinaryOperator::Sub => Ok(lhs - rhs),
                    BinaryOperator::Mul => Ok(lhs * rhs),
                    BinaryOperator::Div => {
                        let result = lhs / rhs;
                        if !result.is_finite() {
                            Err(InterpreterError::DivisionByZero)
                        } else {
                            Ok(result)
                        }
                    }
                    BinaryOperator::ConvertTo => todo!(),
                }
            }
        }
    }

    fn run(&mut self) -> Result<NextAction> {
        match self.ast {
            Statement::Expression(expr) => {
                let value = self.evaluate_expression(expr)?;
                println!();
                println!("    = {value:.1}", value = value);
                println!();
            }
            Statement::Command(Command::List) => {
                println!("List of variables:");
            }
            Statement::Command(Command::Quit) => {
                return Ok(NextAction::Quit);
            }
            Statement::Assignment(_, _) => {
                // TODO
            }
        }

        Ok(NextAction::Continue)
    }
}

pub fn interpret(stmt: &Statement) -> Result<NextAction> {
    let mut interpreter = Interpreter::new(stmt);
    interpreter.run()
}
