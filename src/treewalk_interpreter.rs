use crate::ast::{BinaryOperator, Command, Expression, Statement};
use crate::interpreter::{Interpreter, InterpreterError, NextAction, Result};

pub struct TreewalkInterpreter {}

impl TreewalkInterpreter {
    pub fn new() -> Self {
        Self {}
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
}

impl Interpreter for TreewalkInterpreter {
    fn interpret(&mut self, stmt: &Statement) -> Result<NextAction> {
        match stmt {
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
