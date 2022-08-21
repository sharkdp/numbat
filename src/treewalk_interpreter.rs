use crate::ast::{BinaryOperator, Command, Expression, Statement};
use crate::interpreter::{Interpreter, InterpreterError, InterpreterResult, Result};

pub struct TreewalkInterpreter {}

impl TreewalkInterpreter {
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
    fn new() -> Self {
        Self {}
    }

    fn interpret(&mut self, stmt: &Statement) -> Result<InterpreterResult> {
        match stmt {
            Statement::Expression(expr) => {
                let value = self.evaluate_expression(expr)?;
                Ok(InterpreterResult::Value(value))
            }
            Statement::Command(Command::List) => {
                println!("List of variables:");

                Ok(InterpreterResult::Continue)
            }
            Statement::Command(Command::Exit) => Ok(InterpreterResult::Exit),
            Statement::Assignment(_, _) => {
                todo!()
            }
        }
    }
}
