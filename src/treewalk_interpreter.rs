use std::collections::HashMap;

use crate::ast::{BinaryOperator, Command, Expression, Statement};
use crate::interpreter::{Interpreter, InterpreterError, InterpreterResult, Result};

pub struct TreewalkInterpreter {
    variables: HashMap<String, f64>,
}

impl TreewalkInterpreter {
    fn evaluate_expression(&self, expr: &Expression) -> Result<f64> {
        match expr {
            Expression::Scalar(n) => Ok(n.to_f64()),
            Expression::Identifier(identifier) => {
                if let Some(value) = self.variables.get(identifier) {
                    Ok(*value)
                } else {
                    Err(InterpreterError::UnknownVariable(identifier.clone()))
                }
            }
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
        Self {
            variables: HashMap::new(),
        }
    }

    fn interpret_statement(&mut self, statement: &Statement) -> Result<InterpreterResult> {
        match statement {
            Statement::Expression(expr) => {
                let value = self.evaluate_expression(expr)?;
                Ok(InterpreterResult::Value(value))
            }
            Statement::Command(Command::List) => {
                println!("List of variables:");

                Ok(InterpreterResult::Continue)
            }
            Statement::Command(Command::Exit) => Ok(InterpreterResult::Exit),
            Statement::DeclareVariable(identifier, expr) => {
                let value = self.evaluate_expression(expr)?;
                self.variables.insert(identifier.clone(), value);

                Ok(InterpreterResult::Continue)
            }
            Statement::DeclareDimension(_, _) => todo!(),
        }
    }
}
