use std::collections::HashMap;

use crate::ast::{BinaryOperator, Command, Expression, Statement};
use crate::interpreter::{Interpreter, InterpreterError, InterpreterResult, Result};
use crate::quantity::Quantity;

pub struct TreewalkInterpreter {
    variables: HashMap<String, Quantity>,
}

impl TreewalkInterpreter {
    fn evaluate_expression(&self, expr: &Expression) -> Result<Quantity> {
        match expr {
            Expression::Scalar(n) => Ok(n.into()),
            Expression::Identifier(identifier) => {
                if let Some(quantity) = self.variables.get(identifier) {
                    Ok(quantity.clone())
                } else {
                    Err(InterpreterError::UnknownVariable(identifier.clone()))
                }
            }
            Expression::Negate(rhs) => self.evaluate_expression(rhs).map(|v| -v),
            Expression::BinaryOperator(op, lhs, rhs) => {
                let lhs = self.evaluate_expression(lhs)?;
                let rhs = self.evaluate_expression(rhs)?;

                match op {
                    BinaryOperator::Add => Ok((lhs + rhs).map_err(InterpreterError::UnitError)?),
                    BinaryOperator::Sub => Ok((lhs - rhs).map_err(InterpreterError::UnitError)?),
                    BinaryOperator::Mul => Ok((lhs * rhs).map_err(InterpreterError::UnitError)?),
                    BinaryOperator::Div => {
                        if rhs.is_zero() {
                            Err(InterpreterError::DivisionByZero)
                        } else {
                            Ok((lhs / rhs).map_err(InterpreterError::UnitError)?)
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
                let quantity = self.evaluate_expression(expr)?;
                Ok(InterpreterResult::Quantity(quantity))
            }
            Statement::Command(Command::List) => {
                println!("List of variables:");

                Ok(InterpreterResult::Continue)
            }
            Statement::Command(Command::Exit) => Ok(InterpreterResult::Exit),
            Statement::DeclareVariable(identifier, expr) => {
                let quantity = self.evaluate_expression(expr)?;
                self.variables.insert(identifier.clone(), quantity);

                Ok(InterpreterResult::Continue)
            }
            _ => todo!(),
        }
    }
}
