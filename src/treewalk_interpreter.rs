use std::collections::HashMap;

use crate::interpreter::{Interpreter, InterpreterError, InterpreterResult, Result};
use crate::quantity::Quantity;
use crate::typed_ast::{BinaryOperator, Command, Expression, Statement};

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

                let result = match op {
                    BinaryOperator::Add => lhs + rhs,
                    BinaryOperator::Sub => lhs - rhs,
                    BinaryOperator::Mul => lhs * rhs,
                    BinaryOperator::Div => {
                        if rhs.is_zero() {
                            return Err(InterpreterError::DivisionByZero);
                        } else {
                            lhs / rhs
                        }
                    }
                    BinaryOperator::Power => lhs.power(rhs),
                    BinaryOperator::ConvertTo => lhs.convert_to(rhs.unit()),
                };
                Ok(result.map_err(InterpreterError::ConversionError)?)
            }
        }
    }
}

impl Interpreter for TreewalkInterpreter {
    fn new(_debug: bool) -> Self {
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
            Statement::DeclareVariable(identifier, expr, _dexpr) => {
                let quantity = self.evaluate_expression(expr)?;
                self.variables.insert(identifier.clone(), quantity);

                Ok(InterpreterResult::Continue)
            }
            _ => todo!(),
        }
    }
}
