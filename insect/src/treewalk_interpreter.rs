use std::collections::HashMap;

use crate::interpreter::{Interpreter, InterpreterResult, Result, RuntimeError};
use crate::quantity::Quantity;
use crate::typed_ast::{BinaryOperator, Expression, Statement};

pub struct TreewalkInterpreter {
    variables: HashMap<String, Quantity>,
}

impl TreewalkInterpreter {
    fn evaluate_expression(&self, expr: &Expression) -> Result<Quantity> {
        match expr {
            Expression::Scalar(n) => Ok(n.into()),
            Expression::Identifier(identifier, _type) => {
                if let Some(quantity) = self.variables.get(identifier) {
                    Ok(quantity.clone())
                } else {
                    Err(RuntimeError::UnknownVariable(identifier.clone()))
                }
            }
            Expression::Negate(rhs, _type) => self.evaluate_expression(rhs).map(|v| -v),
            Expression::BinaryOperator(op, lhs, rhs, _type) => {
                let lhs = self.evaluate_expression(lhs)?;
                let rhs = self.evaluate_expression(rhs)?;

                let result = match op {
                    BinaryOperator::Add => lhs + rhs,
                    BinaryOperator::Sub => lhs - rhs,
                    BinaryOperator::Mul => lhs * rhs,
                    BinaryOperator::Div => {
                        if rhs.is_zero() {
                            return Err(RuntimeError::DivisionByZero);
                        } else {
                            lhs / rhs
                        }
                    }
                    BinaryOperator::Power => lhs.power(rhs),
                    BinaryOperator::ConvertTo => lhs.convert_to(rhs.unit()),
                };
                Ok(result.map_err(RuntimeError::ConversionError)?)
            }
            Expression::FunctionCall(_, _, _) => unimplemented!(),
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
            Statement::DeclareVariable(identifier, expr, _dexpr) => {
                let quantity = self.evaluate_expression(expr)?;
                self.variables.insert(identifier.clone(), quantity);

                Ok(InterpreterResult::Continue)
            }
            _ => unimplemented!(),
        }
    }
}
