use crate::ast::{BinaryOperator, Expression};

use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
pub enum InterpreterError {
    #[error("Division by zero")]
    DivisionByZero,
}

type Result<T> = std::result::Result<T, InterpreterError>;

struct Interpreter<'a> {
    ast: &'a Expression,
}

impl<'a> Interpreter<'a> {
    fn new(ast: &'a Expression) -> Self {
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

    fn run(&mut self) -> Result<()> {
        let value = self.evaluate_expression(self.ast)?;
        println!("  = {value:.1}", value = value);

        Ok(())
    }
}

pub fn run(expr: &Expression) -> Result<()> {
    let mut interpreter = Interpreter::new(expr);
    interpreter.run()
}
