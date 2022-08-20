use crate::ast::Statement;

use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
pub enum InterpreterError {
    #[error("Division by zero")]
    DivisionByZero,
}

#[derive(Debug, PartialEq)]
pub enum InterpreterResult {
    Value(f64),
    Continue,
    Exit,
}

pub type Result<T> = std::result::Result<T, InterpreterError>;

pub trait Interpreter {
    fn interpret(&mut self, statement: &Statement) -> Result<InterpreterResult>;
}
