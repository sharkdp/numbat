use crate::ast::Statement;

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

pub type Result<T> = std::result::Result<T, InterpreterError>;

pub trait Interpreter {
    fn interpret(&mut self, statement: &Statement) -> Result<NextAction>;
}
