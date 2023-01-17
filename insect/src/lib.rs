mod arithmetic;
mod ast;
pub mod bytecode_interpreter;
mod dimension;
pub mod interpreter;
mod number;
pub mod parser;
pub mod pretty_print;
mod product;
mod quantity;
mod registry;
mod span;
mod tokenizer;
pub mod treewalk_interpreter;
pub mod typechecker;
mod typed_ast;
mod unit;
mod unit_registry;
mod vm;

use bytecode_interpreter::BytecodeInterpreter;
use interpreter::{Interpreter, InterpreterError};
use parser::{parse, ParseError};
use thiserror::Error;
use typechecker::{TypeCheckError, TypeChecker};

pub use interpreter::InterpreterResult;

#[derive(Debug, Error)]
pub enum InsectError {
    #[error("{0}")]
    ParseError(ParseError),
    #[error("{0}")]
    TypeCheckError(TypeCheckError),
    #[error("{0}")]
    InterpreterError(InterpreterError),
}

pub type Result<T> = std::result::Result<T, InsectError>;

pub struct Insect {
    typechecker: TypeChecker,
    interpreter: BytecodeInterpreter,
}

impl Insect {
    pub fn new_without_prelude() -> Self {
        Insect {
            typechecker: TypeChecker::default(),
            interpreter: BytecodeInterpreter::new(false),
        }
    }

    pub fn new() -> Self {
        let mut insect = Self::new_without_prelude();
        insect
            .interpret(include_str!("../../prelude.ins"))
            .expect("Error while running prelude"); // TODO: read prelude dynamically, error handling
        insect
    }

    pub fn interpret(&mut self, code: &str) -> Result<InterpreterResult> {
        let statements = parse(&code).map_err(InsectError::ParseError)?;
        let statements = self
            .typechecker
            .check_statements(statements)
            .map_err(InsectError::TypeCheckError)?;
        let result = self
            .interpreter
            .interpret_statements(&statements)
            .map_err(InsectError::InterpreterError)?;

        Ok(result)
    }
}
