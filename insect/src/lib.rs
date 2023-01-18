mod arithmetic;
mod ast;
mod bytecode_interpreter;
mod dimension;
mod interpreter;
mod number;
mod parser;
pub mod pretty_print;
mod product;
mod quantity;
mod registry;
mod span;
mod tokenizer;
mod treewalk_interpreter;
mod typechecker;
mod typed_ast;
mod unit;
mod unit_registry;
mod vm;

use bytecode_interpreter::BytecodeInterpreter;
use interpreter::{Interpreter, RuntimeError};
use parser::parse;
use thiserror::Error;
use typechecker::{TypeCheckError, TypeChecker};

use ast::Statement;
pub use interpreter::InterpreterResult;
pub use parser::ParseError;

#[derive(Debug, Error)]
pub enum InsectError {
    #[error("{0}")]
    ParseError(ParseError),
    #[error("{0}")]
    TypeCheckError(TypeCheckError),
    #[error("{0}")]
    RuntimeError(RuntimeError),
}

pub type Result<T> = std::result::Result<T, InsectError>;

pub struct Insect {
    typechecker: TypeChecker,
    interpreter: BytecodeInterpreter,
}

impl Insect {
    pub fn new_without_prelude(debug: bool) -> Self {
        Insect {
            typechecker: TypeChecker::default(),
            interpreter: BytecodeInterpreter::new(debug),
        }
    }

    pub fn new(debug: bool) -> Self {
        let mut insect = Self::new_without_prelude(debug);
        insect
            .interpret(include_str!("../../prelude.ins"))
            .expect("Error while running prelude"); // TODO: read prelude dynamically, error handling
        insect
    }

    pub fn interpret(&mut self, code: &str) -> Result<(Vec<Statement>, InterpreterResult)> {
        let statements = parse(&code).map_err(InsectError::ParseError)?;
        let typed_statements = self
            .typechecker
            .check_statements(statements.clone()) // TODO: get rid of clone?
            .map_err(InsectError::TypeCheckError)?;
        let result = self
            .interpreter
            .interpret_statements(&typed_statements)
            .map_err(InsectError::RuntimeError)?;

        Ok((statements, result))
    }
}
