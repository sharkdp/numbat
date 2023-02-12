mod arithmetic;
mod ast;
mod bytecode_interpreter;
mod dimension;
mod ffi;
mod interpreter;
mod number;
mod parser;
mod prefix;
mod prefix_parser;
mod prefix_transformer;
pub mod pretty_print;
mod product;
mod quantity;
mod registry;
mod span;
mod tokenizer;
mod typechecker;
mod typed_ast;
mod unit;
mod unit_registry;
mod vm;

use bytecode_interpreter::BytecodeInterpreter;
use interpreter::{Interpreter, RuntimeError};
use parser::parse;
use prefix_transformer::Transformer;
use thiserror::Error;
use typechecker::{TypeCheckError, TypeChecker};

use ast::Statement;
pub use interpreter::ExitStatus;
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
    prefix_transformer: Transformer,
    typechecker: TypeChecker,
    interpreter: BytecodeInterpreter,
}

impl Insect {
    pub fn new_without_prelude(debug: bool) -> Self {
        Insect {
            prefix_transformer: Transformer::new(),
            typechecker: TypeChecker::default(),
            interpreter: BytecodeInterpreter::new(debug),
        }
    }

    pub fn new(debug: bool) -> Self {
        let mut insect = Self::new_without_prelude(debug);
        assert!(insect
            .interpret(include_str!("../../prelude.ins"))
            .expect("Error while running prelude")
            .1
            .is_success()); // TODO: read prelude dynamically, error handling
        insect
    }

    pub fn interpret(&mut self, code: &str) -> Result<(Vec<Statement>, InterpreterResult)> {
        let statements = parse(code).map_err(InsectError::ParseError)?;
        let transformed_statements = self.prefix_transformer.transform(&statements[..]);
        let typed_statements = self
            .typechecker
            .check_statements(transformed_statements.clone()) // TODO: get rid of clone?
            .map_err(InsectError::TypeCheckError)?;
        let result = self
            .interpreter
            .interpret_statements(&typed_statements)
            .map_err(InsectError::RuntimeError)?;

        Ok((transformed_statements, result))
    }
}
