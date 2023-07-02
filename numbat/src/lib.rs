mod arithmetic;
mod ast;
mod bytecode_interpreter;
mod decorator;
mod dimension;
mod ffi;
mod interpreter;
pub mod markup;
mod name_resolution;
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
use name_resolution::NameResolutionError;
use parser::parse;
use prefix_transformer::Transformer;
use thiserror::Error;
use typechecker::{TypeCheckError, TypeChecker};

use ast::Statement;
pub use interpreter::ExitStatus;
pub use interpreter::InterpreterResult;
pub use parser::ParseError;

#[derive(Debug, Error)]
pub enum NumbatError {
    #[error("{0}")]
    ParseError(ParseError),
    #[error("{0}")]
    NameResolutionError(NameResolutionError),
    #[error("{0}")]
    TypeCheckError(TypeCheckError),
    #[error("{0}")]
    RuntimeError(RuntimeError),
}

pub type Result<T> = std::result::Result<T, NumbatError>;

pub struct Context {
    prefix_transformer: Transformer,
    typechecker: TypeChecker,
    interpreter: BytecodeInterpreter,
}

impl Context {
    pub fn new_without_prelude(debug: bool) -> Self {
        Context {
            prefix_transformer: Transformer::new(),
            typechecker: TypeChecker::default(),
            interpreter: BytecodeInterpreter::new(debug),
        }
    }

    pub fn new(debug: bool) -> Self {
        let mut numbat = Self::new_without_prelude(debug);
        assert!(numbat
            .interpret(include_str!("../../prelude.nbt"))
            .expect("Error while running prelude")
            .1
            .is_success()); // TODO: read prelude dynamically, error handling
        numbat
    }

    pub fn interpret(&mut self, code: &str) -> Result<(Vec<Statement>, InterpreterResult)> {
        let statements = parse(code).map_err(NumbatError::ParseError)?;
        let transformed_statements = self
            .prefix_transformer
            .transform(statements)
            .map_err(NumbatError::NameResolutionError)?;
        let typed_statements = self
            .typechecker
            .check_statements(transformed_statements.clone()) // TODO(minor): get rid of clone?
            .map_err(NumbatError::TypeCheckError)?;
        let result = self
            .interpreter
            .interpret_statements(&typed_statements)
            .map_err(NumbatError::RuntimeError)?;

        Ok((transformed_statements, result))
    }
}

#[cfg(test)]
mod tests {
    use crate::number::Number;
    use crate::quantity::Quantity;
    use crate::unit::Unit;

    use super::*;
    fn assert_yields_quantity(code: &str, quantity: &Quantity) {
        let result = Context::new(false).interpret(code).unwrap().1;
        match result {
            InterpreterResult::Quantity(q) => {
                assert_eq!(q, *quantity)
            }
            _ => panic!(),
        }
    }

    #[test]
    fn test_full_simplify_basic() {
        assert_yields_quantity("5 cm/m", &Quantity::from_scalar(0.05));
        assert_yields_quantity("hour/second", &Quantity::from_scalar(3600.0));
    }

    #[test]
    fn test_full_simplify_does_not_run_for_explicit_conversions() {
        let q = Quantity::new(Number::from_f64(500.0), Unit::centimeter() / Unit::meter());
        assert_yields_quantity("5 to cm/m", &q);
        assert_yields_quantity(
            "fn f(x: Scalar) -> Scalar = x to cm/m
             f(5)",
            &q,
        );
    }
}
