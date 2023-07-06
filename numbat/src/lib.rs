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
mod resolver;
mod span;
mod tokenizer;
mod typechecker;
mod typed_ast;
mod unit;
mod unit_registry;
mod vm;

use std::path::Path;
use std::path::PathBuf;

use bytecode_interpreter::BytecodeInterpreter;
use interpreter::{Interpreter, RuntimeError};
use name_resolution::NameResolutionError;
use prefix_transformer::Transformer;
use resolver::FileSystemImporter;
use resolver::NullImporter;
use resolver::Resolver;
use resolver::ResolverError;
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
    ResolverError(ResolverError),
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
    module_paths: Vec<PathBuf>,
}

impl Context {
    pub fn new_without_prelude(debug: bool) -> Self {
        Context {
            prefix_transformer: Transformer::new(),
            typechecker: TypeChecker::default(),
            interpreter: BytecodeInterpreter::new(debug),
            module_paths: vec![],
        }
    }

    pub fn new(debug: bool) -> Self {
        let mut context = Self::new_without_prelude(debug);

        let module_path = Path::new("/home/ped1st/software/numbat/modules");
        context.add_module_path(module_path); // TODO

        assert!(context
            .interpret(&std::fs::read_to_string(module_path.join("prelude.nbt")).unwrap())
            .expect("Error while running prelude")
            .1
            .is_success()); // TODO: error handling
        context
    }

    pub fn add_module_path<P: AsRef<Path>>(&mut self, path: P) {
        self.module_paths.push(path.as_ref().to_path_buf());
    }

    pub fn interpret(&mut self, code: &str) -> Result<(Vec<Statement>, InterpreterResult)> {
        let importer = FileSystemImporter::new(&self.module_paths[..]);
        let resolver = Resolver::new(&importer);

        let statements = resolver.resolve(code).map_err(|e| match e {
            ResolverError::ParseError(e) => NumbatError::ParseError(e),
            e => NumbatError::ResolverError(e),
        })?;

        let prefix_transformer_old = self.prefix_transformer.clone();

        let transformed_statements = self
            .prefix_transformer
            .transform(statements)
            .map_err(NumbatError::NameResolutionError)?;
        let result = self
            .typechecker
            .check_statements(transformed_statements.clone()) // TODO(minor): get rid of clone?
            .map_err(NumbatError::TypeCheckError);

        if result.is_err() {
            // Reset the state of the prefix transformer to what we had before. This is necessary
            // for REPL use cases where we want to back track from type-check errors.
            // For example:
            //
            //     >>> let x: Length = 1s      # <-- here we register the name 'x' before type checking
            //     Type check error: Incompatible dimensions in variable declaration:
            //         specified dimension: Length
            //         actual dimension: Time
            //     >>> let x: Length = 1m      # <-- here we want to use the name 'x' again
            //
            self.prefix_transformer = prefix_transformer_old;
        }

        let typed_statements = result?;

        let result = self
            .interpreter
            .interpret_statements(&typed_statements)
            .map_err(NumbatError::RuntimeError)?;

        Ok((transformed_statements, result))
    }
}
