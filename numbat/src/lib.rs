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
pub mod resolver;
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
use prefix_transformer::Transformer;
use resolver::CodeSource;
use resolver::ModuleImporter;
use resolver::NullImporter;
use resolver::Resolver;
use resolver::ResolverError;
use thiserror::Error;
use typechecker::{TypeCheckError, TypeChecker};

use ast::Statement;
pub use interpreter::ExitStatus;
pub use interpreter::InterpreterResult;
pub use parser::ParseError;

pub type Diagnostic = codespan_reporting::diagnostic::Diagnostic<usize>;

#[derive(Debug, Error)]
pub enum NumbatError {
    #[error("{0}")]
    ResolverError(ResolverError),
    #[error("{0}")]
    NameResolutionError(NameResolutionError),
    #[error("{0}")]
    TypeCheckError(TypeCheckError),
    #[error("{0}")]
    RuntimeError(RuntimeError),
}

type Result<T> = std::result::Result<T, NumbatError>;

pub struct Context {
    prefix_transformer: Transformer,
    typechecker: TypeChecker,
    interpreter: BytecodeInterpreter,
    resolver: Resolver,
}

impl Context {
    pub fn new(module_importer: impl ModuleImporter + 'static) -> Self {
        Context {
            prefix_transformer: Transformer::new(),
            typechecker: TypeChecker::default(),
            interpreter: BytecodeInterpreter::new(),
            resolver: Resolver::new(module_importer),
        }
    }

    pub fn new_without_importer() -> Self {
        Self::new(NullImporter::new())
    }

    pub fn set_debug(&mut self, activate: bool) {
        self.interpreter.set_debug(activate);
    }

    pub fn variable_names(&self) -> &[String] {
        &self.prefix_transformer.variable_names
    }

    pub fn function_names(&self) -> &[String] {
        &self.prefix_transformer.function_names
    }

    pub fn unit_names(&self) -> &[Vec<String>] {
        &self.prefix_transformer.unit_names
    }

    pub fn dimension_names(&self) -> &[String] {
        &self.prefix_transformer.dimension_names
    }

    pub fn interpret(
        &mut self,
        code: &str,
        code_source: CodeSource,
    ) -> Result<(Vec<Statement>, InterpreterResult)> {
        let statements = self
            .resolver
            .resolve(code, code_source)
            .map_err(NumbatError::ResolverError)?;

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

    pub fn print_diagnostic(&self, diagnostic: &Diagnostic) {
        use codespan_reporting::term::{
            self,
            termcolor::{ColorChoice, StandardStream},
            Config,
        };

        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = Config::default();

        term::emit(
            &mut writer.lock(),
            &config,
            &self.resolver.files,
            &diagnostic,
        )
        .unwrap();
    }
}
