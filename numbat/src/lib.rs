mod arithmetic;
mod ast;
mod bytecode_interpreter;
mod currency;
mod decorator;
pub mod diagnostic;
mod dimension;
mod ffi;
mod gamma;
mod interpreter;
pub mod markup;
mod math;
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
mod suggestion;
mod tokenizer;
mod typechecker;
mod typed_ast;
mod unit;
mod unit_registry;
pub mod value;
mod vm;

use bytecode_interpreter::BytecodeInterpreter;
use currency::ExchangeRatesCache;
use diagnostic::ErrorDiagnostic;
use dimension::DimensionRegistry;
use interpreter::Interpreter;
use prefix_transformer::Transformer;
use registry::BaseRepresentationFactor;
use resolver::CodeSource;
use resolver::ModuleImporter;
use resolver::NullImporter;
use resolver::Resolver;
use resolver::ResolverError;
use thiserror::Error;
use typechecker::{TypeCheckError, TypeChecker};

pub use diagnostic::Diagnostic;
pub use interpreter::ExitStatus;
pub use interpreter::InterpreterResult;
pub use interpreter::InterpreterSettings;
pub use interpreter::RuntimeError;
pub use name_resolution::NameResolutionError;
pub use parser::ParseError;
pub use typed_ast::Statement;

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
    pub fn new(module_importer: impl ModuleImporter + Send + 'static) -> Self {
        Context {
            prefix_transformer: Transformer::new(),
            typechecker: TypeChecker::default(),
            interpreter: BytecodeInterpreter::new(),
            resolver: Resolver::new(module_importer),
        }
    }

    pub fn new_without_importer() -> Self {
        Self::new(NullImporter::default())
    }

    pub fn set_debug(&mut self, activate: bool) {
        self.interpreter.set_debug(activate);
    }

    /// Fill the currency exchange rate cache. This call is blocking.
    pub fn fetch_exchange_rates() {
        let cache = ExchangeRatesCache::new();
        let _unused = cache.fetch();
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

    pub fn dimension_registry(&self) -> &DimensionRegistry {
        self.typechecker.registry()
    }

    pub fn base_units(&self) -> impl Iterator<Item = String> + '_ {
        self.interpreter
            .get_unit_registry()
            .inner
            .iter_base_entries()
    }

    pub fn unit_representations(&self) -> impl Iterator<Item = (String, Vec<(String, i128)>)> + '_ {
        let registry = self.interpreter.get_unit_registry();
        registry.inner.iter_derived_entries().map(|unit_name| {
            let derived_from = registry
                .inner
                .get_base_representation_for_name(&unit_name)
                .unwrap()
                .iter()
                .map(|BaseRepresentationFactor(name, exp)| (name.clone(), exp.to_integer())) // TODO: check if to_integer can fail here
                .collect();
            (unit_name, derived_from)
        })
    }

    pub fn interpret(
        &mut self,
        code: &str,
        code_source: CodeSource,
    ) -> Result<(Vec<typed_ast::Statement>, InterpreterResult)> {
        self.interpret_with_settings(&mut InterpreterSettings::default(), code, code_source)
    }

    pub fn interpret_with_settings(
        &mut self,
        settings: &mut InterpreterSettings,
        code: &str,
        code_source: CodeSource,
    ) -> Result<(Vec<typed_ast::Statement>, InterpreterResult)> {
        let statements = self
            .resolver
            .resolve(code, code_source)
            .map_err(NumbatError::ResolverError)?;

        let prefix_transformer_old = self.prefix_transformer.clone();

        let result = self
            .prefix_transformer
            .transform(statements)
            .map_err(NumbatError::NameResolutionError);

        if result.is_err() {
            // Reset the state of the prefix transformer to what we had before. This is necessary
            // for REPL use cases where we want to back track from type-check errors.
            // For example:
            //
            //     >>> fn f(h) = 1
            //     error: identifier clash in definition
            //         …
            //     >>> fn f(h_) = 1     # <-- here we want to use 'f' again
            //
            self.prefix_transformer = prefix_transformer_old.clone();
        }

        let transformed_statements = result?;

        let typechecker_old = self.typechecker.clone();

        let result = self
            .typechecker
            .check_statements(transformed_statements)
            .map_err(NumbatError::TypeCheckError);

        if result.is_err() {
            // Reset the state of the prefix transformer to what we had before. This is necessary
            // for REPL use cases where we want to back track from type-check errors.
            // For example:
            //
            //     >>> let x: Length = 1s      # <-- here we register the name 'x' before type checking
            //     Type check error: Incompatible dimensions in variable definition:
            //         specified dimension: Length
            //         actual dimension: Time
            //     >>> let x: Length = 1m      # <-- here we want to use the name 'x' again
            //
            self.prefix_transformer = prefix_transformer_old.clone();
            self.typechecker = typechecker_old.clone();
        }

        let typed_statements = result?;

        let result = self
            .interpreter
            .interpret_statements(settings, &typed_statements);

        if result.is_err() {
            // Similar to above: we need to reset the state of the typechecker and the prefix transformer
            // here for REPL use cases like:
            //
            //    >>> let q = 1 / 0
            //    error: runtime error
            //     = Division by zero
            //
            //    -> 'q' should not be defined, so 'q' properly leads to a "unknown identifier" error
            //       and another 'let q = …' works as intended.
            //
            self.prefix_transformer = prefix_transformer_old;
            self.typechecker = typechecker_old;
        }

        let result = result.map_err(NumbatError::RuntimeError)?;

        Ok((typed_statements, result))
    }

    pub fn print_diagnostic(&self, error: impl ErrorDiagnostic) {
        use codespan_reporting::term::{
            self,
            termcolor::{ColorChoice, StandardStream},
            Config,
        };

        let writer = StandardStream::stderr(ColorChoice::Auto);
        let config = Config::default();

        term::emit(
            &mut writer.lock(),
            &config,
            &self.resolver.files,
            &error.diagnostic(),
        )
        .unwrap();
    }
}
