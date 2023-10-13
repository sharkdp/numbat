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
pub mod keywords;
pub mod markup;
mod math;
pub mod module_importer;
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
use keywords::KEYWORDS;
use markup as m;
use markup::Markup;
use module_importer::{ModuleImporter, NullImporter};
use prefix_transformer::Transformer;
use registry::BaseRepresentationFactor;
use resolver::CodeSource;
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
pub use typed_ast::Type;

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

#[derive(Clone)]
pub struct Context {
    prefix_transformer: Transformer,
    typechecker: TypeChecker,
    interpreter: BytecodeInterpreter,
    resolver: Resolver,
    load_currency_module_on_demand: bool,
}

impl Context {
    pub fn new(module_importer: impl ModuleImporter + Send + Sync + 'static) -> Self {
        Context {
            prefix_transformer: Transformer::new(),
            typechecker: TypeChecker::default(),
            interpreter: BytecodeInterpreter::new(),
            resolver: Resolver::new(module_importer),
            load_currency_module_on_demand: false,
        }
    }

    pub fn new_without_importer() -> Self {
        Self::new(NullImporter::default())
    }

    pub fn set_debug(&mut self, activate: bool) {
        self.interpreter.set_debug(activate);
    }

    pub fn load_currency_module_on_demand(&mut self, yes: bool) {
        self.load_currency_module_on_demand = yes;
    }

    /// Fill the currency exchange rate cache. This call is blocking.
    pub fn prefetch_exchange_rates() {
        let _unused = ExchangeRatesCache::fetch();
    }

    pub fn set_exchange_rates(xml_content: &str) {
        ExchangeRatesCache::set_from_xml(xml_content);
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

    pub fn print_environment(&self) -> Markup {
        let mut functions = Vec::from(self.function_names());
        functions.sort();
        let mut dimensions = Vec::from(self.dimension_names());
        dimensions.sort();
        let mut units = Vec::from(self.unit_names());
        units.sort();
        let mut variables = Vec::from(self.variable_names());
        variables.sort();

        let mut output = m::empty();

        output += m::text("List of functions:") + m::nl();
        for function in functions {
            output += m::whitespace("  ") + m::identifier(function) + m::nl();
        }
        output += m::nl();

        output += m::text("List of dimensions:") + m::nl();
        for dimension in dimensions {
            output += m::whitespace("  ") + m::type_identifier(dimension) + m::nl();
        }
        output += m::nl();

        output += m::text("List of units:") + m::nl();
        for unit_names in units {
            output += m::whitespace("  ")
                + itertools::intersperse(unit_names.iter().map(|n| m::unit(n)), m::text(", "))
                    .sum()
                + m::nl();
        }
        output += m::nl();

        output += m::text("List of variables:") + m::nl();
        for variable in variables {
            output += m::whitespace("  ") + m::identifier(variable) + m::nl();
        }

        output
    }

    pub fn get_completions_for<'a>(&self, word_part: &'a str) -> impl Iterator<Item = String> + 'a {
        let mut words: Vec<_> = KEYWORDS.iter().map(|k| k.to_string()).collect();

        {
            for variable in self.variable_names() {
                words.push(variable.clone());
            }

            for function in self.function_names() {
                words.push(format!("{}(", function));
            }

            for dimension in self.dimension_names() {
                words.push(dimension.clone());
            }

            for unit_names in self.unit_names() {
                for unit in unit_names {
                    words.push(unit.clone());
                }
            }
        }

        words.sort();
        words.dedup();

        words.into_iter().filter(move |w| w.starts_with(word_part))
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

    pub fn resolver(&self) -> &Resolver {
        &self.resolver
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
            .resolve(code, code_source.clone())
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

            if self.load_currency_module_on_demand {
                if let Err(NumbatError::TypeCheckError(TypeCheckError::UnknownIdentifier(
                    _,
                    identifier,
                    _,
                ))) = &result
                {
                    // TODO: maybe we can somehow load this list of identifiers from units::currencies?
                    const CURRENCY_IDENTIFIERS: &[&str] = &[
                        "$",
                        "USD",
                        "dollar",
                        "dollars",
                        "A$",
                        "AUD",
                        "australian_dollar",
                        "australian_dollars",
                        "C$",
                        "CAD",
                        "canadian_dollar",
                        "canadian_dollars",
                        "CHF",
                        "swiss_franc",
                        "swiss_francs",
                        "CNY",
                        "renminbi",
                        "元",
                        "EUR",
                        "euro",
                        "euros",
                        "€",
                        "GBP",
                        "british_pound",
                        "pound_sterling",
                        "£",
                        "JPY",
                        "yen",
                        "yens",
                        "¥",
                        "円",
                    ];
                    if CURRENCY_IDENTIFIERS.contains(&identifier.as_str()) {
                        let mut no_print_settings = InterpreterSettings {
                            print_fn: Box::new(
                                move |_: &m::Markup| { // ignore any print statements when loading this module asynchronously
                                },
                            ),
                        };

                        // We also call this from a thread at program startup, so if a user only starts
                        // to use currencies later on, this will already be available and return immediately.
                        // Otherwise, we fetch it now and make sure to block on this call.
                        {
                            let erc = ExchangeRatesCache::fetch();

                            if erc.is_none() {
                                return Err(NumbatError::RuntimeError(
                                    RuntimeError::CouldNotLoadExchangeRates,
                                ));
                            }
                        }

                        self.interpret_with_settings(
                            &mut no_print_settings,
                            "use units::currencies",
                            CodeSource::Internal,
                        )
                        .ok();

                        // Now we try to evaluate the user expression again:
                        return self.interpret_with_settings(settings, code, code_source);
                    }
                }
            }
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
