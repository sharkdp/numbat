mod jquery_terminal_formatter;
mod utils;

use numbat::diagnostic::ErrorDiagnostic;
use numbat::module_importer::BuiltinModuleImporter;
use std::sync::{Arc, Mutex};
use wasm_bindgen::prelude::*;

use jquery_terminal_formatter::JqueryTerminalFormatter;

use numbat::help::help_markup;
use numbat::markup::Formatter;
use numbat::pretty_print::PrettyPrint;
use numbat::resolver::CodeSource;
use numbat::{markup as m, NameResolutionError, NumbatError};
use numbat::{Context, InterpreterSettings};

use crate::jquery_terminal_formatter::JqueryTerminalWriter;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub fn setup_panic_hook() {
    utils::set_panic_hook();
}

#[wasm_bindgen]
pub struct Numbat {
    ctx: Context,
}

#[wasm_bindgen]
#[derive(Debug, Clone)]
pub struct InterpreterOutput {
    output: String,
    pub is_error: bool,
}

#[wasm_bindgen]
impl InterpreterOutput {
    #[wasm_bindgen(getter)]
    pub fn output(&self) -> String {
        self.output.clone()
    }
}

#[wasm_bindgen]
impl Numbat {
    pub fn new() -> Self {
        let mut ctx = Context::new(BuiltinModuleImporter::default());
        let _ = ctx.interpret("use prelude", CodeSource::Internal).unwrap();
        ctx.set_terminal_width(Some(84)); // terminal width with current layout
        Numbat { ctx }
    }

    pub fn set_exchange_rates(&mut self, xml_content: &str) {
        Context::set_exchange_rates(xml_content);
        let _ = self
            .ctx
            .interpret("use units::currencies", CodeSource::Internal)
            .unwrap();
    }

    fn jq_format(&self, markup: &numbat::markup::Markup, indent: bool) -> String {
        let fmt = JqueryTerminalFormatter {};
        fmt.format(&markup, indent).into()
    }

    pub fn interpret(&mut self, code: &str) -> InterpreterOutput {
        let mut output = String::new();

        let registry = self.ctx.dimension_registry().clone();

        let to_be_printed: Arc<Mutex<Vec<m::Markup>>> = Arc::new(Mutex::new(vec![]));
        let to_be_printed_c = to_be_printed.clone();
        let mut settings = InterpreterSettings {
            print_fn: Box::new(move |s: &m::Markup| {
                to_be_printed_c.lock().unwrap().push(s.clone());
            }),
        };

        match self
            .ctx
            .interpret_with_settings(&mut settings, &code, CodeSource::Text)
        {
            Ok((statements, result)) => {
                // Pretty print
                output.push_str("\n");
                for statement in &statements {
                    output.push_str(&self.jq_format(&statement.pretty_print(), true));
                    output.push_str("\n");
                }
                output.push_str("\n");

                // print(…) and type(…) results
                let to_be_printed = to_be_printed.lock().unwrap();
                for content in to_be_printed.iter() {
                    output.push_str(&self.jq_format(content, true));
                    output.push_str("\n");
                }

                let result_markup = result.to_markup(statements.last(), &registry, true);
                output.push_str(&self.jq_format(&result_markup, true));

                InterpreterOutput {
                    output,
                    is_error: false,
                }
            }
            Err(NumbatError::ResolverError(e)) => self.print_diagnostic(&e),
            Err(NumbatError::NameResolutionError(
                e @ (NameResolutionError::IdentifierClash { .. }
                | NameResolutionError::ReservedIdentifier(_)),
            )) => self.print_diagnostic(&e),
            Err(NumbatError::TypeCheckError(e)) => self.print_diagnostic(&e),
            Err(NumbatError::RuntimeError(e)) => self.print_diagnostic(&e),
        }
    }

    pub fn print_environment(&self) -> JsValue {
        self.jq_format(&self.ctx.print_environment(), false).into()
    }

    pub fn print_functions(&self) -> JsValue {
        self.jq_format(&self.ctx.print_functions(), false).into()
    }

    pub fn print_dimensions(&self) -> JsValue {
        self.jq_format(&self.ctx.print_dimensions(), false).into()
    }

    pub fn print_variables(&self) -> JsValue {
        self.jq_format(&self.ctx.print_variables(), false).into()
    }

    pub fn print_units(&self) -> JsValue {
        self.jq_format(&self.ctx.print_units(), false).into()
    }

    pub fn help(&self) -> JsValue {
        self.jq_format(&help_markup(), true).into()
    }

    pub fn get_completions_for(&self, input: &str) -> Vec<JsValue> {
        self.ctx
            .get_completions_for(input)
            .map(|s| s.trim().trim_end_matches('(').into())
            .collect()
    }

    fn print_diagnostic(&self, error: &dyn ErrorDiagnostic) -> InterpreterOutput {
        use codespan_reporting::term::{self, Config};

        let mut writer = JqueryTerminalWriter::new();
        let config = Config::default();

        let resolver = self.ctx.resolver();

        for diagnostic in error.diagnostics() {
            term::emit(&mut writer, &config, &resolver.files, &diagnostic).unwrap();
        }

        InterpreterOutput {
            output: writer.to_string(),
            is_error: true,
        }
    }
}
