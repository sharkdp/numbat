mod jquery_terminal_formatter;
mod utils;

use std::sync::{Arc, Mutex};
use wasm_bindgen::prelude::*;

use numbat::buffered_writer::BufferedWriter;
use numbat::diagnostic::ErrorDiagnostic;
use numbat::help::help_markup;
use numbat::html_formatter::{HtmlFormatter, HtmlWriter};
use numbat::markup::Formatter;
use numbat::module_importer::BuiltinModuleImporter;
use numbat::pretty_print::PrettyPrint;
use numbat::resolver::CodeSource;
use numbat::{markup as m, NameResolutionError, NumbatError};
use numbat::{Context, InterpreterSettings};

use jquery_terminal_formatter::{JqueryTerminalFormatter, JqueryTerminalWriter};

#[wasm_bindgen]
pub fn setup_panic_hook() {
    utils::set_panic_hook();
}

#[wasm_bindgen]
#[derive(Debug, Clone, Copy)]
pub enum FormatType {
    JqueryTerminal,
    Html,
}

#[wasm_bindgen]
pub struct Numbat {
    ctx: Context,
    enable_pretty_printing: bool,
    format_type: FormatType,
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
    pub fn new(load_prelude: bool, enable_pretty_printing: bool, format_type: FormatType) -> Self {
        let mut ctx = Context::new(BuiltinModuleImporter::default());
        if load_prelude {
            let _ = ctx.interpret("use prelude", CodeSource::Internal).unwrap();
        }
        ctx.set_terminal_width(Some(84)); // terminal width with current layout
        Numbat {
            ctx,
            enable_pretty_printing,
            format_type,
        }
    }

    pub fn set_exchange_rates(&mut self, xml_content: &str) {
        Context::set_exchange_rates(xml_content);
        let _ = self
            .ctx
            .interpret("use units::currencies", CodeSource::Internal)
            .unwrap();
    }

    fn format(&self, markup: &numbat::markup::Markup, indent: bool) -> String {
        let fmt: Box<dyn Formatter> = match self.format_type {
            FormatType::JqueryTerminal => Box::new(JqueryTerminalFormatter {}),
            FormatType::Html => Box::new(HtmlFormatter {}),
        };
        fmt.format(markup, indent)
    }

    pub fn interpret(&mut self, code: &str) -> InterpreterOutput {
        let mut output = String::new();

        let to_be_printed: Arc<Mutex<Vec<m::Markup>>> = Arc::new(Mutex::new(vec![]));
        let to_be_printed_c = to_be_printed.clone();
        let mut settings = InterpreterSettings {
            print_fn: Box::new(move |s: &m::Markup| {
                to_be_printed_c.lock().unwrap().push(s.clone());
            }),
        };

        let nl = &self.format(&numbat::markup::nl(), false);

        let enable_indentation = match self.format_type {
            FormatType::JqueryTerminal => true,
            FormatType::Html => false,
        };

        match self
            .ctx
            .interpret_with_settings(&mut settings, code, CodeSource::Text)
            .map_err(|b| *b)
        {
            Ok((statements, result)) => {
                // Pretty print
                if self.enable_pretty_printing {
                    output.push_str(nl);
                    for statement in &statements {
                        output
                            .push_str(&self.format(&statement.pretty_print(), enable_indentation));
                        output.push_str(nl);
                    }
                    output.push_str(nl);
                }

                // print(…) and type(…) results
                let to_be_printed = to_be_printed.lock().unwrap();
                for content in to_be_printed.iter() {
                    output.push_str(&self.format(content, enable_indentation));
                    output.push_str(nl);
                }

                let result_markup = result.to_markup(
                    statements.last(),
                    &self.ctx.dimension_registry().clone(),
                    true,
                    true,
                );
                output.push_str(&self.format(&result_markup, enable_indentation));

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
        self.format(&self.ctx.print_environment(), false).into()
    }

    pub fn print_functions(&self) -> JsValue {
        self.format(&self.ctx.print_functions(), false).into()
    }

    pub fn print_dimensions(&self) -> JsValue {
        self.format(&self.ctx.print_dimensions(), false).into()
    }

    pub fn print_variables(&self) -> JsValue {
        self.format(&self.ctx.print_variables(), false).into()
    }

    pub fn print_units(&self) -> JsValue {
        self.format(&self.ctx.print_units(), false).into()
    }

    pub fn help(&self) -> JsValue {
        self.format(&help_markup(), true).into()
    }

    pub fn print_info(&mut self, keyword: &str) -> JsValue {
        let output = self.ctx.print_info_for_keyword(keyword);
        self.format(&output, true).into()
    }

    pub fn get_completions_for(&self, input: &str) -> Vec<JsValue> {
        self.ctx
            .get_completions_for(input, false)
            .map(|s| s.trim().trim_end_matches('(').into())
            .collect()
    }

    fn print_diagnostic(&self, error: &dyn ErrorDiagnostic) -> InterpreterOutput {
        use codespan_reporting::term::{self, Config};

        let mut writer: Box<dyn BufferedWriter> = match self.format_type {
            FormatType::JqueryTerminal => Box::new(JqueryTerminalWriter::new()),
            FormatType::Html => Box::new(HtmlWriter::new()),
        };
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
