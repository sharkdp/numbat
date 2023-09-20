mod jquery_terminal_formatter;
mod utils;
mod wasm_importer;

use std::sync::{Arc, Mutex};

use jquery_terminal_formatter::{jt_format, JqueryTerminalFormatter};
use numbat::markup as m;
use numbat::markup::Formatter;
use numbat::pretty_print::PrettyPrint;
use numbat::resolver::CodeSource;
use numbat::{Context, InterpreterResult, InterpreterSettings};

use wasm_bindgen::prelude::*;

use crate::wasm_importer::WasmImporter;

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
impl Numbat {
    pub fn new() -> Self {
        let mut ctx = Context::new(WasmImporter {});
        let _ = ctx.interpret("use prelude", CodeSource::Internal).unwrap();
        Numbat { ctx }
    }

    pub fn interpret(&mut self, code: &str) -> String {
        let mut output = String::new();

        let fmt = JqueryTerminalFormatter {};

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
            Ok((_, result)) => {
                for content in to_be_printed.lock().unwrap().iter() {
                    output.push_str(&fmt.format(content, true));
                }

                match result {
                    InterpreterResult::Value(q) => {
                        output.push_str(&fmt.format(&q.pretty_print(), true))
                    }
                    InterpreterResult::Continue => {}
                    InterpreterResult::Exit(_) => {
                        output.push_str(&jt_format("error", "Error!".into()))
                    }
                }

                output
            }
            Err(e) => format!("{:#}", e),
        }
    }
}
