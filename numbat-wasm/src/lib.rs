mod jquery_terminal_formatter;
mod utils;
mod wasm_importer;

use jquery_terminal_formatter::{jt_format, JqueryTerminalFormatter};
use numbat::markup::Formatter;
use numbat::pretty_print::PrettyPrint;
use numbat::resolver::CodeSource;
use numbat::{Context, InterpreterResult};

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
        let fmt = JqueryTerminalFormatter {};

        match self.ctx.interpret(&code, CodeSource::Text) {
            Ok((_, result)) => match result {
                InterpreterResult::Value(q) => fmt.format(&q.pretty_print(), true),
                InterpreterResult::Continue => "".into(),
                InterpreterResult::Exit(_) => jt_format("error", "Error!".into()),
            },
            Err(e) => format!("{:#}", e),
        }
    }
}
