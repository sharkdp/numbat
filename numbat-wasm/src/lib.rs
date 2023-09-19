mod utils;

use numbat::resolver::{CodeSource, NullImporter};
use numbat::{Context, InterpreterResult};

use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub fn interpret(code: &str) -> String {
    utils::set_panic_hook();

    let mut numbat = Context::new(NullImporter {});
    match numbat.interpret(&code, CodeSource::Text) {
        Ok((_, result)) => match result {
            InterpreterResult::Value(q) => format!("{}", q),
            InterpreterResult::Continue => "Nothing to show".into(),
            InterpreterResult::Exit(_) => "Error!".into(),
        },
        Err(e) => format!("{:#}", e),
    }
}
