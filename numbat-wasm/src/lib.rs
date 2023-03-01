mod utils;

use numbat::{Numbat, InterpreterResult};

use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub fn interpret(code: &str) -> String {
    utils::set_panic_hook();

    let mut numbat = Numbat::new(true);
    match numbat.interpret(&code) {
        Ok((_, result)) => match result {
            InterpreterResult::Quantity(q) => format!("{}", q),
            InterpreterResult::Continue => "Nothing to show".into(),
            InterpreterResult::Exit(_) => "Error!".into(),
        },
        Err(e) => format!("{:#}", e)
    }
}
