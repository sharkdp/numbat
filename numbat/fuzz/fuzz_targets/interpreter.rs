#![no_main]

use libfuzzer_sys::fuzz_target;
use numbat::{self, resolver::CodeSource};

fuzz_target!(|data: &[u8]| {
    if let Ok(code) = std::str::from_utf8(data) {
        let mut ctx = numbat::Context::new_without_importer();
        let _ = ctx.interpret(code, CodeSource::Text);
    }
});
