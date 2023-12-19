#![cfg(target_arch = "wasm32")]

use numbat_wasm::{FormatType, Numbat};
use wasm_bindgen_test::*;

wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test]
fn basic() {
    let mut numbat = Numbat::new(false, false, FormatType::Html);
    assert_eq!(
        numbat.interpret("2 + 3 * 4").output().trim(),
        r#"<span class="numbat-operator">=</span> <span class="numbat-value">14</span>"#
    );
}
