#![cfg(target_arch = "wasm32")]

use wasm_bindgen_test::*;
use insect_wasm::interpret;

wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test]
fn basic() {
    assert_eq!(interpret("2 + 3 * 4"), "14.000000 ");
}
