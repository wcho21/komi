use komi_wasm::execute;
use wasm_bindgen_test::*;

#[wasm_bindgen_test]
fn test_single_number_literal() {
    let input = "+12.25";
    let output = execute(input);
    assert_eq!(output, "komi v1 ok 12.25");
}
