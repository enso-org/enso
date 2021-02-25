//! Tests for cases where parser currently fails. They are ignored, should be removed and placed
//! elsewhere, as the parser gets fixed.

#![feature(matches_macro)]

use wasm_bindgen_test::wasm_bindgen_test;
use wasm_bindgen_test::wasm_bindgen_test_configure;

wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test]
fn missing_macro_segment() {
    // TODO: should succeed
    //  https://github.com/enso-org/enso/issues/256
    assert!(parser::Parser::new_or_panic().parse_line("-> a").is_err());
}

#[wasm_bindgen_test]
fn nested_macros() {
    // TODO: should succeed
    //   https://github.com/enso-org/enso/issues/256 or https://github.com/enso-org/enso/issues/343
    assert!(parser::Parser::new_or_panic().parse_line("(a -> b) -> c").is_err());
}

#[wasm_bindgen_test]
fn extension_operator_methods() {
    let ast = parser::Parser::new_or_panic().parse_line("Int.+").unwrap();

    use ast::*;
    if let Shape::Infix(Infix {larg:_larg,loff:_loff,opr,roff:_roff,rarg}, ..) = ast.shape() {
        if let Shape::Opr(Opr{..}) = opr.shape() {
            // TODO: should be Opr(+). https://github.com/enso-org/enso/issues/565
            if let Shape::Var(Var{..}) = rarg.shape() {
                return;
            }
        }
    }
    panic!("Should have matched into return.");
}
