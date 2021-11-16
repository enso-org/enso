//! The whole content of this file consists of parts that were copied from `js_sys` just to modify
//! `new_with_args` such that it catches syntax errors. This is supposed to be temporary solution.
//! I opened a GitHub issue suggesting that this will be included in wasm-bindgen
//! (https://github.com/rustwasm/wasm-bindgen/issues/2496).
//! A PR with a suggested change to wasm-bindgen can be found here:
//! https://github.com/rustwasm/wasm-bindgen/pull/2497

use js_sys::*;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(extends = Object, is_type_of = JsValue::is_function)]
    #[derive(Clone, Debug, PartialEq, Eq)]
    pub type Function;

    /// The `Function` constructor creates a new `Function` object. Calling the
    /// constructor directly can create functions dynamically, but suffers from
    /// security and similar (but far less significant) performance issues
    /// similar to `eval`. However, unlike `eval`, the `Function` constructor
    /// allows executing code in the global scope, prompting better programming
    /// habits and allowing for more efficient code minification.
    #[allow(unsafe_code)]
    #[wasm_bindgen(constructor, catch)]
    pub fn new_with_args(args: &str, body: &str) -> Result<Function, JsValue>;

    /// The `call()` method calls a function with a given this value and
    /// arguments provided individually.
    #[allow(unsafe_code)]
    #[wasm_bindgen(method, catch, js_name = call)]
    pub fn call1(this: &Function, context: &JsValue, arg1: &JsValue) -> Result<JsValue, JsValue>;
}
