// === Non-Standard Linter Configuration ===
#![allow(unsafe_code)]
#![allow(clippy::too_many_arguments)]

use enso_web as web;



// ================
// === Bindings ===
// ================

macro_rules! define_bindings {
    ($( $(#$meta:tt)* pub fn $fn:ident $args:tt $(-> $ret:ty)?; )*) => {
        #[cfg(not(target_arch = "wasm32"))]
        #[allow(unused_variables)]
        mod bindings {
            use super::*;
            $( define_native_binding! { $(#$meta)* pub fn $fn $args $(-> $ret)?; } )*
        }

        #[cfg(target_arch = "wasm32")]
        mod bindings {
            use super::*;
            use wasm_bindgen::prelude::wasm_bindgen;
            #[wasm_bindgen(module = "/msdfgen_wasm.js")]
            extern "C" {
                $( $(#$meta)* pub fn $fn $args $(-> $ret)?; )*
            }
        }

        pub use bindings::*;
    };
}

#[allow(unused_macros)]
macro_rules! define_native_binding {
    ( $(#$meta:tt)* pub fn $fn:ident $args:tt; ) => {
        pub fn $fn $args {}
    };
    ( $(#$meta:tt)* pub fn $fn:ident $args:tt -> $ret:ty; ) => {
        pub fn $fn $args -> $ret { Default::default() }
    };
}

define_bindings! {
    #[wasm_bindgen(js_name = "addInitializationCb")]
    pub fn on_emscripten_runtime_initialized(callback: web::JsValue);

    #[wasm_bindgen(js_name = "isInitialized")]
    pub fn is_emscripten_runtime_initialized() -> bool;

    #[wasm_bindgen(js_name = "ccall")]
    pub fn emscripten_call_function(
        name: &str,
        return_type: &str,
        types: web::Array,
        values: web::Array,
    ) -> web::JsValue;

    #[wasm_bindgen(js_name = "getValue")]
    pub fn emscripten_get_value_from_memory(address: usize, a_type: &str) -> web::JsValue;

    #[wasm_bindgen(js_name = "_msdfgen_getKerning")]
    pub fn msdfgen_get_kerning
        (font_handle: web::JsValue, left_unicode: u32, right_unicode: u32) -> f64;

    // Actually, this method returns bool, but Emscripten does not translate it to JavaScript
    // boolean type, so we read it here as usize. The 0 value means false, any other means true.
    #[wasm_bindgen(js_name = "_msdfgen_setVariationAxis")]
    pub fn msdfgen_set_variation_axis
        (font_handle: web::JsValue, name: u32, coordinate: f64) -> usize;

    #[wasm_bindgen(js_name = "_msdfgen_generateAutoframedMSDF")]
    pub fn msdfgen_generate_msdf(
        width: usize,
        height: usize,
        font_handle: web::JsValue,
        unicode: u32,
        edge_coloring_angle_threshold: f64,
        range: f64,
        max_scale: f64,
        edge_threshold: f64,
        overlap_support: bool,
    ) -> web::JsValue;

    #[wasm_bindgen(js_name = "_msdfgen_generateAutoframedMSDFByIndex")]
    pub fn msdfgen_generate_msdf_by_index(
        width: usize,
        height: usize,
        font_handle: web::JsValue,
        index: usize,
        edge_coloring_angle_threshold: f64,
        range: f64,
        max_scale: f64,
        edge_threshold: f64,
        overlap_support: bool,
    ) -> web::JsValue;

    #[wasm_bindgen(js_name = "_msdfgen_result_getMSDFData")]
    pub fn msdfgen_result_get_msdf_data(result_handle: web::JsValue) -> usize;

    #[wasm_bindgen(js_name = "_msdfgen_result_getAdvance")]
    pub fn msdfgen_result_get_advance(result_handle: web::JsValue) -> f64;

    #[wasm_bindgen(js_name = "_msdfgen_result_getTranslation")]
    pub fn msdfgen_result_get_translation(result_handle: web::JsValue) -> usize;

    #[wasm_bindgen(js_name = "_msdfgen_result_getScale")]
    pub fn msdfgen_result_get_scale(result_handle: web::JsValue) -> usize;

    #[wasm_bindgen(js_name = "_msdfgen_freeResult")]
    pub fn msdfgen_free_result(result_handle: web::JsValue);

    #[wasm_bindgen(js_name = "_msdfgen_freeFont")]
    pub fn msdfgen_free_font(font_handle: web::JsValue);
}

pub mod ccall_types {
    pub const ARRAY: &str = "array";
    pub const NUMBER: &str = "number";
}
