use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::JsValue;

#[wasm_bindgen(module = "/msdfgen_wasm.js")]
extern {
    #[wasm_bindgen(js_name="ccall")]
    pub fn emscripten_call_function(
        name        : &str,
        return_type : &str,
        types       : js_sys::Array,
        values      : js_sys::Array
    ) -> JsValue;

    #[wasm_bindgen(js_name="getValue")]
    pub fn emscripten_get_value_from_memory(
        address: usize,
        a_type: &str
    ) -> JsValue;

    #[wasm_bindgen(js_name="_msdfgen_maxMSDFSize")]
    pub fn msdfgen_max_msdf_size() -> usize;

    #[wasm_bindgen(js_name="_msdfgen_generateMSDF")]
    pub fn msdfgen_generate_msdf(
        width                           : usize,
        height                          : usize,
        font_handle                     : JsValue,
        unicode                         : u32,
        edge_coloring_angle_threshold   : f64,
        range                           : f64,
        scale_x                         : f64,
        scale_y                         : f64,
        translate_x                     : f64,
        translate_y                     : f64,
        edge_threshold                  : f64,
        overlap_support                 : bool
    ) -> usize;

    #[wasm_bindgen(js_name="_msdfgen_freeFont")]
    pub fn msdfgen_free_font(font_handle: JsValue);
}

pub mod emscripten_data_types {
    pub const FLOAT_SIZE_IN_BYTES : usize = 4;

    pub const ARRAY  : &str = "array";
    pub const NUMBER : &str = "number";
    pub const FLOAT  : &str = "float";
}

pub fn copy_f32_data_from_msdfgen_memory(
    address        : usize,
    output         : &mut[f32],
    elements_count : usize
) {
    for (i, element) in
        output.iter_mut().enumerate().take(elements_count) {

        let offset = i * emscripten_data_types::FLOAT_SIZE_IN_BYTES;
        *element = emscripten_get_value_from_memory(
            address + offset,
            emscripten_data_types::FLOAT
        ).as_f64().unwrap() as f32;
    }
}