use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::JsValue;
use crate::prelude::*;

#[wasm_bindgen(module = "/msdfgen_wasm.js")]
extern {
    #[wasm_bindgen(js_name="addInitializationCb")]
    pub fn on_emscripten_runtime_initialized(callback : JsValue)
        -> js_sys::Promise;

    #[wasm_bindgen(js_name="isInitialized")]
    pub fn is_emscripten_runtime_initialized() -> bool;

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

// ==========================
// === F32ArrayMemoryView ===
// ==========================

pub struct F32ArrayMemoryView {
    begin_address : usize,
    end_address   : usize
}

pub struct F32ArrayMemoryViewIterator {
    next_read_address : usize,
    end_address       : usize
}

impl F32ArrayMemoryView {
    pub fn new(address : usize, size : usize) -> F32ArrayMemoryView {
        let size_in_bytes =
            size * emscripten_data_types::FLOAT_SIZE_IN_BYTES;
        F32ArrayMemoryView {
            begin_address : address,
            end_address   : address + size_in_bytes
        }
    }

    pub fn iter(&self) -> F32ArrayMemoryViewIterator {
        F32ArrayMemoryViewIterator {
            next_read_address : self.begin_address,
            end_address       : self.end_address
        }
    }
}

impl IntoIterator for F32ArrayMemoryView {
    type Item = f32;
    type IntoIter = F32ArrayMemoryViewIterator;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl Iterator for F32ArrayMemoryViewIterator {
    type Item = f32;

    fn next(&mut self) -> Option<Self::Item> {
        let has_element = self.next_read_address < self.end_address;
        has_element.and_option_from(|| {
            let ret_val = emscripten_get_value_from_memory(
                self.next_read_address,
                emscripten_data_types::FLOAT);
            self.next_read_address += emscripten_data_types::FLOAT_SIZE_IN_BYTES;
            Some(ret_val.as_f64().unwrap() as f32)
        })
    }
}