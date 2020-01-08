#![allow(missing_docs)]

mod internal;
pub mod emscripten_data;
pub mod test_utils;

pub use basegl_prelude as prelude;
use internal::*;

use emscripten_data::ArrayMemoryView;
use js_sys::Uint8Array;
use wasm_bindgen::JsValue;
use wasm_bindgen::prelude::Closure;

// ======================
// === Initialization ===
// ======================

/// Add initialization callback
///
/// The callback passed as argument will be called once the msdfgen libirary
/// will be initialized.
pub fn run_once_initialized<F>(callback:F)
where F : 'static + FnOnce() {
    if is_emscripten_runtime_initialized() {
        callback()
    } else {
        let js_callback = Closure::once_into_js(callback);
        on_emscripten_runtime_initialized(js_callback);
    }
}

// ============
// === Font ===
// ============

#[derive(Debug)]
pub struct Font {
    pub handle: JsValue
}

impl Font {
    /// Loading font from memory
    ///
    /// Loads font from a any format which freetype library can handle.
    /// See [https://www.freetype.org/freetype2/docs/index.html] for reference.
    pub fn load_from_memory(data:&[u8]) -> Self {
        let array_type_js  = JsValue::from_str(ccall_types::ARRAY);
        let number_type_js = JsValue::from_str(ccall_types::NUMBER);
        let data_js_array  = Uint8Array::from(data);
        let data_js        = JsValue::from(data_js_array);
        let data_size_js   = JsValue::from_f64(data.len() as f64);

        let function_name  = "msdfgen_loadFontMemory";
        let return_type    = ccall_types::NUMBER;
        let param_types    = js_sys::Array::of2(&array_type_js,&number_type_js);
        let params         = js_sys::Array::of2(&data_js,&data_size_js);

        let handle         = emscripten_call_function(function_name,return_type,param_types,params);
        Font { handle }
    }

    pub fn retrieve_kerning(&self, left:char, right:char) -> f64 {
        let left_unicode  = left as u32;
        let right_unicode = right as u32;
        msdfgen_get_kerning(self.handle.clone(),left_unicode,right_unicode)
    }

    pub fn mock_font() -> Font {
        Font {
            handle : JsValue::from_f64(0.0)
        }
    }
}

impl Drop for Font {
    fn drop(&mut self) {
        msdfgen_free_font(self.handle.clone())
    }
}

// =====================================================
// === Mutlichannel signed distance field generation ===
// =====================================================

/// Parameters of MSDF generation
///
/// The structure gathering MSDF generation parameters meant to be same for all
/// rendered glyphs
pub struct MsdfParameters {
    pub width                         : usize,
    pub height                        : usize,
    pub edge_coloring_angle_threshold : f64,
    pub range                         : f64,
    pub max_scale                     : f64,
    pub edge_threshold                : f64,
    pub overlap_support               : bool
}

pub struct MultichannelSignedDistanceField {
    handle          : JsValue,
    pub advance     : f64,
    pub translation : nalgebra::Vector2<f64>,
    pub scale       : nalgebra::Vector2<f64>,
    pub data        : ArrayMemoryView<f32>
}

impl MultichannelSignedDistanceField {
    pub const CHANNELS_COUNT : usize = 3;

    /// Generate Mutlichannel Signed Distance Field (MSDF) for one glyph
    ///
    /// For more information about MSDF see
    /// [https://github.com/Chlumsky/msdfgen].
    pub fn generate(font:&Font, unicode:u32, params:&MsdfParameters)
    -> MultichannelSignedDistanceField {
        let handle = msdfgen_generate_msdf
            ( params.width
            , params.height
            , font.handle.clone()
            , unicode
            , params.edge_coloring_angle_threshold
            , params.range,params.max_scale
            , params.edge_threshold
            , params.overlap_support
            );
        let advance     = msdfgen_result_get_advance(handle.clone());
        let translation = Self::translation(&handle);
        let scale       = Self::scale(&handle);
        let data_adress = msdfgen_result_get_msdf_data(handle.clone());
        let data_size   = params.width * params.height * Self::CHANNELS_COUNT;
        let data        = ArrayMemoryView::new(data_adress,data_size);
        MultichannelSignedDistanceField{handle,advance,translation,scale,data}
    }

    const DIMENSIONS: usize = 2;

    fn translation(handle : &JsValue) -> nalgebra::Vector2<f64> {
        let address     = msdfgen_result_get_translation(handle.clone());
        let view        = ArrayMemoryView::new(address,Self::DIMENSIONS);
        let mut iter    = view.iter();
        let translate_x = iter.next().unwrap();
        let translate_y = iter.next().unwrap();
        nalgebra::Vector2::new(translate_x,translate_y)
    }

    fn scale(handle : &JsValue) -> nalgebra::Vector2<f64> {
        let address  = msdfgen_result_get_scale(handle.clone());
        let view     = ArrayMemoryView::new(address,Self::DIMENSIONS);
        let mut iter = view.iter();
        let scale_x  = iter.next().unwrap();
        let scale_y  = iter.next().unwrap();
        nalgebra::Vector2::new(scale_x, scale_y)
    }

    pub fn mock_results() -> MultichannelSignedDistanceField {
        MultichannelSignedDistanceField {
            handle      : JsValue::from_f64(0.0),
            advance     : 0.0,
            translation : nalgebra::Vector2::new(0.0, 0.0),
            scale       : nalgebra::Vector2::new(1.0, 1.0),
            data        : ArrayMemoryView::empty()
        }
    }
}

impl Drop for MultichannelSignedDistanceField {
    fn drop(&mut self) {
        msdfgen_free_result(self.handle.clone());
    }
}


// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use crate::*;
    use wasm_bindgen_test::{wasm_bindgen_test, wasm_bindgen_test_configure};
    use basegl_core_embedded_fonts::EmbeddedFonts;
    use std::future::Future;
    use test_utils::TestAfterInit;
    use nalgebra::Vector2;

    wasm_bindgen_test_configure!(run_in_browser);

    #[wasm_bindgen_test(async)]
    fn generate_msdf_for_capital_a() -> impl Future<Output=()> {
        TestAfterInit::schedule(|| {
            // given
            let font_base = EmbeddedFonts::create_and_fill();
            let font = Font::load_from_memory(
                font_base.font_data_by_name.get("DejaVuSansMono-Bold").unwrap()
            );
            let params = MsdfParameters {
                width                         : 32,
                height                        : 32,
                edge_coloring_angle_threshold : 3.0,
                range                         : 2.0,
                max_scale                     : 2.0,
                edge_threshold                : 1.001,
                overlap_support               : true
            };
            // when
            let msdf = MultichannelSignedDistanceField::generate(
                &font,
                'A' as u32,
                &params,
            );
            // then
            let data : Vec<f32> = msdf.data.iter().collect();
            assert_eq!(-0.9408906                , data[0]); // Note [asserts]
            assert_eq!(0.2                       , data[10]);
            assert_eq!(-4.3035655                , data[data.len()-1]);
            assert_eq!(Vector2::new(3.03125, 1.0), msdf.translation);
            assert_eq!(Vector2::new(1.25, 1.25)  , msdf.scale);
            assert_eq!(19.265625                 , msdf.advance);
        })
    }

    /* Note [asserts]
     *
     * we're checking rust - js interface only, so there is no need to check
     * all values
     */
}
