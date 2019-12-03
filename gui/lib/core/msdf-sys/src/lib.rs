mod internal;
pub mod test_utils;
pub use basegl_prelude as prelude;

use internal::{
    on_emscripten_runtime_initialized,
    is_emscripten_runtime_initialized,
    emscripten_call_function,
    msdfgen_generate_msdf,
    msdfgen_free_font,
    emscripten_data_types,
    F32ArrayMemoryView,
};
use js_sys::Uint8Array;
use wasm_bindgen::JsValue;
use wasm_bindgen::prelude::Closure;
pub use vector2d::Vector2D;

// ======================
// === Initialization ===
// ======================

/// Add initialization callback
///
/// The callback passed as argument will be called once the msdfgen libirary
/// will be initialized.
pub fn run_once_initialized<F>(callback : F)
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

pub struct Font {
    pub handle: JsValue
}

impl Font {
    /// Loading font from memory
    ///
    /// Loads font from a any format which freetype library can handle.
    /// See [https://www.freetype.org/freetype2/docs/index.html] for reference.
    pub fn load_from_memory(data: &[u8]) -> Self {
        let param_types = js_sys::Array::of2(
            &JsValue::from_str(emscripten_data_types::ARRAY),
            &JsValue::from_str(emscripten_data_types::NUMBER)
        );
        let params = js_sys::Array::of2(
            &JsValue::from(Uint8Array::from(data)),
            &JsValue::from_f64(data.len() as f64)
        );
        let handle = emscripten_call_function(
            "msdfgen_loadFontMemory",
            emscripten_data_types::NUMBER,
            param_types,
            params);
        Font { handle }
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
pub struct MSDFParameters {
    pub width                         : usize,
    pub height                        : usize,
    pub edge_coloring_angle_threshold : f64,
    pub range                         : f64,
    pub edge_threshold                : f64,
    pub overlap_support               : bool
}

pub const MAX_MSDF_SIZE       : usize = 64;
pub const MSDF_CHANNELS_COUNT : usize = 3;

///// Generate Mutlichannel Signed Distance Field (MSDF) for one glyph
/////
///// For more information about MSDF see [https://github.com/Chlumsky/msdfgen].
pub fn generate_msdf<Output : Extend<f32>>(
    output    : &mut Output,
    font      : &Font,
    unicode   : u32,
    params    : &MSDFParameters,
    scale     : Vector2D<f64>,
    translate : Vector2D<f64>,
) {
    assert!(params.width  <= MAX_MSDF_SIZE);
    assert!(params.height <= MAX_MSDF_SIZE);

    let output_size = params.width * params.height * MSDF_CHANNELS_COUNT;
    let output_address = msdfgen_generate_msdf(
        params.width,
        params.height,
        font.handle.clone(),
        unicode,
        params.edge_coloring_angle_threshold,
        params.range,
        scale.x,
        scale.y,
        translate.x,
        translate.y,
        params.edge_threshold,
        params.overlap_support
    );
    let view = F32ArrayMemoryView::new(output_address, output_size);

    output.extend(view); // Note [Output variable]
}

// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use wasm_bindgen_test::{wasm_bindgen_test, wasm_bindgen_test_configure};
    use internal::msdfgen_max_msdf_size;
    use crate::*;
    use basegl_core_embedded_fonts::EmbeddedFonts;
    use std::future::Future;
    use test_utils::TestAfterInit;

    wasm_bindgen_test_configure!(run_in_browser);

    #[wasm_bindgen_test(async)]
    fn generate_msdf_for_capital_a() -> impl Future<Output=()> {
        TestAfterInit::schedule(|| {
            // given
            let font_base = EmbeddedFonts::create_and_fill();
            let font = Font::load_from_memory(
                font_base.font_data_by_name.get("DejaVuSansMono-Bold").unwrap()
            );
            let params = MSDFParameters {
                width: 32,
                height: 32,
                edge_coloring_angle_threshold: 3.0,
                range: 2.0,
                edge_threshold: 1.001,
                overlap_support: true
            };
            // when
            let mut msdf = Vec::<f32>::new();
            generate_msdf(
                &mut msdf,
                &font,
                'A' as u32,
                &params,
                Vector2D { x: 1.0, y: 1.0 },
                Vector2D { x: 0.0, y: 0.0 }
            );
            // then
            // Note [asserts]
            assert_eq!(0.42730755, msdf[0]);
            assert_eq!(0.75, msdf[10]);
            assert_eq!(-9.759168, msdf[msdf.len()-1]);
        })
    }

    /* Note [asserts]
     *
     * we're checking rust - js interface only, so there is no need to check
     * all values
     */

    #[wasm_bindgen_test(async)]
    fn msdf_data_limits() -> impl Future<Output=()> {
        TestAfterInit::schedule(|| {
            assert!(MAX_MSDF_SIZE <= msdfgen_max_msdf_size());
        })
    }
}
