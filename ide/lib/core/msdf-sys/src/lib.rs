mod internal;

use internal::{
    emscripten_call_function,
    msdfgen_generate_msdf,
    msdfgen_free_font,
    emscripten_data_types
};
use js_sys::Uint8Array;
use wasm_bindgen::JsValue;
use vector2d::Vector2D;
use crate::internal::copy_f32_data_from_msdfgen_memory;

// ============
// === Font ===
// ============

pub struct Font {
    handle: JsValue
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
        unsafe { // Note [Usage of Uint8Array::view]
            let params = js_sys::Array::of2(
                &JsValue::from(Uint8Array::view(data)),
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
}

/*
 * Note [Usage of Uint8Array::view]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * We use view in this place to avoid copying font data. This is the only way
 * to do it with js_sys structures. The Uint8Array does not leave function
 * scope, so does not excess lifetime of data
 */

impl Drop for Font {
    fn drop(&mut self) {
        msdfgen_free_font(self.handle.clone())
    }
}

// =======================================
// === MutlichannelSignedDistanceField ===
// =======================================

/// Mutlichannel Signed Distance Field for one glyph
///
/// For more information about MSDF see [https://github.com/Chlumsky/msdfgen].
pub struct MutlichannelSignedDistanceField {
    pub width  : usize,
    pub height : usize,
    pub data   : [f32; Self::MAX_DATA_SIZE]
}

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

impl MutlichannelSignedDistanceField {
    pub const MAX_SIZE       : usize = 32;
    pub const CHANNELS_COUNT : usize = 3;
    pub const MAX_DATA_SIZE  : usize = Self::MAX_SIZE * Self::MAX_SIZE *
        Self::CHANNELS_COUNT;

    pub fn generate(
        font      : &Font,
        unicode   : u32,
        params    : &MSDFParameters,
        scale     : Vector2D<f64>,
        translate : Vector2D<f64>,
    ) -> MutlichannelSignedDistanceField {
        assert!(params.width  <= Self::MAX_SIZE);
        assert!(params.height <= Self::MAX_SIZE);

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
        let mut data : [f32; Self::MAX_DATA_SIZE] = [0.0; Self::MAX_DATA_SIZE];
        let data_size = params.width * params.height *
            Self::CHANNELS_COUNT;
        copy_f32_data_from_msdfgen_memory(
            output_address,
            &mut data,
            data_size
        );
        MutlichannelSignedDistanceField {
            width: params.width,
            height: params.height,
            data
        }
    }
}



#[cfg(test)]
mod tests {
    use wasm_bindgen_test::wasm_bindgen_test;
    use internal::msdfgen_max_msdf_size;
    use crate::*;

    #[wasm_bindgen_test]
    fn generate_msdf_for_capital_a() {
        // given
        let test_font : &[u8] = include_bytes!(
            concat!(env!("OUT_DIR"), "/DejaVuSansMono-Bold.ttf")
        );
        let font = Font::load_from_memory(test_font);
        let params = MSDFParameters {
            width: 32,
            height: 32,
            edge_coloring_angle_threshold: 3.0,
            range: 2.0,
            edge_threshold: 1.001,
            overlap_support: true
        };
        // when
        let msdf = MutlichannelSignedDistanceField::generate(
            &font,
            'A' as u32,
            &params,
            Vector2D { x: 1.0, y: 1.0 },
            Vector2D { x: 0.0, y: 0.0 }
        );
        // then
        // Note [asserts]
        assert_eq!(0.42730755, msdf.data[0]);
        assert_eq!(0.75, msdf.data[10]);
        assert_eq!(-9.759168, msdf.data[params.width * params.height *
            MutlichannelSignedDistanceField::CHANNELS_COUNT - 1]);
    }

    /* Note [asserts]
     *
     * we're checking rust - js interface only, so there is no need to check
     * all values
     */

    #[wasm_bindgen_test]
    fn msdf_data_limits() {
        assert!(MutlichannelSignedDistanceField::MAX_SIZE
            < msdfgen_max_msdf_size());
    }
}
