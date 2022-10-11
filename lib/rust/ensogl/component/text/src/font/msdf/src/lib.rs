//! MSDF-gen libraries bindings and utilities.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use crate::prelude::*;
use binding::*;

use emscripten_data::ArrayMemoryView;
use enso_web as web;
use enso_web::Closure;
use enso_web::JsValue;
use js_sys::Uint8Array;
use owned_ttf_parser::Tag;
use std::future::Future;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;



mod binding;
pub mod emscripten_data;
pub mod texture;

pub use texture::*;

/// Common types.
pub mod prelude {
    pub use enso_prelude::*;
    pub use enso_types::*;
}



// ==============
// === Errors ===
// ==============

#[allow(missing_docs)]
#[derive(Clone, Debug, Fail, Eq, PartialEq)]
pub enum SetVariationAxisError {
    #[fail(
        display = "Msdfgen `setVariationAxis` operation was not successfull for axis: {}.",
        name
    )]
    LibraryError { name: String },
}



// ======================
// === Initialization ===
// ======================

/// Add initialization callback. The callback passed as argument will be called once the msdfgen
/// library is initialized.
pub fn run_once_initialized<F>(callback: F)
where F: 'static + FnOnce() {
    if is_emscripten_runtime_initialized() {
        callback()
    } else {
        let js_callback = Closure::once_into_js(callback);
        on_emscripten_runtime_initialized(js_callback);
    }
}

/// A future which resolves once the msdfgen library is initialized.
pub fn initialized() -> impl Future<Output = ()> {
    MsdfgenJsInitialized()
}

/// A future for running test after initialization.
#[derive(Debug)]
struct MsdfgenJsInitialized();

impl Future for MsdfgenJsInitialized {
    type Output = ();

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        if is_emscripten_runtime_initialized() {
            Poll::Ready(())
        } else {
            let waker = cx.waker().clone();
            run_once_initialized(move || waker.wake());
            Poll::Pending
        }
    }
}



// =================
// === OwnedFace ===
// =================

/// A font face loaded to JS memory and handled by the msdfgen library. The name uses the "owned"
/// prefix in order to stay compatible with the `ttf-parser` library.
#[allow(missing_docs)]
#[derive(Debug)]
pub struct OwnedFace {
    pub handle: JsValue,
}

#[cfg(target_arch = "wasm32")]
impl Drop for OwnedFace {
    fn drop(&mut self) {
        msdfgen_free_font(self.handle.clone())
    }
}

impl OwnedFace {
    /// Load font from memory.
    ///
    /// Loads font from any format which freetype library can handle. See
    /// [https://www.freetype.org/freetype2/docs/index.html] for reference.
    pub fn load_from_memory(data: &[u8]) -> Self {
        if cfg!(target_arch = "wasm32") {
            Self::load_from_memory_wasm(data)
        } else {
            Self::mock_font()
        }
    }

    fn load_from_memory_wasm(data: &[u8]) -> Self {
        let array_type_js = JsValue::from_str(ccall_types::ARRAY);
        let number_type_js = JsValue::from_str(ccall_types::NUMBER);
        let data_js_array = Uint8Array::from(data);
        let data_js = JsValue::from(data_js_array);
        let data_size_js = JsValue::from_f64(data.len() as f64);

        let function_name = "msdfgen_loadFontMemory";
        let return_type = ccall_types::NUMBER;
        let param_types = web::Array::of2(&array_type_js, &number_type_js);
        let params = web::Array::of2(&data_js, &data_size_js);

        let handle = emscripten_call_function(function_name, return_type, param_types, params);
        OwnedFace { handle }
    }

    /// Set font's variation axis.
    pub fn set_variation_axis(
        &self,
        tag: Tag,
        coordinate: f64,
    ) -> Result<(), SetVariationAxisError> {
        let ok = msdfgen_set_variation_axis(self.handle.clone(), tag.0, coordinate) != 0;
        ok.ok_or_else(|| SetVariationAxisError::LibraryError { name: tag.to_string() })
    }

    /// Mocked version of this struct. Used for testing purposes.
    pub fn mock_font() -> OwnedFace {
        let handle = JsValue::from_f64(0.0);
        OwnedFace { handle }
    }
}



// ======================
// === MsdfParameters ===
// ======================

/// Parameters of MSDF generation.
///
/// The structure gathering MSDF generation parameters meant to be same for all rendered glyphs.
#[derive(Clone, Copy, Debug)]
#[allow(missing_docs)]
pub struct MsdfParameters {
    pub width: usize,
    pub height: usize,
    pub edge_coloring_angle_threshold: f64,
    pub range: f64,
    pub max_scale: f64,
    pub edge_threshold: f64,
    pub overlap_support: bool,
}



// ============
// === Msdf ===
// ============

/// Binding to the MSDF-gen library.
#[allow(missing_docs)]
#[derive(Debug)]
pub struct Msdf {
    handle:          JsValue,
    pub advance:     f64,
    pub translation: nalgebra::Vector2<f64>,
    pub scale:       nalgebra::Vector2<f64>,
    pub data:        ArrayMemoryView<f32>,
}

impl Drop for Msdf {
    fn drop(&mut self) {
        msdfgen_free_result(self.handle.clone());
    }
}

impl Msdf {
    /// Number of used color channels in the MSDF texture.
    pub const CHANNELS_COUNT: usize = 3;

    /// Generate Mutlichannel Signed Distance Field (MSDF) for one glyph.
    ///
    /// For more information about MSDF see [https://github.com/Chlumsky/msdfgen].
    pub fn generate(font: &OwnedFace, unicode: u32, params: &MsdfParameters) -> Msdf {
        let handle = msdfgen_generate_msdf(
            params.width,
            params.height,
            font.handle.clone(),
            unicode,
            params.edge_coloring_angle_threshold,
            params.range,
            params.max_scale,
            params.edge_threshold,
            params.overlap_support,
        );
        Self::msdf_from_generation_result_handle(handle, params)
    }

    /// Generate Mutlichannel Signed Distance Field (MSDF) for one glyph by its index in the font.
    ///
    /// For more information about MSDF see [https://github.com/Chlumsky/msdfgen].
    pub fn generate_by_index(font: &OwnedFace, index: usize, params: &MsdfParameters) -> Msdf {
        let handle = msdfgen_generate_msdf_by_index(
            params.width,
            params.height,
            font.handle.clone(),
            index,
            params.edge_coloring_angle_threshold,
            params.range,
            params.max_scale,
            params.edge_threshold,
            params.overlap_support,
        );
        Self::msdf_from_generation_result_handle(handle, params)
    }

    fn msdf_from_generation_result_handle(handle: JsValue, params: &MsdfParameters) -> Msdf {
        let advance = msdfgen_result_get_advance(handle.clone());
        let translation = Self::translation(&handle);
        let scale = Self::scale(&handle);
        let data_adress = msdfgen_result_get_msdf_data(handle.clone());
        let data_size = params.width * params.height * Self::CHANNELS_COUNT;
        let data = ArrayMemoryView::new(data_adress, data_size);
        Msdf { handle, advance, translation, scale, data }
    }

    const DIMENSIONS: usize = 2;

    fn translation(handle: &JsValue) -> nalgebra::Vector2<f64> {
        let address = msdfgen_result_get_translation(handle.clone());
        let view = ArrayMemoryView::new(address, Self::DIMENSIONS);
        let mut iter = view.iter();
        let translate_x = iter.next().unwrap();
        let translate_y = iter.next().unwrap();
        nalgebra::Vector2::new(translate_x, translate_y)
    }

    fn scale(handle: &JsValue) -> nalgebra::Vector2<f64> {
        let address = msdfgen_result_get_scale(handle.clone());
        let view = ArrayMemoryView::new(address, Self::DIMENSIONS);
        let mut iter = view.iter();
        let scale_x = iter.next().unwrap();
        let scale_y = iter.next().unwrap();
        nalgebra::Vector2::new(scale_x, scale_y)
    }

    /// Mocked version of this struct. Used for testing purposes.
    pub fn mock_results() -> Msdf {
        Msdf {
            handle:      JsValue::from_f64(0.0),
            advance:     0.0,
            translation: nalgebra::Vector2::new(0.0, 0.0),
            scale:       nalgebra::Vector2::new(1.0, 1.0),
            data:        ArrayMemoryView::empty(),
        }
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use ensogl_text_embedded_fonts::Embedded;
    use nalgebra::Vector2;
    use wasm_bindgen_test::wasm_bindgen_test;
    use wasm_bindgen_test::wasm_bindgen_test_configure;

    wasm_bindgen_test_configure!(run_in_browser);

    #[wasm_bindgen_test(async)]
    async fn generate_msdf_for_capital_a() {
        initialized().await;
        // given
        let font_base = Embedded::init_and_load_embedded_fonts();
        let font_name = "DejaVuSansMono-Bold.ttf";
        let font = OwnedFace::load_from_memory(font_base.data.get(font_name).unwrap());
        let params = MsdfParameters {
            width: 32,
            height: 32,
            edge_coloring_angle_threshold: 3.0,
            range: 2.0,
            max_scale: 2.0,
            edge_threshold: 1.001,
            overlap_support: true,
        };
        // when
        let msdf = Msdf::generate(&font, 'A' as u32, &params);
        // then
        let data: Vec<f32> = msdf.data.iter().collect();
        assert_eq!(-0.9408906, data[0]); // Note [asserts]
        assert_eq!(0.2, data[10]);
        assert_eq!(-4.3035655, data[data.len() - 1]);
        assert_eq!(Vector2::new(3.03125, 1.0), msdf.translation);
        assert_eq!(Vector2::new(1.25, 1.25), msdf.scale);
        assert_eq!(19.265625, msdf.advance);
    }

    #[wasm_bindgen_test(async)]
    async fn generate_msdf_for_capital_a_by_index() {
        initialized().await;
        // given
        let font_base = Embedded::init_and_load_embedded_fonts();
        let font_name = "DejaVuSansMono-Bold.ttf";
        let font = OwnedFace::load_from_memory(font_base.data.get(font_name).unwrap());
        let params = MsdfParameters {
            width: 32,
            height: 32,
            edge_coloring_angle_threshold: 3.0,
            range: 2.0,
            max_scale: 2.0,
            edge_threshold: 1.001,
            overlap_support: true,
        };
        // when
        let msdf = Msdf::generate_by_index(&font, 36, &params);
        // then
        let data: Vec<f32> = msdf.data.iter().collect();
        assert_eq!(-0.9408906, data[0]); // Note [asserts]
        assert_eq!(0.2, data[10]);
        assert_eq!(-4.3035655, data[data.len() - 1]);
        assert_eq!(Vector2::new(3.03125, 1.0), msdf.translation);
        assert_eq!(Vector2::new(1.25, 1.25), msdf.scale);
        assert_eq!(19.265625, msdf.advance);
    }

    /* Note [asserts]
     *
     * we're checking rust - js interface only, so there is no need to check
     * all values
     */
}
