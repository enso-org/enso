//! In this module we handle the fonts information required for rendering glyphs.

use crate::prelude::*;

use enso_shapely::shared;
use ensogl_core::display::scene;
use ensogl_core::display::Scene;
use ensogl_text_embedded_fonts as embedded_fonts;
use ensogl_text_embedded_fonts::EmbeddedFonts;
use ensogl_text_embedded_fonts::Family;
use ensogl_text_msdf_sys as msdf_sys;
use msdf_sys::Msdf;
use msdf_sys::MsdfParameters;
use owned_ttf_parser as ttf;
use owned_ttf_parser::AsFaceRef;
use serde;
use std::collections::hash_map::Entry;


pub use ensogl_text_font::*;


// =================
// === Constants ===
// =================

/// Default font the app will revert to if a desired font could not be loaded.
pub const DEFAULT_FONT: &str = embedded_fonts::DefaultFamily::regular();



// ====================
// === RegistryData ===
// ====================

shared! { Registry
/// Structure keeping all fonts loaded from different sources.
#[derive(Debug)]
pub struct RegistryData {
    embedded : EmbeddedFonts,
    fonts    : HashMap<String,Font>,
    default  : Font,
}

impl {
    /// Load a font by name. The font can be loaded either from cache or from the embedded fonts'
    /// registry if not used before. Returns None if the name is missing in both cache and embedded
    /// font list.
    pub fn try_load(&mut self, name:&str) -> Option<Font> {
        match self.fonts.entry(name.to_string()) {
            Entry::Occupied (entry) => Some(entry.get().clone_ref()),
            Entry::Vacant   (entry) => RegistryData::try_from_embedded(&self.embedded,name).map(|font| {
                entry.insert(font.clone_ref());
                font
            })
        }
    }

    /// Load a font by name. The font can be loaded either from cache or from the embedded fonts'
    /// registry if not used before. Returns default font if the name is missing in both cache and
    /// embedded font list.
    pub fn load(&mut self, name:&str) -> Font {
        self.try_load(name).unwrap_or_else(|| self.default())
    }

    /// Get the default font. It is often used in case the desired font could not be loaded.
    pub fn default(&self) -> Font {
        self.default.clone_ref()
    }
}}

impl RegistryData {
    // TODO: rename and update docs
    /// Create render info for one of embedded fonts
    pub fn try_from_embedded(base: &EmbeddedFonts, name: &str) -> Option<Font> {
        base.ttf_binary_data
            .get(name)
            .and_then(|data| Font::from_raw_data(name.to_string(), data).ok())
    }

    /// Create empty font `Registry` and load raw data of embedded fonts.
    pub fn init_and_load_default() -> RegistryData {
        let embedded = EmbeddedFonts::create_and_fill();
        let fonts = HashMap::new();
        let default_font = DEFAULT_FONT;
        let default = Self::try_from_embedded(&embedded, default_font)
            .unwrap_or_else(|| panic!("Cannot load the default font '{}'.", default_font));
        Self { embedded, fonts, default }
    }
}

impl Registry {
    /// Constructor.
    pub fn init_and_load_default() -> Registry {
        let rc = Rc::new(RefCell::new(RegistryData::init_and_load_default()));
        Self { rc }
    }
}

impl scene::Extension for Registry {
    fn init(_scene: &Scene) -> Self {
        Self::init_and_load_default()
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use ensogl_text_embedded_fonts;
    use ensogl_text_embedded_fonts::EmbeddedFonts;
    use ensogl_text_embedded_fonts::Family;
    use wasm_bindgen_test::wasm_bindgen_test;
    use wasm_bindgen_test::wasm_bindgen_test_configure;

    const TEST_FONT_NAME: &str = embedded_fonts::DefaultFamily::mono_bold();

    fn create_test_font() -> Font {
        let embedded_fonts = EmbeddedFonts::create_and_fill();
        Font::try_from_embedded(&embedded_fonts, TEST_FONT_NAME).unwrap()
    }

    wasm_bindgen_test_configure!(run_in_browser);

    #[wasm_bindgen_test(async)]
    async fn empty_font_render_info() {
        ensogl_text_msdf_sys::initialized().await;
        let font_render_info = create_test_font();

        assert_eq!(TEST_FONT_NAME, font_render_info.name);
        assert_eq!(0, font_render_info.atlas.with_borrowed_data(|t: &[u8]| t.len()));
        assert_eq!(0, font_render_info.glyphs.len());
    }

    #[wasm_bindgen_test(async)]
    async fn loading_glyph_info() {
        ensogl_text_msdf_sys::initialized().await;
        let font_render_info = create_test_font();

        font_render_info.glyph_info('A');
        font_render_info.glyph_info('B');

        let chars = 2;
        let tex_width = msdf::Texture::WIDTH;
        let tex_height = msdf::Texture::ONE_GLYPH_HEIGHT * chars;
        let channels = Msdf::CHANNELS_COUNT;
        let tex_size = tex_width * tex_height * channels;

        assert_eq!(tex_height, font_render_info.msdf_texture_rows());
        assert_eq!(tex_size, font_render_info.atlas.with_borrowed_data(|t| t.len()));
        assert_eq!(chars, font_render_info.glyphs.len());

        let first_char = font_render_info.glyphs.get_or_create('A', || panic!("Expected value"));
        let second_char = font_render_info.glyphs.get_or_create('B', || panic!("Expected value"));

        let first_index = 0;
        let second_index = 1;

        assert_eq!(first_index, first_char.msdf_texture_glyph_id);
        assert_eq!(second_index, second_char.msdf_texture_glyph_id);
    }

    #[wasm_bindgen_test(async)]
    async fn getting_or_creating_char() {
        ensogl_text_msdf_sys::initialized().await;
        let font_render_info = create_test_font();

        {
            let char_info = font_render_info.glyph_info('A');
            assert_eq!(0, char_info.msdf_texture_glyph_id);
        }
        assert_eq!(1, font_render_info.glyphs.len());

        {
            let char_info = font_render_info.glyph_info('A');
            assert_eq!(0, char_info.msdf_texture_glyph_id);
        }
        assert_eq!(1, font_render_info.glyphs.len());
    }
}
