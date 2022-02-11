//! In this module we handle the fonts information required for rendering glyphs.

use crate::prelude::*;

pub mod msdf;

use enso_shapely::shared;
use ensogl_core::display::scene;
use ensogl_core::display::Scene;
use ensogl_text_embedded_fonts::EmbeddedFonts;
use ensogl_text_msdf_sys as msdf_sys;
use msdf_sys::Msdf;
use msdf_sys::MsdfParameters;
use std::collections::hash_map::Entry;



// =================
// === Constants ===
// =================

/// Default font the app will revert to if a desired font could not be loaded.
pub const DEFAULT_FONT: &str = "DejaVuSans";



// =============
// === Cache ===
// =============

#[derive(Debug)]
pub struct Cache<K: Eq + Hash, V> {
    map: RefCell<HashMap<K, V>>,
}

impl<K: Eq + Hash, V: Copy> Cache<K, V> {
    pub fn get_or_create<F>(&self, key: K, constructor: F) -> V
    where F: FnOnce() -> V {
        let mut map = self.map.borrow_mut();
        match map.entry(key) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => *entry.insert(constructor()),
        }
    }
}

impl<K: Eq + Hash, V> Cache<K, V> {
    pub fn len(&self) -> usize {
        self.map.borrow().len()
    }

    pub fn is_empty(&self) -> bool {
        self.map.borrow().is_empty()
    }

    pub fn invalidate(&self, key: &K) {
        self.map.borrow_mut().remove(key);
    }
}

impl<K: Eq + Hash, V> Default for Cache<K, V> {
    fn default() -> Self {
        Cache { map: default() }
    }
}



// ========================
// === Font render info ===
// ========================

/// Data used for rendering a single glyph.
///
/// Each distance and transformation values are expressed in normalized coordinates, where
/// (0.0, 0.0) is initial pen position for an character, and `y` = 1.0 is _ascender_.
///
/// The `offset` and `scale` fields transforms the _base square_ for a character, such the glyph
/// will be rendered correctly with assigned MSDF texture. The _base square_ corners are (0.0, 0.0),
/// (1.0, 1.0).
///
/// For explanation of various font-rendering terms, see the
/// [freetype documentation](https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html#section-1)
#[derive(Copy, Clone, Debug)]
pub struct GlyphRenderInfo {
    /// An index of glyph in a msdf texture (counted from the top of column). For details, see
    /// msdf::Texture documentation.
    pub msdf_texture_glyph_id: usize,

    /// A required offset of the _base square_. See structure documentation for details.
    pub offset: Vector2<f32>,

    /// A required scale of the _base square_. See structure documentation for details.
    pub scale: Vector2<f32>,

    /// Distance between two successive pen positions for specific glyph.
    pub advance: f32,
}

impl GlyphRenderInfo {
    /// See `MSDF_PARAMS` docs.
    pub const MAX_MSDF_SHRINK_FACTOR: f64 = 4.0;

    /// See `MSDF_PARAMS` docs.
    pub const MAX_MSDF_GLYPH_SCALE: f64 = 2.0;

    /// Parameters used for MSDF generation.
    ///
    /// The range was picked such way, that we avoid fitting range in one rendered pixel.
    /// Otherwise the antialiasing won't work. I assumed some maximum `shrink factor` (how many
    /// times rendered square will be smaller than MSDF size), and pick an arbitrary maximum glyph
    /// scale up.
    ///
    /// The rest of parameters are the defaults taken from msdfgen library
    pub const MSDF_PARAMS: MsdfParameters = MsdfParameters {
        width: msdf::Texture::WIDTH,
        height: msdf::Texture::ONE_GLYPH_HEIGHT,
        edge_coloring_angle_threshold: 3.0,
        range: Self::MAX_MSDF_SHRINK_FACTOR * Self::MAX_MSDF_GLYPH_SCALE,
        max_scale: Self::MAX_MSDF_GLYPH_SCALE,
        edge_threshold: 1.001,
        overlap_support: true,
    };

    /// Load new GlyphRenderInfo from msdf_sys font handle. This also extends the atlas with
    /// MSDF generated for this character.
    pub fn load(handle: &msdf_sys::Font, ch: char, atlas: &msdf::Texture) -> Self {
        let unicode = ch as u32;
        let params = Self::MSDF_PARAMS;
        let msdf = Msdf::generate(handle, unicode, &params);
        let inversed_scale = Vector2::new(1.0 / msdf.scale.x, 1.0 / msdf.scale.y);
        let translation = msdf::convert_msdf_translation(&msdf);
        let glyph_id = atlas.rows() / msdf::Texture::ONE_GLYPH_HEIGHT;
        atlas.extend_with_raw_data(msdf.data.iter());
        GlyphRenderInfo {
            msdf_texture_glyph_id: glyph_id,
            offset:                -translation,
            scale:                 Vector2(inversed_scale.x as f32, inversed_scale.y as f32),
            advance:               msdf::x_distance_from_msdf_value(msdf.advance),
        }
    }
}



// ============
// === Font ===
// ============

/// A single font data used for rendering.
///
/// The data for individual characters and kerning are load on demand.
///
/// Each distance and transformation values are expressed in normalized coordinates, where `y` = 0.0
/// is _baseline_ and `y` = 1.0 is _ascender_. For explanation of various font-rendering terms, see
/// [freetype documentation](https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html#section-1)

#[derive(Debug, Clone, CloneRef, Deref)]
pub struct Font {
    rc: Rc<FontData>,
}

impl From<FontData> for Font {
    fn from(t: FontData) -> Self {
        let rc = Rc::new(t);
        Self { rc }
    }
}



// ================
// === FontData ===
// ================

/// Internal representation of `Font`.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct FontData {
    pub name:  String,
    msdf_font: msdf_sys::Font,
    atlas:     msdf::Texture,
    glyphs:    Cache<char, GlyphRenderInfo>,
    kerning:   Cache<(char, char), f32>,
}

impl Font {
    /// Constructor.
    pub fn from_msdf_font(name: String, msdf_font: msdf_sys::Font) -> Self {
        let atlas = default();
        let glyphs = default();
        let kerning = default();
        FontData { name, msdf_font, atlas, glyphs, kerning }.into()
    }

    /// Constructor.
    pub fn from_raw_data(name: String, font_data: &[u8]) -> Self {
        Self::from_msdf_font(name, msdf_sys::Font::load_from_memory(font_data))
    }

    /// Create render info for one of embedded fonts
    pub fn try_from_embedded(base: &EmbeddedFonts, name: &str) -> Option<Self> {
        let font_data_opt = base.font_data_by_name.get(name);
        font_data_opt.map(|data| Self::from_raw_data(name.to_string(), data))
    }

    /// Get render info for one character, generating one if not found.
    pub fn glyph_info(&self, ch: char) -> GlyphRenderInfo {
        let handle = &self.msdf_font;
        self.glyphs.get_or_create(ch, move || GlyphRenderInfo::load(handle, ch, &self.atlas))
    }

    /// Get kerning between two characters
    pub fn kerning(&self, left: char, right: char) -> f32 {
        self.kerning.get_or_create((left, right), || {
            let msdf_val = self.msdf_font.retrieve_kerning(left, right);
            msdf::x_distance_from_msdf_value(msdf_val)
        })
    }

    /// A whole msdf texture bound for this font.
    pub fn with_borrowed_msdf_texture_data<F, R>(&self, operation: F) -> R
    where F: FnOnce(&[u8]) -> R {
        self.atlas.with_borrowed_data(operation)
    }

    /// Get number of rows in msdf texture.
    pub fn msdf_texture_rows(&self) -> usize {
        self.atlas.rows()
    }

    #[cfg(test)]
    pub fn mock(name: impl Into<String>) -> Self {
        Self::from_msdf_font(name.into(), msdf_sys::Font::mock_font())
    }

    #[cfg(test)]
    pub fn mock_char_info(
        &self,
        ch: char,
        offset: Vector2<f32>,
        scale: Vector2<f32>,
        advance: f32,
    ) -> GlyphRenderInfo {
        self.glyphs.invalidate(&ch);
        let data_size = msdf::Texture::ONE_GLYPH_SIZE;
        let msdf_data = (0..data_size).map(|_| 0.12345);
        let msdf_texture_glyph_id = self.msdf_texture_rows() / msdf::Texture::ONE_GLYPH_HEIGHT;

        self.atlas.extend_with_raw_data(msdf_data);
        self.glyphs.get_or_create(ch, move || GlyphRenderInfo {
            offset,
            scale,
            advance,
            msdf_texture_glyph_id,
        })
    }

    #[cfg(test)]
    pub fn mock_kerning_info(&self, l: char, r: char, value: f32) {
        self.kerning.invalidate(&(l, r));
        self.kerning.get_or_create((l, r), || value);
    }
}



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
    /// Load a font by name. The font can be loaded either from cache or from the embedded fonts
    /// registry if not used before. Returns None if the name is missing in both cache and embedded
    /// font list.
    pub fn try_load(&mut self, name:&str) -> Option<Font> {
        match self.fonts.entry(name.to_string()) {
            Entry::Occupied (entry) => Some(entry.get().clone_ref()),
            Entry::Vacant   (entry) => Font::try_from_embedded(&self.embedded,name).map(|font| {
                entry.insert(font.clone_ref());
                font
            })
        }
    }

    /// Load a font by name. The font can be loaded either from cache or from the embedded fonts
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
    /// Create empty font `Registry` and load raw data of embedded fonts.
    pub fn init_and_load_default() -> RegistryData {
        let embedded = EmbeddedFonts::create_and_fill();
        let fonts = HashMap::new();
        let default_name = DEFAULT_FONT;
        let default = Font::try_from_embedded(&embedded, default_name)
            .unwrap_or_else(|| panic!("Cannot load default font {}.", default_name));
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

    use ensogl_text_embedded_fonts::EmbeddedFonts;
    use wasm_bindgen_test::wasm_bindgen_test;
    use wasm_bindgen_test::wasm_bindgen_test_configure;

    const TEST_FONT_NAME: &str = "DejaVuSansMono-Bold";

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
