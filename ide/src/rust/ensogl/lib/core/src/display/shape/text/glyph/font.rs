//! In this module we handle the fonts information required for rendering glyphs.

use crate::prelude::*;

use crate::display::shape::text::glyph::msdf;

use ensogl_text_msdf_sys as msdf_sys;
use ensogl_text_embedded_fonts::EmbeddedFonts;
use msdf_sys::MsdfParameters;
use msdf_sys::Msdf;
use std::collections::hash_map::Entry;



// =============
// === Cache ===
// =============

#[derive(Debug)]
struct Cache<K:Eq+Hash, V> {
    map: RefCell<HashMap<K,V>>,
}

impl<K:Eq+Hash, V:Copy> Cache<K,V> {
    pub fn get_or_create<F>(&self, key:K, constructor:F) -> V
    where F : FnOnce() -> V {
        let mut map = self.map.borrow_mut();
        match map.entry(key) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => *entry.insert(constructor()),
        }
    }
}

impl<K:Eq+Hash, V:Clone> Cache<K,V> {
    pub fn get_clone_or_create<F>(&self, key:K, constructor:F) -> V
        where F : FnOnce() -> V {
        let mut map = self.map.borrow_mut();
        match map.entry(key) {
            Entry::Occupied(entry) => entry.get().clone(),
            Entry::Vacant(entry) => entry.insert(constructor()).clone(),
        }
    }
}

impl<K:Eq+Hash, V> Cache<K,V> {
    pub fn len(&self) -> usize {
        self.map.borrow().len()
    }

    pub fn is_empty(&self) -> bool {
        self.map.borrow().is_empty()
    }

    pub fn invalidate(&self, key:&K) {
        self.map.borrow_mut().remove(key);
    }
}

impl<K:Eq+Hash, V> Default for Cache<K,V> {
    fn default() -> Self {
        Cache { map:default() }
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
#[derive(Copy,Clone,Debug)]
pub struct GlyphRenderInfo {
    /// An index of glyph in a msdf texture (counted from the top of column. For details, see
    /// msdf::Texture documentation.
    pub msdf_texture_glyph_id: usize,

    /// A required offset of the _base square_ see structure documentation for details.
    pub offset: Vector2<f32>,

    /// A required scale of the _base square_ see structure documentation for details.
    pub scale: Vector2<f32>,

    /// An advance. Advance is the distance between two successive pen positions for specific glyph.
    pub advance: f32
}

impl GlyphRenderInfo {
    /// See `MSDF_PARAMS` docs.
    pub const MAX_MSDF_SHRINK_FACTOR : f64 = 4.0;

    /// See `MSDF_PARAMS` docs.
    pub const MAX_MSDF_GLYPH_SCALE : f64 = 2.0;

    /// Parameters used for MSDF generation.
    ///
    /// The range was picked such way, that we avoid fitting range in one rendered pixel.
    /// Otherwise the antialiasing won't work. I assumed some maximum `shrink factor` (how many
    /// times rendered square will be smaller than MSDF size), and pick an arbitrary maximum glyph
    /// scale up.
    ///
    /// The rest of parameters are the defaults taken from msdfgen library
    pub const MSDF_PARAMS : MsdfParameters = MsdfParameters {
        width                         : msdf::Texture::WIDTH,
        height                        : msdf::Texture::ONE_GLYPH_HEIGHT,
        edge_coloring_angle_threshold : 3.0,
        range                         : Self::MAX_MSDF_SHRINK_FACTOR * Self::MAX_MSDF_GLYPH_SCALE,
        max_scale                     : Self::MAX_MSDF_GLYPH_SCALE,
        edge_threshold                : 1.001,
        overlap_support               : true
    };

    /// Load new GlyphRenderInfo from msdf_sys font handle. This also extends the msdf_texture with
    /// MSDF generated for this character.
    pub fn load(handle:&msdf_sys::Font, ch:char, msdf_texture:&msdf::Texture) -> Self {
        let unicode        = ch as u32;
        let params         = Self::MSDF_PARAMS;

        let msdf           = Msdf::generate(handle,unicode,&params);
        let inversed_scale = Vector2::new(1.0/msdf.scale.x, 1.0/msdf.scale.y);
        let translation    = msdf::convert_msdf_translation(&msdf);
        let glyph_id       = msdf_texture.rows() / msdf::Texture::ONE_GLYPH_HEIGHT;
        msdf_texture.extend_with_raw_data(msdf.data.iter());
        GlyphRenderInfo {
            msdf_texture_glyph_id : glyph_id,
            offset                : nalgebra::convert(-translation),
            scale                 : nalgebra::convert(inversed_scale),
            advance               : msdf::x_distance_from_msdf_value(msdf.advance),
        }
    }
}

/// A single font data used for rendering
///
/// The data for individual characters and kerning are load on demand.
///
/// Each distance and transformation values are expressed in normalized coordinates, where `y` = 0.0
/// is _baseline_ and `y` = 1.0 is _ascender_. For explanation of various font-rendering terms, see
/// [freetype documentation](https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html#section-1)
#[derive(Debug)]
pub struct RenderInfo {
    /// Name of the font.
    pub name      : String,
    msdf_sys_font : msdf_sys::Font,
    msdf_texture  : msdf::Texture,
    glyphs        : Cache<char,GlyphRenderInfo>,
    kerning       : Cache<(char,char),f32>
}

impl RenderInfo {
    /// Create render info based on font data in memory
    pub fn new(name:String, font_data:&[u8]) -> RenderInfo {
        RenderInfo {name,
            msdf_sys_font : msdf_sys::Font::load_from_memory(font_data),
            msdf_texture  : default(),
            glyphs        : default(),
            kerning       : default(),
        }
    }

    /// Create render info for one of embedded fonts
    pub fn from_embedded(base:&EmbeddedFonts, name:&str)
    -> Option<RenderInfo> {
        let font_data_opt = base.font_data_by_name.get(name);
        font_data_opt.map(|data| RenderInfo::new(name.to_string(),data))
    }

    /// Get render info for one character, generating one if not found
    pub fn get_glyph_info(&self, ch:char) -> GlyphRenderInfo {
        let handle = &self.msdf_sys_font;
        self.glyphs.get_or_create(ch, move || GlyphRenderInfo::load(handle,ch,&self.msdf_texture))
    }

    /// Get kerning between two characters
    pub fn get_kerning(&self, left:char, right:char) -> f32 {
        self.kerning.get_or_create((left,right), || {
            let msdf_val = self.msdf_sys_font.retrieve_kerning(left, right);
            msdf::x_distance_from_msdf_value(msdf_val)
        })
    }

    /// A whole msdf texture bound for this font.
    pub fn with_borrowed_msdf_texture_data<F,R>(&self, operation:F) -> R
    where F : FnOnce(&[u8]) -> R {
        self.msdf_texture.with_borrowed_data(operation)
    }

    /// Get number of rows in msdf texture.
    pub fn msdf_texture_rows(&self) -> usize {
        self.msdf_texture.rows()
    }

    #[cfg(test)]
    pub fn mock_font(name : String) -> RenderInfo {
        RenderInfo { name,
            msdf_sys_font : msdf_sys::Font::mock_font(),
            msdf_texture  : default(),
            glyphs        : default(),
            kerning       : default(),
        }
    }

    #[cfg(test)]
    pub fn mock_char_info
    (&self, ch:char, offset:Vector2<f32>, scale:Vector2<f32>, advance:f32) -> GlyphRenderInfo {
        self.glyphs.invalidate(&ch);
        let data_size             = msdf::Texture::ONE_GLYPH_SIZE;
        let msdf_data             = (0..data_size).map(|_| 0.12345);
        let msdf_texture_glyph_id = self.msdf_texture_rows() / msdf::Texture::ONE_GLYPH_HEIGHT;

        self.msdf_texture.extend_with_raw_data(msdf_data);
        self.glyphs.get_or_create(ch, move || {
            GlyphRenderInfo {offset,scale,advance,msdf_texture_glyph_id}
        })
    }

    #[cfg(test)]
    pub fn mock_kerning_info(&self, l:char, r:char, value:f32) {
        self.kerning.invalidate(&(l,r));
        self.kerning.get_or_create((l,r),|| value);
    }
}



// ==============
// === Handle ===
// ==============

/// A handle for fonts loaded into memory.
pub type Handle = Rc<RenderInfo>;



// ================
// === Registry ===
// ================

/// Structure keeping all fonts loaded from different sources.
#[derive(Debug)]
pub struct Registry {
    embedded : EmbeddedFonts,
    fonts    : HashMap<String,Handle>,
}

impl Registry {
    /// Create empty `Fonts` structure (however it contains raw data of embedded fonts).
    pub fn new() -> Registry {
        Registry {
            embedded : EmbeddedFonts::create_and_fill(),
            fonts    : HashMap::new(),
        }
    }

    /// Get render font info from loaded fonts, and if it does not exists, load data from one of
    /// embedded fonts. Returns None if the name is missing in both loaded and embedded font list.
    pub fn get_or_load_embedded_font(&mut self, name:&str) -> Option<Handle> {
        match self.fonts.entry(name.to_string()) {
            Entry::Occupied(entry) => Some(entry.get().clone()),
            Entry::Vacant(entry)   => {
                let font_opt = RenderInfo::from_embedded(&self.embedded,name);
                font_opt.map(|font| {
                    let rc = Rc::new(font);
                    entry.insert(rc.clone_ref());
                    rc
                })
            }
        }
    }

    /// Get handle one of loaded fonts.
    pub fn get_render_info(&mut self, name:&str) -> Option<Handle> {
        self.fonts.get_mut(name).cloned()
    }
}

impl Default for Registry {
    fn default() -> Self {
        Self::new()
    }
}



// ======================
// === SharedRegistry ===
// ======================

/// Shared version of `Registry`.
#[derive(Clone,CloneRef,Debug)]
pub struct SharedRegistry {
    model : Rc<RefCell<Registry>>
}

impl SharedRegistry {
    /// Constructor.
    #[allow(clippy::new_without_default)] // FIXME rename new to something more explicit
    pub fn new() -> SharedRegistry {
        let model = Rc::new(RefCell::new(Registry::new()));
        Self {model}
    }

    /// Get render font info from loaded fonts, and if it does not exists, load data from one of
    /// embedded fonts. Returns None if the name is missing in both loaded and embedded font list.
    pub fn get_or_load_embedded_font(&self, name:&str) -> Option<Handle> {
        self.model.borrow_mut().get_or_load_embedded_font(name)
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

    const TEST_FONT_NAME : &str = "DejaVuSansMono-Bold";

    fn create_test_font_render_info() -> RenderInfo {
        let mut embedded_fonts = EmbeddedFonts::create_and_fill();
        RenderInfo::from_embedded(
            &mut embedded_fonts,
            TEST_FONT_NAME
        ).unwrap()
    }

    wasm_bindgen_test_configure!(run_in_browser);

    #[wasm_bindgen_test(async)]
    async fn empty_font_render_info() {
        ensogl_text_msdf_sys::initialized().await;
        let font_render_info = create_test_font_render_info();

        assert_eq!(TEST_FONT_NAME, font_render_info.name);
        assert_eq!(0, font_render_info.msdf_texture.with_borrowed_data(|t|t.len()));
        assert_eq!(0, font_render_info.glyphs.len());
    }

    #[wasm_bindgen_test(async)]
    async fn loading_glyph_info() {
        ensogl_text_msdf_sys::initialized().await;
        let font_render_info = create_test_font_render_info();

        font_render_info.get_glyph_info('A');
        font_render_info.get_glyph_info('B');

        let chars      = 2;
        let tex_width  = msdf::Texture::WIDTH;
        let tex_height = msdf::Texture::ONE_GLYPH_HEIGHT * chars;
        let channels   = Msdf::CHANNELS_COUNT;
        let tex_size   = tex_width * tex_height * channels;

        assert_eq!(tex_height , font_render_info.msdf_texture_rows());
        assert_eq!(tex_size   , font_render_info.msdf_texture.with_borrowed_data(|t|t.len()));
        assert_eq!(chars      , font_render_info.glyphs.len());

        let first_char  = font_render_info.glyphs.get_or_create('A', || panic!("Expected value"));
        let second_char = font_render_info.glyphs.get_or_create('B', || panic!("Expected value"));

        let first_index  = 0;
        let second_index = 1;

        assert_eq!(first_index  , first_char.msdf_texture_glyph_id);
        assert_eq!(second_index , second_char.msdf_texture_glyph_id);
    }

    #[wasm_bindgen_test(async)]
    async fn getting_or_creating_char() {
        ensogl_text_msdf_sys::initialized().await;
        let font_render_info = create_test_font_render_info();

        {
            let char_info = font_render_info.get_glyph_info('A');
            assert_eq!(0, char_info.msdf_texture_glyph_id);
        }
        assert_eq!(1, font_render_info.glyphs.len());

        {
            let char_info = font_render_info.get_glyph_info('A');
            assert_eq!(0, char_info.msdf_texture_glyph_id);
        }
        assert_eq!(1, font_render_info.glyphs.len());
    }
}
