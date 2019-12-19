use crate::prelude::*;

use crate::text::msdf::MsdfTexture;
use crate::text::msdf::convert_msdf_transformation;
use crate::text::msdf::x_distance_from_msdf_value;

use basegl_core_msdf_sys as msdf_sys;
use basegl_core_embedded_fonts::EmbeddedFonts;
use msdf_sys::MsdfParameters;
use msdf_sys::MultichannelSignedDistanceField;
use std::collections::hash_map::Entry::Occupied;
use std::collections::hash_map::Entry::Vacant;

// ========================
// === Font render info ===
// ========================

/// Data used for rendering a single glyph
///
/// Each distance and transformation values are expressed in normalized coordinates, where
/// (0.0, 0.0) is initial pen position for an character, and `y` = 1.0 is _ascender_.
///
/// `from_base_layout` transforms the _base square_ for a character, such the glyph will be rendered
/// correctly with assigned MSDF texture. The _base square_ corners are (0.0, 0.0), (1.0, 1.0).
/// See also `glyph_render::GLYPH_SQUARE_VERTICES_BASE_LAYOUT`.
///
/// For explanation of various font-rendering terms, see
/// [freetype documentation](https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html#section-1)
#[derive(Debug)]
pub struct GlyphRenderInfo {
    pub msdf_texture_rows : std::ops::Range<usize>,
    pub from_base_layout  : nalgebra::Projective2<f64>,
    pub advance           : f64
}

/// A single font data used for rendering
///
/// The data for individual characters and kerning are load on demand.
///
/// Each distance and transformation values are expressed in normalized coordinates, where `y` = 0.0
/// is _baseline_ and `y` = 1.0 is _ascender_. For explanation of various font-rendering terms, see
/// [freetype documentation](https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html#section-1)
#[derive(Debug)]
pub struct FontRenderInfo {
    pub name          : String,
    pub msdf_sys_font : msdf_sys::Font,
    pub msdf_texture  : MsdfTexture,
    glyphs            : HashMap<char,GlyphRenderInfo>,
    kerning           : HashMap<(char,char),f64>
}

impl FontRenderInfo {
    pub const MAX_MSDF_SHRINK_FACTOR : f64 = 4.; // Note [Picked MSDF parameters]
    pub const MAX_MSDF_GLYPH_SCALE   : f64 = 2.; // Note [Picked MSDF parameters]

    pub const MSDF_PARAMS : MsdfParameters = MsdfParameters {
        width                         : MsdfTexture::WIDTH,
        height                        : MsdfTexture::ONE_GLYPH_HEIGHT,
        edge_coloring_angle_threshold : 3.0,   // Note [Picked MSDF parameters]
        range                         : Self::MAX_MSDF_SHRINK_FACTOR * Self::MAX_MSDF_GLYPH_SCALE,
        max_scale                     : Self::MAX_MSDF_GLYPH_SCALE,
        edge_threshold                : 1.001, // Note [Picked MSDF parameters]
        overlap_support               : true   // Note [Picked MSDF parameters]
    };

    /* Note [Picked MSDF parameters]
     *
     * The range was picked such way, that we avoid fitting range in one rendered pixel.
     * Otherwise the antialiasing won't work. I assumed some maximum `shrink factor` (how many
     * times rendered square will be smaller than MSDF size), and pick an arbitrary maximum glyph
     * scale up.
     *
     * The rest of parameters are the defaults taken from msdfgen library
     */

    /// Create render info based on font data in memory
    pub fn new(name:String, font_data:&[u8]) -> FontRenderInfo {
        FontRenderInfo {
            name,
            msdf_sys_font : msdf_sys::Font::load_from_memory(font_data),
            msdf_texture  : MsdfTexture { data : Vec::new() },
            glyphs        : HashMap::new(),
            kerning       : HashMap::new()
        }
    }

    /// Create render info for one of embedded fonts
    pub fn from_embedded(base:&EmbeddedFonts, name:&str)
    -> Option<FontRenderInfo> {
        let font_data_opt = base.font_data_by_name.get(name);
        font_data_opt.map(|data| FontRenderInfo::new(name.to_string(),data))
    }

    /// Load char render info
    pub fn load_char(&mut self, ch:char) {
        let handle              = &self.msdf_sys_font;
        let unicode             = ch as u32;
        let params              = Self::MSDF_PARAMS;
        let msdf_height         = MsdfTexture::ONE_GLYPH_HEIGHT;
        let msdf_tex_rows_begin = self.msdf_texture.rows();
        let msdf_tex_rows_end   = msdf_tex_rows_begin + msdf_height;

        let msdf                = MultichannelSignedDistanceField::generate(handle,unicode,&params);
        let msdf_transformation = convert_msdf_transformation(&msdf);
        let advance             = x_distance_from_msdf_value(msdf.advance);
        let glyph_info = GlyphRenderInfo {
            msdf_texture_rows     : msdf_tex_rows_begin..msdf_tex_rows_end,
            from_base_layout      : msdf_transformation.inverse(),
            advance
        };
        self.msdf_texture.extend(msdf.data.iter());
        self.glyphs.insert(ch, glyph_info);
    }

    /// Get render info for one character, generating one if not found
    pub fn get_glyph_info(&mut self, ch:char) -> &GlyphRenderInfo {
        if !self.glyphs.contains_key(&ch) {
            self.load_char(ch);
        }
        self.glyphs.get(&ch).unwrap()
    }

    /// Get kerning between two characters
    pub fn get_kerning(&mut self, left : char, right : char) -> f64 {
        match self.kerning.entry((left,right)) {
            Occupied(entry) => *entry.get(),
            Vacant(entry)   => {
                let msdf_val   = self.msdf_sys_font.retrieve_kerning(left, right);
                let normalized = x_distance_from_msdf_value(msdf_val);
                *entry.insert(normalized)
            }
        }
    }

    #[cfg(test)]
    pub fn mock_font(name : String) -> FontRenderInfo {
        FontRenderInfo {
            name,
            msdf_sys_font : msdf_sys::Font::mock_font(),
            msdf_texture  : MsdfTexture { data : Vec::new() },
            glyphs        : HashMap::new(),
            kerning       : HashMap::new()
        }
    }

    #[cfg(test)]
    pub fn mock_char_info(&mut self, ch : char) -> &mut GlyphRenderInfo {
        let msdf_height             = MsdfTexture::ONE_GLYPH_HEIGHT;
        let msdf_texture_rows_begin = self.msdf_texture.rows();
        let msdf_texture_rows_end   = msdf_texture_rows_begin + msdf_height;
        let data_size               = MsdfTexture::ONE_GLYPH_SIZE;
        let msdf_data               = (0..data_size).map(|_| 0.12345);

        let char_info = GlyphRenderInfo {
            msdf_texture_rows     : (msdf_texture_rows_begin..msdf_texture_rows_end),
            from_base_layout      : nalgebra::Transform::identity(),
            advance               : 0.0
        };
        self.msdf_texture.extend(msdf_data);
        self.glyphs.insert(ch, char_info);
        self.glyphs.get_mut(&ch).unwrap()
    }

    #[cfg(test)]
    pub fn mock_kerning_info(&mut self, l : char, r : char, value : f64) {
        self.kerning.insert((l, r),value);
    }
}


// ===================
// === LoadedFonts ===
// ===================

/// A handle for fonts loaded into memory.
pub type FontId = usize;

/// Structure keeping all fonts loaded from different sources.
#[derive(Debug)]
pub struct Fonts {
    embedded : EmbeddedFonts,
    fonts    : HashMap<FontId,FontRenderInfo>,
    next_id  : FontId
}

impl Fonts {
    /// Create empty `Fonts` structure (however it contains raw data of embedded fonts).
    pub fn new() -> Fonts {
        Fonts {
            embedded : EmbeddedFonts::create_and_fill(),
            fonts    : HashMap::new(),
            next_id  : 1
        }
    }

    /// Load data from one of embedded fonts. Returns None if embedded font not found.
    pub fn load_embedded_font(&mut self, name:&str) -> Option<FontId> {
        let render_info = FontRenderInfo::from_embedded(&self.embedded,name);
        render_info.map(|info| self.put_render_info(info))
    }

    fn put_render_info(&mut self, font:FontRenderInfo) -> FontId {
        let id = self.next_id;
        self.fonts.insert(id,font);
        self.next_id += 1;
        id
    }

    /// Get render info of one of loaded fonts. Panics for unrecognized id - you should only use
    /// ids returned from `Fonts`' functions.
    pub fn get_render_info(&mut self, id:FontId) -> &mut FontRenderInfo {
        self.fonts.get_mut(&id).unwrap()
    }
}

impl Default for Fonts {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::text::msdf::MsdfTexture;
    use basegl_core_msdf_sys as msdf_sys;
    use basegl_core_embedded_fonts::EmbeddedFonts;
    use wasm_bindgen_test::{wasm_bindgen_test, wasm_bindgen_test_configure};
    use msdf_sys::test_utils::TestAfterInit;
    use std::future::Future;

    const TEST_FONT_NAME : &str = "DejaVuSansMono-Bold";

    fn create_test_font_render_info() -> FontRenderInfo {
        let mut embedded_fonts = EmbeddedFonts::create_and_fill();
        FontRenderInfo::from_embedded(
            &mut embedded_fonts,
            TEST_FONT_NAME
        ).unwrap()
    }

    wasm_bindgen_test_configure!(run_in_browser);

    #[wasm_bindgen_test(async)]
    fn empty_font_render_info() -> impl Future<Output=()> {
        TestAfterInit::schedule(||{
            let font_render_info = create_test_font_render_info();

            assert_eq!(TEST_FONT_NAME, font_render_info.name);
            assert_eq!(0, font_render_info.msdf_texture.data.len());
            assert_eq!(0, font_render_info.glyphs.len());
        })
    }

    #[wasm_bindgen_test(async)]
    fn loading_chars() -> impl Future<Output=()> {
        TestAfterInit::schedule(|| {
            let mut font_render_info = create_test_font_render_info();

            font_render_info.load_char('A');
            font_render_info.load_char('B');

            let chars      = 2;
            let tex_width  = MsdfTexture::WIDTH;
            let tex_height = MsdfTexture::ONE_GLYPH_HEIGHT * chars;
            let channels   = MultichannelSignedDistanceField::CHANNELS_COUNT;
            let tex_size   = tex_width * tex_height * channels;

            assert_eq!(tex_height , font_render_info.msdf_texture.rows());
            assert_eq!(tex_size   , font_render_info.msdf_texture.data.len());
            assert_eq!(chars      , font_render_info.glyphs.len());

            let first_char  = font_render_info.glyphs.get(&'A').unwrap();
            let second_char = font_render_info.glyphs.get(&'B').unwrap();

            let first_range  = 0..MsdfTexture::ONE_GLYPH_HEIGHT;
            let second_range = MsdfTexture::ONE_GLYPH_HEIGHT..tex_height;

            assert_eq!(first_range  , first_char.msdf_texture_rows);
            assert_eq!(second_range , second_char.msdf_texture_rows);
        })
    }

    #[wasm_bindgen_test(async)]
    fn getting_or_creating_char() -> impl Future<Output=()> {
        TestAfterInit::schedule(|| {
            let mut font_render_info = create_test_font_render_info();

            {
                let char_info = font_render_info.get_glyph_info('A');
                assert_eq!(0..MsdfTexture::WIDTH, char_info.msdf_texture_rows);
            }
            assert_eq!(1, font_render_info.glyphs.len());

            {
                let char_info = font_render_info.get_glyph_info('A');
                assert_eq!(0..MsdfTexture::WIDTH, char_info.msdf_texture_rows);
            }
            assert_eq!(1, font_render_info.glyphs.len());
        })
    }
}
