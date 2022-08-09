/// Commonly used types and functions.
pub mod prelude {
    pub use ensogl_core::prelude::*;
}

use prelude::*;

use ensogl_text_msdf_sys as msdf_sys;
use msdf_sys::Msdf;
use msdf_sys::MsdfParameters;
use owned_ttf_parser as ttf;
use owned_ttf_parser::AsFaceRef;



// ==============
// === Export ===
// ==============

pub mod msdf;



// =============
// === Cache ===
// =============

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct Cache<K: Eq + Hash, V> {
    map: RefCell<HashMap<K, V>>,
}

impl<K: Eq + Hash, V: Copy> Cache<K, V> {
    pub fn get_or_create<F>(&self, key: K, constructor: F) -> V
    where F: FnOnce() -> V {
        *self.map.borrow_mut().entry(key).or_insert_with(|| constructor())
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
/// (0.0, 0.0) is initial pen position for a character, and `y` = 1.0 is an _ascender_.
///
/// The `offset` and `scale` fields transforms the _base square_ for a character, such the glyph
/// will be rendered correctly with assigned MSDF texture. The _base square_ corners are (0.0, 0.0),
/// (1.0, 1.0).
///
/// For explanation of various font-rendering terms, see the
/// [freetype documentation](https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html#section-1)
#[derive(Copy, Clone, Debug, serde::Serialize, serde::Deserialize)]
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
    /// Otherwise, the antialiasing won't work. I assumed some maximum `shrink factor` (how many
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

    /// Load new [`GlyphRenderInfo`] from msdf_sys font handle. This also extends the atlas with
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

const FONT_FACE_NUMBER: u32 = 0;

/// Internal representation of `Font`.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct FontData {
    pub name:  String,
    // FIXME: Contains the binary font data. This field should not be needed when it would be
    //        possible to access the font data from within the `ttf::OwnedFace` structure.
    //        See: https://github.com/RazrFalcon/ttf-parser/issues/103
    data:      Vec<u8>,
    msdf_font: Option<msdf_sys::Font>,
    font_face: ttf::OwnedFace,
    atlas:     msdf::Texture,
    glyphs:    Cache<char, GlyphRenderInfo>,
    /// Kerning is also available in the `font_face` structure, but accessing it is slower than via
    /// a cache.
    kerning:   Cache<(char, char), f32>,
}

impl FontData {
    /// Removes all glyph info from the font definition, leaving helper tables, such as the kerning
    /// table. The glyph atlas is not removed. The main purpose of this function is to prevent
    /// extraction of the font files from the application, while still allowing rendering the glyphs
    /// to the screen. You can think of the glyph atlas like a set of glyph images. This allows
    /// usage of commercial fonts without the need to distribute the font with the application, and
    /// thus, it allows for more flexibility in font licensing (according to our font provider).
    ///
    /// The [`msdf_font`] definition is removed, which means that no new glyphs can be added to the
    /// atlas after this operation. Moreover, all glyph shape data is erased from the [`font_face`]
    /// definition, which means that the exact glyph shapes could not be reconstructed anymore.
    ///
    /// To learn more about the tables being removed, please refer to the `ttf-parser` documentation
    /// at https://docs.rs/ttf-parser/0.15.2/ttf_parser/index.html#modules.
    pub fn remove_glyph_data(&mut self) {
        mem::take(&mut self.msdf_font);
        let data_slice = &self.data[..];
        let data_ptr = data_slice.as_ptr();
        if let Ok(font_face) = ttf::RawFace::from_slice(data_slice, FONT_FACE_NUMBER) {
            let tags_to_remove =
                [b"CBDT", b"CBLC", b"CFF ", b"CFF2", b"fvar", b"gvar", b"sbix", b"SVG ", b"glyf"];
            let mut ranges_to_remove = tags_to_remove
                .into_iter()
                .filter_map(|tag| {
                    font_face.table(ttf::Tag::from_bytes(tag)).map(|glyph_table| {
                        let glyph_table_ptr = glyph_table.as_ptr();
                        // Safety: This is safe, as both pointers refer to the same slice.
                        let start_index = unsafe { glyph_table_ptr.offset_from(data_ptr) } as usize;
                        start_index..(start_index + glyph_table.len())
                    })
                })
                .sorted_by_key(|t| t.start)
                .into_iter();
            let mut range_to_remove = ranges_to_remove.next();
            let mut index = 0;
            self.data.retain(|_| {
                let mut keep = true;
                if let Some(range) = &range_to_remove {
                    let in_range = range.contains(&index);
                    let next_in_range = range.contains(&(index + 1));
                    if in_range && !next_in_range {
                        range_to_remove = ranges_to_remove.next();
                    }
                    keep = !in_range;
                }
                index += 1;
                keep
            });
        }
        if let Ok(font_face) = ttf::OwnedFace::from_vec(self.data.clone(), FONT_FACE_NUMBER) {
            self.font_face = font_face;
        }
    }
}

impl Font {
    /// Constructor.
    pub fn from_msdf_font(
        name: String,
        msdf_font: msdf_sys::Font,
        font_face: ttf::OwnedFace,
        data: Vec<u8>,
    ) -> Self {
        let atlas = default();
        let glyphs = default();
        let kerning = default();
        let msdf_font = Some(msdf_font);
        FontData { name, data, msdf_font, font_face, atlas, glyphs, kerning }.into()
    }

    /// Constructor.
    pub fn from_raw_data(name: String, font_data: &[u8]) -> Result<Self, ttf::FaceParsingError> {
        ttf::OwnedFace::from_vec(font_data.into(), FONT_FACE_NUMBER).map(|face| {
            let data = font_data.into();
            Self::from_msdf_font(name, msdf_sys::Font::load_from_memory(font_data), face, data)
        })
    }



    /// Get render info for one character, generating one if not found.
    pub fn glyph_info(&self, ch: char) -> GlyphRenderInfo {
        let handle = &self.msdf_font;
        self.glyphs.get_or_create(ch, move || {
            // FIXME: display default image for missing glyph.
            GlyphRenderInfo::load(handle.as_ref().unwrap(), ch, &self.atlas)
        })
    }

    /// Get kerning between two characters.
    pub fn kerning(&self, left: char, right: char) -> f32 {
        let opt_kerning = self.font_face.as_face_ref().glyph_index(left).and_then(|left_id| {
            self.font_face.as_face_ref().glyph_index(right).map(|right_id| {
                self.kerning.get_or_create((left, right), || {
                    let tables = self.font_face.as_face_ref().tables();
                    let units_per_em = tables.head.units_per_em;
                    let kern_table = tables.kern.and_then(|t| t.subtables.into_iter().next());
                    let kerning = kern_table.and_then(|t| t.glyphs_kerning(left_id, right_id));
                    kerning.unwrap_or_default() as f32 / units_per_em as f32
                })
            })
        });
        opt_kerning.unwrap_or_default()
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



// ================================
// === FontDataWithoutGlyphInfo ===
// ================================

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct FontDataWithoutGlyphInfo {
    pub name: String,
    data:     Vec<u8>,
    atlas:    msdf::Texture,
    glyphs:   Cache<char, GlyphRenderInfo>,
    kerning:  Cache<(char, char), f32>,
}

impl From<FontData> for FontDataWithoutGlyphInfo {
    fn from(mut font_data: FontData) -> Self {
        font_data.remove_glyph_data();
        let name = font_data.name;
        let data = font_data.data;
        let atlas = font_data.atlas;
        let glyphs = font_data.glyphs;
        let kerning = font_data.kerning;
        FontDataWithoutGlyphInfo { name, data, atlas, glyphs, kerning }
    }
}

impl From<FontDataWithoutGlyphInfo> for FontData {
    fn from(mut font_data: FontDataWithoutGlyphInfo) -> Self {
        let name = font_data.name;
        let data = font_data.data;
        let atlas = font_data.atlas;
        let glyphs = font_data.glyphs;
        let kerning = font_data.kerning;
        let msdf_font = None;
        let err = "Internal error. The font data cannot be reconstructed.";
        let font_face = ttf::OwnedFace::from_vec(data.clone(), FONT_FACE_NUMBER).expect(err);
        FontData { name, data, atlas, glyphs, kerning, msdf_font, font_face }
    }
}
