//! In this module we handle the fonts information required for rendering glyphs.

use crate::prelude::*;

use enso_shapely::shared;
use ensogl_core::display::scene;
use ensogl_core::display::Scene;
use ensogl_text_embedded_fonts as embedded_fonts;
use ensogl_text_embedded_fonts::EmbeddedFontsData;
use ensogl_text_embedded_fonts::NonVariableFontFamilyDefinition;
// use ensogl_text_embedded_fonts::Family;
use ensogl_text_embedded_fonts::FontFamilyDefinition;
use ensogl_text_embedded_fonts::FontName;
use ensogl_text_embedded_fonts::NonVariableFontFaceHeader;
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
pub const DEFAULT_FONT: &str = "dejavusans"; //embedded_fonts::DefaultFamily::regular();



// ====================
// === RegistryData ===
// ====================

shared! { FontLoader
/// Structure keeping all fonts loaded from different sources.
#[derive(Debug)]
pub struct FontLoaderData {
    font_family_definitions: HashMap<FontName, FontFamilyDefinition>,
    embedded_fonts_data : EmbeddedFontsData,
}

impl {

}}

impl FontLoaderData {
    pub fn init_and_load_embedded_font_data() -> Self {
        let embedded_fonts_data = EmbeddedFontsData::new();
        let font_family_definitions = ensogl_text_embedded_fonts::font_family_files_map().clone();
        Self { embedded_fonts_data, font_family_definitions }
    }
}

impl FontLoader {
    pub fn init_and_load_embedded_font_data() -> Self {
        let data = FontLoaderData::init_and_load_embedded_font_data();
        let rc = Rc::new(RefCell::new(data));
        Self { rc }
    }
}


shared! { Registry
/// Structure keeping all fonts loaded from different sources.
#[derive(Debug)]
pub struct RegistryData {
    font_loader: FontLoader,
    fonts:       HashMap<FontName,Font>,
}

impl {

    /// Load a font by name. The font can be loaded either from cache or from the embedded fonts'
    /// registry if not used before. Returns None if the name is missing in both cache and embedded
    /// font list.
    pub fn load(&mut self, name:impl Into<FontName>) -> Font {
        let name = name.into();
        event!(WARN, "Loading font: {:?}", name);
        match self.fonts.entry(name.clone()) {
            Entry::Occupied (entry) => entry.get().clone_ref(),
            Entry::Vacant   (entry) => {
                    // FIXME: unwrap
                let definition =
                    self.font_loader.rc.borrow().font_family_definitions.get(&name).unwrap().clone();
                match definition {
                    FontFamilyDefinition::NonVariable(definition) =>
                        Font::new(name, definition, self.font_loader.clone_ref()),
                    t => panic!("{:?}", t),
                }
            }
        }
    }
}}

impl RegistryData {
    /// Create empty font `Registry` and load raw data of embedded fonts.
    pub fn init_and_load_embedded_font_data() -> RegistryData {
        let fonts = HashMap::new();
        let font_loader = FontLoader::init_and_load_embedded_font_data();
        Self { font_loader, fonts }
    }
}

impl Registry {
    /// Constructor.
    pub fn init_and_load_embedded_font_data() -> Registry {
        let rc = Rc::new(RefCell::new(RegistryData::init_and_load_embedded_font_data()));
        Self { rc }
    }
}

impl scene::Extension for Registry {
    fn init(_scene: &Scene) -> Self {
        Self::init_and_load_embedded_font_data()
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

/// TTF files can contain multiple face definitions. We support only the first defined, just as
/// most web browsers (you cannot define `@font-face` in CSS for multiple faces of the same file).
const FONT_FACE_NUMBER: u32 = 0;


#[derive(Debug)]
pub struct FontFace {
    pub msdf: msdf_sys::Font,
    pub ttf:  ttf::OwnedFace,
}

#[derive(Debug, Default)]
pub struct NonVariableFontFamily {
    pub faces: HashMap<NonVariableFontFaceHeader, FontFace>,
}

#[derive(Debug)]
pub enum FontFamily {
    Variable(FontFace),
    NonVariable(NonVariableFontFamily),
}


/// Internal representation of `Font`.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct FontData {
    pub name:       FontName,
    pub definition: NonVariableFontFamilyDefinition,
    family:         RefCell<NonVariableFontFamily>,
    atlas:          msdf::Texture,
    glyphs:         Cache<(NonVariableFontFaceHeader, char), GlyphRenderInfo>,
    /// Kerning is also available in the `font_face` structure, but accessing it is slower than via
    /// a cache.
    kerning:        Cache<(char, char), f32>,
    loader:         FontLoader,
}


impl Font {
    /// Constructor.
    pub fn new(
        name: FontName,
        definition: NonVariableFontFamilyDefinition,
        loader: FontLoader,
    ) -> Self {
        let atlas = default();
        let glyphs = default();
        let kerning = default();
        let family = default();
        FontData { name, definition, family, atlas, glyphs, kerning, loader }.into()
    }

    pub fn get_or_load_face<'a, 'b, 'c>(
        header: NonVariableFontFaceHeader,
        family: &'a mut NonVariableFontFamily,
        definition: &'b NonVariableFontFamilyDefinition,
        loader: &'c FontLoader,
        f: impl FnOnce(&'a FontFace),
    ) {
        if family.faces.contains_key(&header) {
            if let Some(face) = family.faces.get(&header) {
                f(face);
            }
        } else {
            let opt_face = definition.map.get(&header).and_then(|file_name| {
                // FIXME conversion + FIXME loader should be prepared for async loading with
                //  a callback
                let x: &str = &*file_name;
                // FIXME: warning when trying to load font not from embedded resources.
                loader.rc.borrow().embedded_fonts_data.data.get(x).and_then(|font_data| {
                    let result = ttf::OwnedFace::from_vec((**font_data).into(), FONT_FACE_NUMBER)
                        .map(|ttf| {
                            let msdf = msdf_sys::Font::load_from_memory(font_data);
                            FontFace { msdf, ttf }
                        });
                    result.map_err(|err| event!(ERROR, "Error parsing font: {}", err)).ok()
                })
            });
            if let Some(face) = opt_face {
                family.faces.insert(header, face);
                f(family.faces.get(&header).unwrap());
            }
        }
    }

    /// Get render info for one character, generating one if not found.
    pub fn with_glyph_info(
        &self,
        header: NonVariableFontFaceHeader,
        ch: char,
        f: impl FnOnce(GlyphRenderInfo),
    ) {
        let opt_render_info = self.glyphs.map.borrow().get(&(header, ch)).copied();
        if let Some(render_info) = opt_render_info {
            f(render_info);
        } else {
            Self::get_or_load_face(
                header,
                &mut self.family.borrow_mut(),
                &self.definition,
                &self.loader,
                |face| {
                    let render_info = GlyphRenderInfo::load(&face.msdf, ch, &self.atlas);
                    self.glyphs.map.borrow_mut().insert((header, ch), render_info);
                    f(render_info)
                },
            )
        }
    }

    /// Get kerning between two characters.
    pub fn with_kerning(
        &self,
        header: NonVariableFontFaceHeader,
        left: char,
        right: char,
        f: impl FnOnce(f32),
    ) {
        Self::get_or_load_face(
            header,
            &mut self.family.borrow_mut(),
            &self.definition,
            &self.loader,
            |face| {
                let opt_kerning = face.ttf.as_face_ref().glyph_index(left).and_then(|left_id| {
                    face.ttf.as_face_ref().glyph_index(right).map(|right_id| {
                        self.kerning.get_or_create((left, right), || {
                            let tables = face.ttf.as_face_ref().tables();
                            let units_per_em = tables.head.units_per_em;
                            let kern_table =
                                tables.kern.and_then(|t| t.subtables.into_iter().next());
                            let kerning =
                                kern_table.and_then(|t| t.glyphs_kerning(left_id, right_id));
                            kerning.unwrap_or_default() as f32 / units_per_em as f32
                        })
                    })
                });
                f(opt_kerning.unwrap_or_default())
            },
        )
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


// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use ensogl_text_embedded_fonts;
    use ensogl_text_embedded_fonts::EmbeddedFontsData;
    // use ensogl_text_embedded_fonts::Family;
    use wasm_bindgen_test::wasm_bindgen_test;
    use wasm_bindgen_test::wasm_bindgen_test_configure;

    const TEST_FONT_NAME: &str = embedded_fonts::DefaultFamily::mono_bold();

    fn create_test_font() -> Font {
        let embedded_fonts = EmbeddedFontsData::create_and_fill();
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
