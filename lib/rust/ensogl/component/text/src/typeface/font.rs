//! In this module we handle the fonts information required for rendering glyphs.

use crate::prelude::*;

use enso_shapely::shared;
use ensogl_core::display::scene;
use ensogl_core::display::Scene;
use ensogl_text_embedded_fonts as embedded_fonts;
use ensogl_text_embedded_fonts::EmbeddedFontsData;
use ensogl_text_embedded_fonts::FontFamilyDefinition;
use ensogl_text_embedded_fonts::Name;
use ensogl_text_embedded_fonts::NonVariableFontFaceHeader;
use ensogl_text_embedded_fonts::NonVariableFontFamilyDefinition;
use ensogl_text_embedded_fonts::VariableFontFamilyDefinition;
use ensogl_text_msdf_sys as msdf_sys;
use msdf_sys::Msdf;
use msdf_sys::MsdfParameters;
use ordered_float::NotNan;
use owned_ttf_parser as ttf;
use owned_ttf_parser::AsFaceRef;
use owned_ttf_parser::GlyphId;
use owned_ttf_parser::Style;
use owned_ttf_parser::Tag;
use owned_ttf_parser::Weight;
use owned_ttf_parser::Width;
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
    font_family_definitions: HashMap<Name, FontFamilyDefinition>,
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
    fonts:       HashMap<Name,Font>,
}

impl {

    /// Load a font by name. The font can be loaded either from cache or from the embedded fonts'
    /// registry if not used before. Returns None if the name is missing in both cache and embedded
    /// font list.
    pub fn load(&mut self, name:impl Into<Name>) -> Font {
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
                        NonVariableFont::new(name, definition, self.font_loader.clone_ref()).into(),
                    FontFamilyDefinition::Variable(definition) =>
                        VariableFont::new(name, definition, self.font_loader.clone_ref()).into(),
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


/// TTF files can contain multiple face definitions. We support only the first defined, just as
/// most web browsers (you cannot define `@font-face` in CSS for multiple faces of the same file).
const FONT_FACE_NUMBER: u32 = 0;



pub trait FaceLoader<Variations> {
    fn get_or_load_face<F>(&self, variations: &Variations, loader: &FontLoader, f: F)
    where F: for<'a> FnOnce(&'a FontFace);
}

#[derive(Debug)]
pub struct FontFace {
    pub msdf: msdf_sys::Font,
    pub ttf:  ttf::OwnedFace,
}

#[derive(Debug)]
pub struct NonVariableFontFamily {
    pub definition: NonVariableFontFamilyDefinition,
    pub faces:      Rc<RefCell<HashMap<NonVariableFontFaceHeader, FontFace>>>,
}

impl From<NonVariableFontFamilyDefinition> for NonVariableFontFamily {
    fn from(definition: NonVariableFontFamilyDefinition) -> Self {
        Self { definition, faces: default() }
    }
}

impl FaceLoader<NonVariableFontFaceHeader> for NonVariableFontFamily {
    fn get_or_load_face<F>(
        &self,
        variations: &NonVariableFontFaceHeader,
        loader: &FontLoader,
        f: F,
    ) where
        F: for<'a> FnOnce(&'a FontFace),
    {
        if self.faces.borrow().contains_key(&variations) {
            if let Some(face) = self.faces.borrow().get(&variations) {
                f(face);
            }
        } else {
            let opt_face = self.definition.map.get(&variations).and_then(|file_name| {
                // FIXME conversion
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
                self.faces.borrow_mut().insert(*variations, face);
                f(self.faces.borrow().get(variations).unwrap());
            }
        }
    }
}

#[derive(Debug)]
pub struct VariableFontFamily {
    pub definition: VariableFontFamilyDefinition,
    pub face:       Rc<RefCell<Option<FontFace>>>,
}

impl From<VariableFontFamilyDefinition> for VariableFontFamily {
    fn from(definition: VariableFontFamilyDefinition) -> Self {
        Self { definition, face: default() }
    }
}

impl<T> FaceLoader<T> for VariableFontFamily {
    fn get_or_load_face<F>(&self, _variations: &T, loader: &FontLoader, f: F)
    where F: for<'a> FnOnce(&'a FontFace) {
        if self.face.borrow().is_some() {
            if let Some(face) = self.face.borrow().as_ref() {
                f(face);
            }
        } else {
            let x: &str = &self.definition.file;
            let opt_face =
                loader.rc.borrow().embedded_fonts_data.data.get(x).and_then(|font_data| {
                    let result = ttf::OwnedFace::from_vec((**font_data).into(), FONT_FACE_NUMBER)
                        .map(|ttf| {
                            let msdf = msdf_sys::Font::load_from_memory(font_data);
                            FontFace { msdf, ttf }
                        });
                    result.map_err(|err| event!(ERROR, "Error parsing font: {}", err)).ok()
                });

            if let Some(face) = opt_face {
                *self.face.borrow_mut() = Some(face);
                f(self.face.borrow().as_ref().unwrap());
            }
        }
    }
}

#[derive(Debug)]
pub enum FontFamily {
    Variable(VariableFontFamily),
    NonVariable(NonVariableFontFamily),
}


// ====================
// === VariableFont ===
// ====================

/// https://docs.microsoft.com/en-us/typography/opentype/spec/dvaraxisreg#registered-axis-tags
#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct VariationAxes {
    pub ital:         NotNan<f32>,
    pub opsz:         NotNan<f32>,
    pub slnt:         NotNan<f32>,
    pub wght:         NotNan<f32>,
    pub wdth:         NotNan<f32>,
    pub non_standard: Vec<(Tag, NotNan<f32>)>,
}

impl VariationAxes {
    /// https://fonts.google.com/knowledge/glossary/weight_axis
    pub fn set_weight(&mut self, value: Weight) {
        self.wght = value.to_number().into();
    }

    /// https://fonts.google.com/knowledge/glossary/width_axis
    pub fn set_width(&mut self, value: Width) {
        let wdth = match value {
            Width::UltraCondensed => 25.0,
            Width::ExtraCondensed => 43.75,
            Width::Condensed => 62.5,
            Width::SemiCondensed => 81.25,
            Width::Normal => 100.0,
            Width::SemiExpanded => 118.75,
            Width::Expanded => 137.5,
            Width::ExtraExpanded => 156.25,
            Width::UltraExpanded => 175.0,
        };
        self.wdth = NotNan::new(wdth).unwrap();
    }

    /// https://fonts.google.com/knowledge/glossary/italic_axis
    /// https://fonts.google.com/knowledge/glossary/slant_axis
    pub fn set_style(&mut self, value: Style) {
        match value {
            Style::Normal => {
                self.ital = 0_u16.into();
                self.slnt = 0_u16.into();
            }
            Style::Italic => {
                self.ital = 1_u16.into();
                self.slnt = 0_u16.into();
            }
            Style::Oblique => {
                self.ital = 0_u16.into();
                self.slnt = 90_u16.into();
            }
        }
    }
}



// ============
// === Font ===
// ============

#[derive(Debug, Clone, CloneRef, From)]
pub enum Font {
    NonVariable(NonVariableFont),
    Variable(VariableFont),
}

pub type NonVariableFont = FontTemplate<NonVariableFontFamily, NonVariableFontFaceHeader>;
pub type VariableFont = FontTemplate<VariableFontFamily, VariationAxes>;


impl Font {
    pub fn possible_weights(&self) -> Option<Vec<Weight>> {
        match self {
            Font::NonVariable(font) => Some(font.family.definition.possible_weights()),
            Font::Variable(_) => None,
        }
    }

    pub fn get_or_load_glyph_info(
        &self,
        non_variable_font_variations: NonVariableFontFaceHeader,
        variable_font_variations: &VariationAxes,
        glyph_id: GlyphId,
        f: impl FnOnce(GlyphRenderInfo),
    ) {
        match self {
            Font::NonVariable(font) =>
                font.get_or_load_glyph_info(&non_variable_font_variations, glyph_id, f),
            Font::Variable(font) =>
                font.get_or_load_glyph_info(variable_font_variations, glyph_id, f),
        }
    }

    pub fn glyph_id_of_code_point(
        &self,
        non_variable_font_variations: NonVariableFontFaceHeader,
        variable_font_variations: &VariationAxes,
        code_point: char,
        f: impl FnOnce(Option<GlyphId>),
    ) {
        match self {
            Font::NonVariable(font) =>
                font.glyph_id_of_code_point(&non_variable_font_variations, code_point, f),
            Font::Variable(font) =>
                font.glyph_id_of_code_point(variable_font_variations, code_point, f),
        }
    }

    pub fn msdf_texture_rows(&self) -> usize {
        match self {
            Font::NonVariable(font) => font.msdf_texture_rows(),
            Font::Variable(font) => font.msdf_texture_rows(),
        }
    }

    pub fn with_borrowed_msdf_texture_data<R>(&self, operation: impl FnOnce(&[u8]) -> R) -> R {
        match self {
            Font::NonVariable(font) => font.with_borrowed_msdf_texture_data(operation),
            Font::Variable(font) => font.with_borrowed_msdf_texture_data(operation),
        }
    }
}


// ====================
// === FontTemplate ===
// ====================

/// A single font data used for rendering.
///
/// The data for individual characters and kerning are load on demand.
///
/// Each distance and transformation values are expressed in normalized coordinates, where `y` = 0.0
/// is _baseline_ and `y` = 1.0 is _ascender_. For explanation of various font-rendering terms, see
/// [freetype documentation](https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html#section-1)

// #[derive(Debug, Clone, CloneRef, Deref)]
// pub struct NonVariableFont {
//     rc: Rc<NonVariableFontData>,
// }

#[derive(Debug, Default)]
pub struct FontDataCache {
    kerning: HashMap<(GlyphId, GlyphId), f32>,
    glyphs:  HashMap<GlyphId, GlyphRenderInfo>,
}

#[derive(Deref, Derivative, CloneRef, Debug)]
#[derivative(Clone(bound = ""))]
pub struct FontTemplate<Family, Variations> {
    rc: Rc<FontDataTemplate<Family, Variations>>,
}

#[derive(Debug)]
#[allow(missing_docs)]
pub struct FontDataTemplate<Family, Variations> {
    pub name:               Name,
    family:                 Family,
    atlas:                  msdf::Texture,
    cache:                  RefCell<HashMap<Variations, FontDataCache>>,
    loader:                 FontLoader,
    // FIXME: remove after MSDF-gen API will be updated to handle GlyphIds.
    glyph_id_to_code_point: RefCell<HashMap<GlyphId, char>>,
}

impl<F, V> From<FontDataTemplate<F, V>> for FontTemplate<F, V> {
    fn from(t: FontDataTemplate<F, V>) -> Self {
        let rc = Rc::new(t);
        Self { rc }
    }
}

impl<F: FaceLoader<V>, V: Eq + Hash + Clone> FontTemplate<F, V> {
    /// Constructor.
    pub fn new(name: Name, family: impl Into<F>, loader: FontLoader) -> Self {
        let atlas = default();
        let cache = default();
        let family = family.into();
        let glyph_id_to_code_point = default();
        FontDataTemplate { name, family, atlas, cache, loader, glyph_id_to_code_point }.into()
    }


    pub fn glyph_id_of_code_point(
        &self,
        variations: &V,
        code_point: char,
        f: impl FnOnce(Option<GlyphId>),
    ) {
        self.family.get_or_load_face(variations, &self.loader, |face| {
            let id = face.ttf.as_face_ref().glyph_index(code_point);
            if let Some(id) = id {
                self.glyph_id_to_code_point.borrow_mut().insert(id, code_point);
            }
            f(id)
        })
    }

    /// Get render info for one character, generating one if not found.
    pub fn get_or_load_glyph_info(
        &self,
        variations: &V,
        glyph_id: GlyphId,
        f: impl FnOnce(GlyphRenderInfo),
    ) {
        let opt_render_info =
            self.cache.borrow().get(variations).and_then(|t| t.glyphs.get(&glyph_id)).copied();
        if let Some(render_info) = opt_render_info {
            f(render_info);
        } else {
            self.family.get_or_load_face(&variations, &self.loader, |face| {
                // TODO: Switch from chars to GlyphIDs here.
                let ch = *self.glyph_id_to_code_point.borrow().get(&glyph_id).unwrap();
                // TODO: Use variations to generate variable-width glyphs.
                let render_info = GlyphRenderInfo::load(&face.msdf, ch, &self.atlas);
                if !self.cache.borrow().contains_key(variations) {
                    self.cache.borrow_mut().insert(variations.clone(), default());
                }
                self.cache
                    .borrow_mut()
                    .get_mut(variations)
                    .unwrap()
                    .glyphs
                    .insert(glyph_id, render_info);
                f(render_info)
            })
        }
    }

    /// Get kerning between two characters.
    pub fn with_kerning(
        &self,
        variations: &V,
        left_id: GlyphId,
        right_id: GlyphId,
        f: impl FnOnce(f32),
    ) {
        self.family.get_or_load_face(variations, &self.loader, |face| {
            if !self.cache.borrow().contains_key(variations) {
                self.cache.borrow_mut().insert(variations.clone(), default());
            }
            let kerning = *self
                .cache
                .borrow_mut()
                .get_mut(variations)
                .unwrap()
                .kerning
                .entry((left_id, right_id))
                .or_insert_with(|| {
                    let tables = face.ttf.as_face_ref().tables();
                    let units_per_em = tables.head.units_per_em;
                    let kern_table = tables.kern.and_then(|t| t.subtables.into_iter().next());
                    let kerning = kern_table.and_then(|t| t.glyphs_kerning(left_id, right_id));
                    kerning.unwrap_or_default() as f32 / units_per_em as f32
                });
            f(kerning)
        })
    }

    /// A whole MSDF texture bound for this font.
    pub fn with_borrowed_msdf_texture_data<R>(&self, operation: impl FnOnce(&[u8]) -> R) -> R {
        self.atlas.with_borrowed_data(operation)
    }

    /// Get number of rows in MSDF texture.
    pub fn msdf_texture_rows(&self) -> usize {
        self.atlas.rows()
    }

    // #[cfg(test)]
    // pub fn mock(name: impl Into<String>) -> Self {
    //     Self::from_msdf_font(name.into(), msdf_sys::NonVariableFont::mock_font())
    // }
    //
    // #[cfg(test)]
    // pub fn mock_char_info(
    //     &self,
    //     ch: char,
    //     offset: Vector2<f32>,
    //     scale: Vector2<f32>,
    //     advance: f32,
    // ) -> GlyphRenderInfo {
    //     self.glyphs.invalidate(&ch);
    //     let data_size = msdf::Texture::ONE_GLYPH_SIZE;
    //     let msdf_data = (0..data_size).map(|_| 0.12345);
    //     let msdf_texture_glyph_id = self.msdf_texture_rows() / msdf::Texture::ONE_GLYPH_HEIGHT;
    //
    //     self.atlas.extend_with_raw_data(msdf_data);
    //     self.glyphs.get_or_create(ch, move || GlyphRenderInfo {
    //         offset,
    //         scale,
    //         advance,
    //         msdf_texture_glyph_id,
    //     })
    // }
    //
    // #[cfg(test)]
    // pub fn mock_kerning_info(&self, l: char, r: char, value: f32) {
    //     self.kerning.invalidate(&(l, r));
    //     self.kerning.get_or_create((l, r), || value);
    // }
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

    fn create_test_font() -> NonVariableFont {
        let embedded_fonts = EmbeddedFontsData::create_and_fill();
        NonVariableFont::try_from_embedded(&embedded_fonts, TEST_FONT_NAME).unwrap()
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
