//! In this module we handle the fonts' information required for rendering glyphs.

use crate::prelude::*;

use enso_shapely::shared;
use ensogl_core::display::scene;
use ensogl_core::display::Scene;
use ensogl_text_embedded_fonts::Embedded;
use ensogl_text_embedded_fonts::FamilyDefinition;
use ensogl_text_embedded_fonts::Name;
use ensogl_text_embedded_fonts::NonVariableFaceHeader;
use ensogl_text_embedded_fonts::NonVariableFamilyDefinition;
use ensogl_text_embedded_fonts::VariableFamilyDefinition;
use ensogl_text_msdf as msdf;
use ordered_float::NotNan;
use owned_ttf_parser as ttf;
use owned_ttf_parser::AsFaceRef;
use owned_ttf_parser::GlyphId;
use owned_ttf_parser::Style;
use owned_ttf_parser::Tag;
use owned_ttf_parser::Weight;
use owned_ttf_parser::Width;
use std::collections::hash_map::Entry;

pub mod glyph;
pub mod glyph_render_info;
pub mod pen;

pub use glyph_render_info::GlyphRenderInfo;



// =================
// === Constants ===
// =================

/// TTF files can contain multiple face definitions. We support only the first defined, just as
/// most web browsers (you cannot define `@font-face` in CSS for multiple faces of the same file).
const FONT_FACE_INDEX: u32 = 0;

/// Default non-monospace font the app will revert to if a desired font could not be loaded.
pub const DEFAULT_FONT: &str = "default";

/// Default monospace font the app will revert to if a desired font could not be loaded.
pub const DEFAULT_FONT_MONO: &str = "default-mono";



// =====================
// === VariationAxes ===
// =====================

/// Variation axes of variable fonts. Contains five common axes and a general way of storing
/// non-common ones. The axe name is [`Tag`], which is a 4-bytes identifier constructed from the axe
/// name, e.g. by `Tag::from_bytes(b"ital")`. See the following link to learn more:
/// https://docs.microsoft.com/en-us/typography/opentype/spec/dvaraxisreg#registered-axis-tags
#[allow(missing_docs)]
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
    /// Weight setter. See the following docs to learn more:
    /// https://fonts.google.com/knowledge/glossary/weight_axis
    pub fn set_weight(&mut self, value: Weight) {
        self.wght = value.to_number().into();
    }

    /// Width setter. See the following docs to learn more:
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

    /// Style setter. See the following docs to learn more:
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



// ================
// === Registry ===
// ================

shared! { Registry
/// Structure keeping all fonts loaded from different sources.
#[derive(Debug)]
pub struct RegistryData {
    embedded: Embedded,
    fonts:          HashMap<Name,Font>,
}

impl {
    pub fn load_default(&mut self) -> Font {
        self.load(DEFAULT_FONT)
    }

    /// Load a font by name. The font can be loaded either from cache or from the embedded fonts'
    /// registry if not used before. Returns the default font if the name is missing in both cache
    /// and embedded font list.
    pub fn load(&mut self, name:impl Into<Name>) -> Font {
        let name = name.into();
        self.try_load(&name).unwrap_or_else(|| {
            event!(WARN, "Font '{name}' not found. Loading the default font.");
            self.try_load(DEFAULT_FONT).expect("Default font not found.")
        })
    }

    /// Load a font by name. The font can be loaded either from cache or from the embedded fonts'
    /// registry if not used before. Returns [`None`] if the name is missing in both cache and
    /// embedded font list.
    pub fn try_load(&mut self, name:impl Into<Name>) -> Option<Font> {
        let name = name.into();
        event!(DEBUG, "Loading font: {:?}", name);
        match self.fonts.entry(name.clone()) {
            Entry::Occupied (entry) => Some(entry.get().clone_ref()),
            Entry::Vacant   (entry) => {
                self.embedded.definitions.get(&name).map(|definition| {
                    let font: Font = match definition {
                        FamilyDefinition::NonVariable(definition) => {
                            let family = NonVariableFamily::from(definition);
                            family.load_all_faces(&self.embedded);
                            NonVariableFont::new(name, family).into()
                        }
                        FamilyDefinition::Variable(definition) => {
                            let family = VariableFamily::from(definition);
                            family.load_all_faces(&self.embedded);
                            VariableFont::new(name, family).into()
                        }
                    };
                    entry.insert(font.clone_ref());
                    font
                })
            }
        }
    }
}}

impl Registry {
    /// Constructor.
    pub fn init_and_load_embedded_fonts() -> Registry {
        let embedded = Embedded::init_and_load_embedded_fonts();
        let fonts = HashMap::new();
        let data = RegistryData { embedded, fonts };
        let rc = Rc::new(RefCell::new(data));
        Self { rc }
    }
}

impl scene::Extension for Registry {
    fn init(_scene: &Scene) -> Self {
        Self::init_and_load_embedded_fonts()
    }
}



// ============
// === Face ===
// ============

/// A face of a font. In case of non-variable fonts, a face corresponds to a font variation defined
/// as a triple (width, weight, style), see [`NonVariableFaceHeader`]. In case of variable
/// fonts, the font variation ([`VariationAxes`]) is set up at runtime, so only one face is needed.
/// The face consists of a ttf face and MSDF one.
#[allow(missing_docs)]
#[derive(Debug)]
pub struct Face {
    pub msdf: msdf::OwnedFace,
    pub ttf:  ttf::OwnedFace,
}

impl Face {
    /// Load the font face from memory. Corrupted faces will be reported.
    fn load_from_memory(name: &str, embedded: &Embedded) -> Option<Face> {
        embedded.data.get(name).and_then(|data| {
            let result = ttf::OwnedFace::from_vec((**data).into(), FONT_FACE_INDEX).map(|ttf| {
                let msdf = msdf::OwnedFace::load_from_memory(data);
                Face { msdf, ttf }
            });
            result.map_err(|err| event!(ERROR, "Error parsing font: {}", err)).ok()
        })
    }
}



// ==============
// === Family ===
// ==============

/// A generalization of a font family, a set of font faces. Allows borrowing a font face based on
/// variations. For non-variable fonts, variations is a triple (width, weight, style), see
/// [`NonVariableFaceHeader`] to learn more. For variable faces the variation is [`VariationAxes`],
/// however, as variable fonts have one face only, this parameter is not used while borrowing the
/// face.
#[allow(missing_docs)]
pub trait Family<Variations> {
    fn with_borrowed_face<F, T>(&self, variations: &Variations, f: F) -> T
    where F: for<'a> FnOnce(Option<&'a Face>) -> T;
}

/// A non-variable font family. Contains font family definition and a mapping from font variations
/// to font faces.
#[allow(missing_docs)]
#[derive(Debug)]
pub struct NonVariableFamily {
    pub definition: NonVariableFamilyDefinition,
    pub faces:      Rc<RefCell<HashMap<NonVariableFaceHeader, Face>>>,
}

/// A variable font family. Contains font family definition and the font face.
#[allow(missing_docs)]
#[derive(Debug)]
pub struct VariableFamily {
    pub definition: VariableFamilyDefinition,
    pub face:       Rc<RefCell<Option<Face>>>,
}

impl NonVariableFamily {
    /// Load all font faces from the embedded font data. Corrupted faces will be reported and
    /// ignored.
    fn load_all_faces(&self, embedded: &Embedded) {
        for (header, file_name) in &self.definition.map {
            if let Some(face) = Face::load_from_memory(&*file_name, embedded) {
                self.faces.borrow_mut().insert(*header, face);
            }
        }
    }
}

impl VariableFamily {
    /// Load all font faces from the embedded font data. Corrupted faces will be reported and
    /// ignored.
    fn load_all_faces(&self, embedded: &Embedded) {
        if let Some(face) = Face::load_from_memory(&self.definition.file_name, embedded) {
            self.face.borrow_mut().replace(face);
        }
    }
}

impl Family<NonVariableFaceHeader> for NonVariableFamily {
    fn with_borrowed_face<F, T>(&self, variations: &NonVariableFaceHeader, f: F) -> T
    where F: for<'a> FnOnce(Option<&'a Face>) -> T {
        f(self.faces.borrow().get(variations))
    }
}

impl<V> Family<V> for VariableFamily {
    fn with_borrowed_face<F, T>(&self, _variations: &V, f: F) -> T
    where F: for<'a> FnOnce(Option<&'a Face>) -> T {
        f(self.face.borrow().as_ref())
    }
}


impl From<&VariableFamilyDefinition> for VariableFamily {
    fn from(definition: &VariableFamilyDefinition) -> Self {
        let definition = definition.clone();
        Self { definition, face: default() }
    }
}

impl From<&NonVariableFamilyDefinition> for NonVariableFamily {
    fn from(definition: &NonVariableFamilyDefinition) -> Self {
        let definition = definition.clone();
        Self { definition, faces: default() }
    }
}



// ============
// === Font ===
// ============

/// A typeface, commonly referred to as a font. See the documentation of [`FontTemplate`] to learn
/// more.
#[allow(missing_docs)]
#[derive(Debug, Clone, CloneRef, From)]
pub enum Font {
    NonVariable(NonVariableFont),
    Variable(VariableFont),
}

/// A non-variable version of [`Font`].
pub type NonVariableFont = FontTemplate<NonVariableFamily, NonVariableFaceHeader>;

/// A variable version of [`Font`].
pub type VariableFont = FontTemplate<VariableFamily, VariationAxes>;

impl Font {
    /// List all possible weights. In case of variable fonts, [`None`] will be returned.
    pub fn possible_weights(&self) -> Option<Vec<Weight>> {
        match self {
            Font::NonVariable(font) => Some(font.family.definition.possible_weights()),
            Font::Variable(_) => None,
        }
    }

    /// Get render info for one character, generating one if not found.
    pub fn glyph_info(
        &self,
        non_variable_font_variations: NonVariableFaceHeader,
        variable_font_variations: &VariationAxes,
        glyph_id: GlyphId,
    ) -> Option<GlyphRenderInfo> {
        match self {
            Font::NonVariable(font) => font.glyph_info(&non_variable_font_variations, glyph_id),
            Font::Variable(font) => font.glyph_info(variable_font_variations, glyph_id),
        }
    }

    /// Get the glyph id of the provided code point.
    pub fn glyph_id_of_code_point(
        &self,
        non_variable_font_variations: NonVariableFaceHeader,
        variable_font_variations: &VariationAxes,
        code_point: char,
    ) -> Option<GlyphId> {
        match self {
            Font::NonVariable(font) =>
                font.glyph_id_of_code_point(&non_variable_font_variations, code_point),
            Font::Variable(font) =>
                font.glyph_id_of_code_point(variable_font_variations, code_point),
        }
    }

    /// Get number of rows in MSDF texture.
    pub fn msdf_texture_rows(&self) -> usize {
        match self {
            Font::NonVariable(font) => font.msdf_texture_rows(),
            Font::Variable(font) => font.msdf_texture_rows(),
        }
    }

    /// A whole MSDF texture bound for this font.
    pub fn with_borrowed_msdf_texture_data<R>(&self, operation: impl FnOnce(&[u8]) -> R) -> R {
        match self {
            Font::NonVariable(font) => font.with_borrowed_msdf_texture_data(operation),
            Font::Variable(font) => font.with_borrowed_msdf_texture_data(operation),
        }
    }

    /// Get kerning between two characters.
    pub fn kerning(
        &self,
        non_variable_font_variations: NonVariableFaceHeader,
        variable_font_variations: &VariationAxes,
        left_id: GlyphId,
        right_id: GlyphId,
    ) -> f32 {
        match self {
            Font::NonVariable(font) =>
                font.kerning(&non_variable_font_variations, left_id, right_id),
            Font::Variable(font) => font.kerning(variable_font_variations, left_id, right_id),
        }
    }
}



// ====================
// === FontTemplate ===
// ====================

/// Internal representation of [`Font`]. It contains references to the font family definition,
/// a texture with MSDF-encoded glyph shapes, and a cache for common glyph properties, used to
/// layout glyphs.
#[derive(Deref, Derivative, CloneRef, Debug)]
#[derivative(Clone(bound = ""))]
pub struct FontTemplate<Family, Variations> {
    rc: Rc<FontTemplateData<Family, Variations>>,
}

/// Internal representation of [`FontTemplate`].
#[derive(Debug)]
#[allow(missing_docs)]
pub struct FontTemplateData<Family, Variations> {
    pub name:                   Name,
    pub family:                 Family,
    pub atlas:                  msdf::Texture,
    pub cache:                  RefCell<HashMap<Variations, FontDataCache>>,
    // FIXME: remove after MSDF-gen API will be updated to handle GlyphIds.
    pub glyph_id_to_code_point: RefCell<HashMap<GlyphId, char>>,
}

/// A cache for common glyph properties, used to layout glyphs.
#[derive(Debug, Default)]
pub struct FontDataCache {
    kerning: HashMap<(GlyphId, GlyphId), f32>,
    glyphs:  HashMap<GlyphId, GlyphRenderInfo>,
}

impl<F, V> From<FontTemplateData<F, V>> for FontTemplate<F, V> {
    fn from(t: FontTemplateData<F, V>) -> Self {
        let rc = Rc::new(t);
        Self { rc }
    }
}

impl<F: Family<V>, V: Eq + Hash + Clone> FontTemplate<F, V> {
    /// Constructor.
    pub fn new(name: Name, family: impl Into<F>) -> Self {
        let atlas = default();
        let cache = default();
        let family = family.into();
        let glyph_id_to_code_point = default();
        let data = FontTemplateData { name, family, atlas, cache, glyph_id_to_code_point };
        Self { rc: Rc::new(data) }
    }

    /// Get the glyph id of the provided code point.
    pub fn glyph_id_of_code_point(&self, variations: &V, code_point: char) -> Option<GlyphId> {
        self.family.with_borrowed_face(variations, |opt_face| {
            opt_face.and_then(|face| {
                let id = face.ttf.as_face_ref().glyph_index(code_point);
                if let Some(id) = id {
                    self.glyph_id_to_code_point.borrow_mut().insert(id, code_point);
                }
                id
            })
        })
    }

    /// Get render info for one character, generating one if not found.
    pub fn glyph_info(&self, variations: &V, glyph_id: GlyphId) -> Option<GlyphRenderInfo> {
        let opt_render_info =
            self.cache.borrow().get(variations).and_then(|t| t.glyphs.get(&glyph_id)).copied();
        if opt_render_info.is_some() {
            opt_render_info
        } else {
            self.family.with_borrowed_face(variations, |opt_face| {
                opt_face.map(|face| {
                    // TODO: Switch from chars to GlyphIDs here.
                    let ch = *self.glyph_id_to_code_point.borrow().get(&glyph_id).unwrap();
                    // TODO: Use variations to generate variable-width glyphs.
                    face.msdf.set_variation_axis("wght", 600.0).unwrap();
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
                    render_info
                })
            })
        }
    }

    /// Get kerning between two characters.
    pub fn kerning(&self, variations: &V, left_id: GlyphId, right_id: GlyphId) -> f32 {
        self.family.with_borrowed_face(variations, |opt_face| {
            opt_face
                .map(|face| {
                    if !self.cache.borrow().contains_key(variations) {
                        self.cache.borrow_mut().insert(variations.clone(), default());
                    }
                    *self
                        .cache
                        .borrow_mut()
                        .get_mut(variations)
                        .unwrap()
                        .kerning
                        .entry((left_id, right_id))
                        .or_insert_with(|| {
                            let tables = face.ttf.as_face_ref().tables();
                            let units_per_em = tables.head.units_per_em;
                            let kern_table =
                                tables.kern.and_then(|t| t.subtables.into_iter().next());
                            let kerning =
                                kern_table.and_then(|t| t.glyphs_kerning(left_id, right_id));
                            kerning.unwrap_or_default() as f32 / units_per_em as f32
                        })
                })
                .unwrap_or_default()
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
}
