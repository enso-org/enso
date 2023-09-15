//! This module defines glyphs and glyphs systems. All glyphs in a glyph system share the same font,
//! but can differ in all other aspects.

use crate::prelude::*;

use crate::buffer::formatting::PropertyDiffApply;
use crate::font;
use crate::font::VariationAxes;
use crate::PropertyDiff;
use crate::ResolvedProperty;
use crate::SdfWeight;
use crate::Size;

use enso_text::Byte;
use ensogl_core::data::color;
use ensogl_core::data::color::Rgba;
use ensogl_core::display;
use ensogl_core::display::layout::alignment;
use ensogl_core::display::scene::Scene;
use ensogl_core::display::symbol::geometry::SpriteSystem;
use ensogl_core::display::symbol::material::Material;
use ensogl_core::display::symbol::shader::builder::CodeTemplate;
use ensogl_core::system::gpu::texture;
use font::FontWithGpuData;
use font::GlyphRenderInfo;
use font::Style;
use font::Weight;
use font::Width;
use owned_ttf_parser::GlyphId;



// ==================
// === SystemData ===
// ==================

/// Shape system data. Manages custom GLSL shader code for the glyph shape system.
#[derive(Debug, Clone, Copy)]
#[allow(missing_docs)]
pub struct SystemData {}

const FUNCTIONS: &str = include_str!("glsl/glyph.glsl");
const MAIN: &str = "output_color = color_from_msdf(); output_id=vec4(0.0,0.0,0.0,0.0);";

impl SystemData {
    /// Defines a default material of this system.
    fn material() -> Material {
        let mut material = Material::new();
        material.add_input_def::<texture::FloatSamplerArray>("atlas");
        material.add_input_def::<Vector2<u32>>("msdf_size");
        material.add_input_def::<u32>("atlas_index");
        material.add_input("pixel_ratio", 1.0);
        material.add_input("z_zoom_1", 1.0);
        material.add_input("msdf_range", GlyphRenderInfo::MSDF_PARAMS.range as f32);
        material.add_input("font_size", 10.0);
        material.add_input("color", Vector4::new(0.0, 0.0, 0.0, 1.0));
        material.add_input("sdf_weight", 0.0);
        // === Adjusting look and feel of different fonts on different operating systems ===
        material.add_input("opacity_increase", 0.0);
        material.add_input("opacity_exponent", 1.0);
        // TODO[WD]: We need to use this output, as we need to declare the same amount of shader
        //     outputs as the number of attachments to framebuffer. We should manage this more
        //     intelligent. For example, we could allow defining output shader fragments,
        //     which will be enabled only if pass of given attachment type was enabled.
        material.add_output("id", Vector4::<f32>::new(0.0, 0.0, 0.0, 0.0));

        let code = CodeTemplate::new(FUNCTIONS, MAIN, "");
        material.set_code(code);
        material
    }
}



// =================
// === ShapeData ===
// =================

/// Shape data. Allows passing font information to the [`SystemData`].
#[allow(missing_docs)]
#[derive(Debug)]
pub struct ShapeData {
    pub font: FontWithGpuData,
}

impl ShapeData {
    fn flavor(&self) -> display::shape::system::ShapeSystemFlavor {
        let mut hasher = DefaultHasher::new();
        std::hash::Hash::hash(&self.font.name(), &mut hasher);
        display::shape::system::ShapeSystemFlavor { flavor: hasher.finish() }
    }
}

/// The glyph shape used for rendering all text elements.
pub mod glyph_shape {
    use super::*;
    // FIXME[WD]: We are using old shape generator here which does not use shader precompilation.
    //   To be fixed in the next PR: https://www.pivotaltracker.com/story/show/184304289
    ensogl_core::shape_old! {
        type SystemData = SystemData;
        type ShapeData = ShapeData;
        flavor = ShapeData::flavor;
        above = [ensogl_core::display::shape::compound::rectangle, ensogl_core::gui::cursor::shape];
        (
            style: Style,
            font_size: f32,
            color: Vector4<f32>,
            sdf_weight: f32,
            atlas_index: u32
        ) {
            // The shape does not matter. The [`SystemData`] defines custom GLSL code.
            Plane().into()
        }
    }
}

impl display::shape::CustomSystemData<glyph_shape::Shape> for SystemData {
    fn new(
        data: &display::shape::ShapeSystemStandardData<glyph_shape::Shape>,
        shape_data: &ShapeData,
    ) -> Self {
        let font = &shape_data.font;
        let size = font.msdf_texture().size();
        let sprite_system = &data.model.sprite_system;
        let symbol = sprite_system.symbol();

        *data.model.material.borrow_mut() = Self::material();
        *data.model.geometry_material.borrow_mut() = SpriteSystem::default_geometry_material();
        data.model.do_not_use_shape_definition.set(true);

        sprite_system.unsafe_set_alignment(alignment::Dim2::left_bottom());
        display::world::with_context(|t| {
            let mut variables = t.variables.borrow_mut();
            variables.add("msdf_range", GlyphRenderInfo::MSDF_PARAMS.range as f32);
            variables.add("msdf_size", size);
        });

        let mut variables = symbol.variables.borrow_mut();
        variables.add_uniform_or_panic("atlas", &font.atlas);
        variables.add_uniform_or_panic("opacity_increase", &font.opacity_increase);
        variables.add_uniform_or_panic("opacity_exponent", &font.opacity_exponent);

        SystemData {}
    }
}



// =============
// === Glyph ===
// =============

/// A glyph rendered on screen.
#[derive(Clone, CloneRef, Debug, Deref, display::Object)]
pub struct Glyph {
    data: Rc<GlyphData>,
}

/// Internal structure of [`Glyph`].
#[allow(missing_docs)]
#[derive(Debug, display::Object)]
pub struct GlyphData {
    pub view:               glyph_shape::View,
    pub line_byte_offset:   Cell<Byte>,
    pub x_advance:          Cell<f32>,
    /// Indicates whether this glyph is attached to cursor. Needed for text width computation.
    /// Attached glyphs should not be considered part of the line during animation because they
    /// will be moved around, so they need to be ignored when computing the line width.
    pub attached_to_cursor: Cell<bool>,
    glyph_id:               Cell<GlyphId>,
    display_object:         display::object::Instance,
    properties:             Cell<font::family::NonVariableFaceHeader>,
    variations:             RefCell<VariationAxes>,
}


// === Face Header Properties Getters and Setters ===

/// For each property, such as `Weight` defines:
/// ```text
/// pub fn weight(&self) -> Weight { ... }
/// pub fn set_weight(&self, weight: Weight) { ... }
/// ...
/// ```
///
/// It also defines:
///
/// ``text
/// pub fn set_properties(&self, props: font::family::NonVariableFaceHeader) { ... }
/// ```
macro_rules! define_prop_setters_and_getters {
    ($($prop:ident),*$(,)?) => { paste! {

        /// Set `NonVariableFaceHeader` of the glyph.
        pub fn set_properties(&self, props: font::family::NonVariableFaceHeader) {
            self.properties.set(props.clone());
            $(
                self.variations.borrow_mut().[<set_ $prop:snake:lower>](props.[<$prop:snake:lower>]);
            )*
            self.refresh();
        }

        $(
            #[doc = "Setter of the glyph `"]
            #[doc = stringify!($prop)]
            #[doc = "` property."]
            pub fn [<set_ $prop:snake:lower>](&self, value: $prop) {
                self.properties.modify(|p| p.[<$prop:snake:lower>] = value);
                self.variations.borrow_mut().[<set_ $prop:snake:lower>](value);
                self.refresh();
            }

            #[doc = "Gets the current `"]
            #[doc = stringify!($prop)]
            #[doc = "` property value.`"]
            pub fn [<$prop:snake:lower>](&self) -> $prop {
                self.properties.get().[<$prop:snake:lower>]
            }
        )*
    }};
}

impl Glyph {
    define_prop_setters_and_getters![Weight, Style, Width];
}


// === Formatting properties ===

/// For each formatting property defines:
/// ```text
/// pub fn mod_size(&self, f: impl FnOnce(Size) -> Size) { ... }
/// ...
/// ```
///
/// It also defines:
/// ```text
/// pub fn set_property(&self, property: ResolvedProperty) { ... }
/// ```
macro_rules! define_formatting_setters_and_mods {
    ($($name:ident : $tp:ty),* $(,)?) => {
        paste! {
            /// Property value setter.
            pub fn set_property(&self, property: ResolvedProperty) {
                match property {
                    $(ResolvedProperty::[<$name:camel>](t) => self.[<set_ $name:snake:lower>](t)),*
                }
            }

            $(
                /// Property value modifier.
                pub fn [<mod_ $name:snake:lower>](&self, f:impl FnOnce($tp) -> $tp) {
                    self.[<set_ $name:snake:lower>](f(self.[<$name:snake:lower>]()))
                }
            )*
        }
    };
}


impl Glyph {
    crate::with_formatting_properties! {define_formatting_setters_and_mods}
}


// === Formatting Property Diffs ===

macro_rules! define_formatting_property_diffs {
    ($($field:ident : $tp:ty = $def:expr),* $(,)?) => {paste! {
        impl Glyph {
            /// Apply the property diff.
            pub fn mod_property(&self, property: PropertyDiff) {
                match property {
                    $(PropertyDiff::$field(diff) => {
                        self.[<mod_ $field:snake:lower>](|t| t.apply_diff(diff))
                    })*
                }
            }
        }
    }}
}

crate::with_formatting_property_diffs!(define_formatting_property_diffs);



// === Properties Getters and Setters ===

impl Glyph {
    /// Color getter.
    pub fn color(&self) -> color::Lcha {
        Rgba::from(self.view.color.get()).into()
    }

    /// Color setter.
    pub fn set_color(&self, color: color::Lcha) {
        self.view.color.set(Rgba::from(color).into());
    }

    /// SDF-based glyph thickness getter.
    pub fn sdf_weight(&self) -> SdfWeight {
        SdfWeight(self.view.sdf_weight.get())
    }

    /// SDF-based glyph thickness adjustment. Values greater than 0 make the glyph thicker, while
    /// values lower than 0 makes it thinner.
    pub fn set_sdf_weight(&self, value: impl Into<SdfWeight>) {
        self.view.sdf_weight.set(value.into().value);
    }

    /// Size getter.
    pub fn font_size(&self) -> Size {
        Size(self.view.font_size.get())
    }

    /// Size setter.
    pub fn set_font_size(&self, size: Size) {
        let size = size.value;
        self.view.font_size.set(size);
        let opt_glyph_info = self.view.data.borrow().font.glyph_info(
            self.properties.get(),
            &self.variations.borrow(),
            self.glyph_id.get(),
        );
        if let Some(glyph_info) = opt_glyph_info {
            self.view.set_size(glyph_info.scale.scale(size));
        } else {
            error!("Cannot find glyph render info for glyph id: {:?}.", self.glyph_id.get());
        }
    }
}


// === Glyph Modification ===

impl Glyph {
    /// Change the displayed character.
    pub fn set_glyph_id(&self, glyph_id: GlyphId) {
        self.glyph_id.set(glyph_id);
        let variations = self.variations.borrow();
        let opt_glyph_info =
            self.view.data.borrow().font.glyph_info(self.properties.get(), &variations, glyph_id);
        if let Some(glyph_info) = opt_glyph_info {
            self.view.atlas_index.set(glyph_info.msdf_texture_glyph_id);
            self.view.set_size(glyph_info.scale.scale(self.font_size().value));
        } else {
            // This should not happen. Fonts contain special glyph for missing characters.
            warn!("Cannot find glyph render info for glyph id: {:?}.", glyph_id);
        }
    }

    /// Refresh the glyph.
    ///
    /// # Performance
    /// It might be possible to get better performance by using dirty flags. Right now, after
    /// modification of each glyph property which causes the glyph to be re-rendered, such as
    /// weight or style, we call this method. This is not optimal when multiple properties are
    /// changed at once. However, such a situation is not common as many property modifiers, such as
    /// color do not call this method.
    fn refresh(&self) {
        self.set_glyph_id(self.glyph_id.get());
    }
}



// =================
// === WeakGlyph ===
// =================

/// Weak version of [`Glyph`].
#[derive(Clone, CloneRef, Debug)]
pub struct WeakGlyph {
    data: Weak<GlyphData>,
}

impl Glyph {
    /// Create a weak version of this glyph.
    pub fn downgrade(&self) -> WeakGlyph {
        WeakGlyph { data: Rc::downgrade(&self.data) }
    }
}

impl WeakGlyph {
    /// Upgrade the weak glyph to a strong one.
    pub fn upgrade(&self) -> Option<Glyph> {
        self.data.upgrade().map(|data| Glyph { data })
    }
}



// ==============
// === System ===
// ==============

/// A system for displaying glyphs.
#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct System {
    pub font: FontWithGpuData,
}

impl System {
    /// Constructor.
    #[profile(Detail)]
    pub fn new(scene: impl AsRef<Scene>, font_name: impl Into<font::Name>) -> Self {
        let scene = scene.as_ref();
        let fonts = scene.extension::<font::Registry>();
        let font = fonts.load(font_name);
        Self { font }
    }

    /// Create new glyph. In the returned glyph the further parameters (position,size,character)
    /// may be set.
    #[profile(Debug)]
    pub fn new_glyph(&self) -> Glyph {
        let display_object = display::object::Instance::new_no_debug();
        let font = self.font.clone_ref();
        let glyph_id = default();
        let line_byte_offset = default();
        let properties = default();
        let variations = default();
        let x_advance = default();
        let attached_to_cursor = default();
        let view = glyph_shape::View::new_with_data(ShapeData { font });
        view.color.set(Vector4::new(0.0, 0.0, 0.0, 0.0));
        view.atlas_index.set(0);
        display_object.add_child(&view);
        Glyph {
            data: Rc::new(GlyphData {
                view,
                display_object,
                glyph_id,
                line_byte_offset,
                properties,
                variations,
                x_advance,
                attached_to_cursor,
            }),
        }
    }
}
