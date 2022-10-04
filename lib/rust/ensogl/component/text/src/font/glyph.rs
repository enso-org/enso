//! This module defines glyphs and glyphs systems. All glyphs in a glyph system share the same font,
//! but can differ in all other aspects.

use crate::prelude::*;
use ensogl_core::display::world::*;

use crate::buffer::formatting::PropertyDiffApply;
use crate::font;
use crate::font::VariationAxes;
use crate::PropertyDiff;
use crate::ResolvedProperty;
use crate::SdfWeight;
use crate::Size;

use enso_frp::stream::ValueProvider;
use enso_text::Byte;
use ensogl_core::application::command::FrpNetworkProvider;
use ensogl_core::data::color;
use ensogl_core::data::color::Rgba;
use ensogl_core::display;
use ensogl_core::display::layout::Alignment;
use ensogl_core::display::scene::Scene;
use ensogl_core::display::symbol::material::Material;
use ensogl_core::display::symbol::shader::builder::CodeTemplate;
#[cfg(target_arch = "wasm32")]
use ensogl_core::display::world;
use ensogl_core::frp;
#[cfg(target_arch = "wasm32")]
use ensogl_core::system::gpu;
use ensogl_core::system::gpu::texture;
use font::Font;
use font::GlyphRenderInfo;
use font::Style;
use font::Weight;
use font::Width;
use owned_ttf_parser::GlyphId;



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! {
    Input {
        set_color(color::Lcha),
        skip_color_animation(),
    }
}



// =============
// === Glyph ===
// =============

/// Glyph texture. Contains all letters encoded in MSDF format.
#[cfg(target_arch = "wasm32")]
type Texture = gpu::Texture<texture::GpuOnly, texture::Rgb, u8>;

#[cfg(target_arch = "wasm32")]
fn new_texture(context: &Context, param: (i32, i32)) -> Texture {
    Texture::new(context, param)
}

#[cfg(not(target_arch = "wasm32"))]
type Texture = u32;

#[cfg(not(target_arch = "wasm32"))]
fn new_texture(_context: &Context, _param: (i32, i32)) -> Texture {
    0
}


#[cfg(target_arch = "wasm32")]
type Context = world::Context;
#[cfg(not(target_arch = "wasm32"))]
type Context = ();

#[cfg(target_arch = "wasm32")]
fn get_context(scene: &Scene) -> Context {
    // FIXME: The following line is unsafe. It can fail if the context was lost before
    // calling this function. Also, the texture will not be restored
    // after context restoration.
    scene.context.borrow().as_ref().unwrap().clone_ref()
}

#[cfg(not(target_arch = "wasm32"))]
fn get_context(_scene: &Scene) -> Context {}

/// A glyph rendered on screen.
#[derive(Clone, CloneRef, Debug, Deref)]
pub struct Glyph {
    data: Rc<GlyphData>,
}

/// Internal structure of [`Glyph`].
#[allow(missing_docs)]
#[derive(Debug, Deref)]
pub struct GlyphData {
    // Please note that [`GlyphData`] does not implement [`Clone`]. This FRP network will not be
    // cloned in the FRP definition and thus will not cause any mem leak.
    #[deref]
    pub frp:                Frp,
    pub glyph_id:           Cell<GlyphId>,
    pub line_byte_offset:   Cell<Byte>,
    pub display_object:     display::object::Instance,
    pub sprite:             Sprite,
    pub context:            Context,
    pub font:               Font,
    pub properties:         Cell<font::family::NonVariableFaceHeader>,
    pub variations:         RefCell<VariationAxes>,
    pub size:               Attribute<f32>,
    pub color:              Attribute<Vector4<f32>>,
    pub sdf_weight:         Attribute<f32>,
    pub atlas_index:        Attribute<f32>,
    pub atlas:              Uniform<Texture>,
    pub color_animation:    color::Animation,
    pub x_advance:          Rc<Cell<f32>>,
    /// Indicates whether this glyph is attached to cursor. Needed for text width computation.
    /// Attached glyphs should not be considered part of the line during animation because they
    /// will be moved around, so they need to be ignored when computing the line width.
    pub attached_to_cursor: Rc<Cell<bool>>,
}


// === Face Header Properties Getters and Setters ===

/// For each property, such as `Weight(Thin, ExtraLight, ...)` defines:
/// ```text
/// pub fn weight(&self) -> Weight { ... }
/// pub fn set_weight(&self, weight: Weight) { ... }
/// pub fn set_weight_thin(&self) { ... }
/// pub fn set_weight_extra_light(&self) { ... }
/// ...
/// pub fn is_weight_thin(&self) { ... }
/// pub fn is_weight_extra_light(&self) { ... }
/// ...
/// ```
///
/// It also defines:
///
/// ``text
/// pub fn set_properties(&self, props: font::family::NonVariableFaceHeader) { ... }
/// ```
macro_rules! define_prop_setters_and_getters {
    ($($prop:ident ($($variant:ident),* $(,)?)),*$(,)?) => { paste! {

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

            $(
                #[doc = "Set the `"]
                #[doc = stringify!($prop)]
                #[doc = "` property to `"]
                #[doc = stringify!($variant)]
                #[doc = "`."]
                pub fn [<set_ $prop:snake:lower _ $variant:snake:lower>](&self) {
                    self.[<set_ $prop:snake:lower>]($prop::$variant)
                }

                #[doc = "Checks whether the `"]
                #[doc = stringify!($prop)]
                #[doc = "` property is set to `"]
                #[doc = stringify!($variant)]
                #[doc = "`."]
                pub fn [<is_ $prop:snake:lower _ $variant:snake:lower>](&self) -> bool {
                    self.properties.get().[<$prop:snake:lower>] == $prop::$variant
                }
            )*
        )*
    }};
}

impl Glyph {
    define_prop_setters_and_getters![
        Weight(Thin, ExtraLight, Light, Normal, Medium, SemiBold, Bold, ExtraBold, Black),
        Style(Normal, Italic, Oblique),
        Width(
            UltraCondensed,
            ExtraCondensed,
            Condensed,
            SemiCondensed,
            Normal,
            SemiExpanded,
            Expanded,
            ExtraExpanded,
            UltraExpanded
        )
    ];
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
        self.set_color.value()
    }

    /// SDF-based glyph thickness getter.
    pub fn sdf_weight(&self) -> SdfWeight {
        SdfWeight(self.sdf_weight.get())
    }

    /// SDF-based glyph thickness adjustment. Values greater than 0 make the glyph thicker, while
    /// values lower than 0 makes it thinner.
    pub fn set_sdf_weight(&self, value: impl Into<SdfWeight>) {
        self.sdf_weight.set(value.into().value);
    }

    /// Size getter.
    pub fn size(&self) -> Size {
        Size(self.size.get())
    }

    /// Size setter.
    pub fn set_size(&self, size: Size) {
        let size = size.value;
        self.size.set(size);
        let opt_glyph_info = self.font.glyph_info(
            self.properties.get(),
            &self.variations.borrow(),
            self.glyph_id.get(),
        );
        if let Some(glyph_info) = opt_glyph_info {
            self.sprite.size.set(glyph_info.scale.scale(size))
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
        let opt_glyph_info = self.font.glyph_info(self.properties.get(), &variations, glyph_id);
        if let Some(glyph_info) = opt_glyph_info {
            self.atlas_index.set(glyph_info.msdf_texture_glyph_id as f32);
            self.update_atlas();
            self.sprite.size.set(glyph_info.scale.scale(self.size().value));
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

    /// Check whether the CPU-bound texture changed and if so, upload it to GPU.
    #[cfg(not(target_arch = "wasm32"))]
    fn update_atlas(&self) {}
    #[cfg(target_arch = "wasm32")]
    fn update_atlas(&self) {
        let cpu_tex_height = self.font.msdf_texture_rows() as i32;
        let gpu_tex_height = self.atlas.with_content(|texture| texture.storage().height);
        let texture_changed = cpu_tex_height != gpu_tex_height;
        if texture_changed {
            let cpu_tex_width = font::msdf::Texture::WIDTH as i32;
            let texture = Texture::new(&self.context, (cpu_tex_width, cpu_tex_height));
            self.font.with_borrowed_msdf_texture_data(|data| texture.reload_with_content(data));
            self.atlas.set(texture);
        }
    }
}

impl display::Object for Glyph {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
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
    logger:        Logger,
    context:       Context,
    sprite_system: SpriteSystem,
    pub font:      Font,
    size:          Buffer<f32>,
    color:         Buffer<Vector4<f32>>,
    sdf_weight:    Buffer<f32>,
    atlas_index:   Buffer<f32>,
    atlas:         Uniform<Texture>,
}

impl System {
    /// Constructor.
    #[profile(Detail)]
    pub fn new(scene: impl AsRef<Scene>, font: Font) -> Self {
        let logger = Logger::new("glyph_system");
        let size = font::msdf::Texture::size();
        let scene = scene.as_ref();
        let sprite_system = SpriteSystem::new(scene);
        let symbol = sprite_system.symbol();
        let context = get_context(scene);
        let texture = new_texture(&context, (0, 0));
        let mesh = symbol.surface();

        sprite_system.set_material(Self::material());
        sprite_system.set_alignment(Alignment::bottom_left());
        scene.variables.add("msdf_range", GlyphRenderInfo::MSDF_PARAMS.range as f32);
        scene.variables.add("msdf_size", size);
        Self {
            logger,
            context,
            sprite_system,
            font,
            atlas: symbol.variables().add_or_panic("atlas", texture),
            size: mesh.instance_scope().add_buffer("font_size"),
            color: mesh.instance_scope().add_buffer("color"),
            sdf_weight: mesh.instance_scope().add_buffer("sdf_weight"),
            atlas_index: mesh.instance_scope().add_buffer("atlas_index"),
        }
    }

    /// Create new glyph. In the returned glyph the further parameters (position,size,character)
    /// may be set.
    pub fn new_glyph(&self) -> Glyph {
        let frp = Frp::new();
        #[allow(clippy::clone_on_copy)]
        #[allow(clippy::unit_arg)]
        let context = self.context.clone();
        let display_object = display::object::Instance::new();
        let sprite = self.sprite_system.new_instance();
        let instance_id = sprite.instance_id;
        let size = self.size.at(instance_id);
        let color = self.color.at(instance_id);
        let sdf_weight = self.sdf_weight.at(instance_id);
        let atlas_index = self.atlas_index.at(instance_id);
        let font = self.font.clone_ref();
        let atlas = self.atlas.clone();
        let glyph_id = default();
        let line_byte_offset = default();
        let properties = default();
        let variations = default();
        let color_animation = color::Animation::new(frp.network());
        let x_advance = default();
        let attached_to_cursor = default();
        display_object.add_child(&sprite);
        color.set(Vector4::new(0.0, 0.0, 0.0, 0.0));
        atlas_index.set(0.0);

        let network = frp.network();
        frp::extend! {network
            color_animation.target <+ frp.set_color;
            color_animation.skip <+ frp.skip_color_animation;
            eval color_animation.value ((c) color.set(Rgba::from(c).into()));
        }

        Glyph {
            data: Rc::new(GlyphData {
                frp,
                display_object,
                sprite,
                context,
                font,
                size,
                color,
                sdf_weight,
                atlas_index,
                atlas,
                glyph_id,
                line_byte_offset,
                properties,
                variations,
                color_animation,
                x_advance,
                attached_to_cursor,
            }),
        }
    }

    /// Get underlying sprite system.
    pub fn sprite_system(&self) -> &SpriteSystem {
        &self.sprite_system
    }
}

impl display::Object for System {
    fn display_object(&self) -> &display::object::Instance {
        self.sprite_system.display_object()
    }
}


// === Material ===

#[cfg(target_os = "macos")]
const FUNCTIONS: &str = include_str!("glsl/glyph_mac.glsl");
#[cfg(not(target_os = "macos"))]
const FUNCTIONS: &str = include_str!("glsl/glyph.glsl");

const MAIN: &str = "output_color = color_from_msdf(); output_id=vec4(0.0,0.0,0.0,0.0);";

impl System {
    /// Defines a default material of this system.
    fn material() -> Material {
        let mut material = Material::new();
        material.add_input_def::<texture::FloatSampler>("atlas");
        material.add_input_def::<Vector2<f32>>("msdf_size");
        material.add_input_def::<f32>("atlas_index");
        material.add_input("pixel_ratio", 1.0);
        material.add_input("z_zoom_1", 1.0);
        material.add_input("msdf_range", GlyphRenderInfo::MSDF_PARAMS.range as f32);
        material.add_input("font_size", 10.0);
        material.add_input("color", Vector4::new(0.0, 0.0, 0.0, 1.0));
        material.add_input("sdf_weight", 0.0);
        // FIXME We need to use this output, as we need to declare the same amount of shader
        // FIXME outputs as the number of attachments to framebuffer. We should manage this more
        // FIXME intelligent. For example, we could allow defining output shader fragments,
        // FIXME which will be enabled only if pass of given attachment type was enabled.
        material.add_output("id", Vector4::<f32>::new(0.0, 0.0, 0.0, 0.0));

        let code = CodeTemplate::new(FUNCTIONS, MAIN, "");
        material.set_code(code);
        material
    }
}
