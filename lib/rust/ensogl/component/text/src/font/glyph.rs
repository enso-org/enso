//! This module defines glyphs and glyphs systems. All glyphs in a glyph system share the same font,
//! but can differ in all other aspects.

use crate::prelude::*;
use ensogl_core::display::world::*;

use crate::font;
use crate::font::VariationAxes;

use ensogl_core::data::color::Rgba;
use ensogl_core::display;
use ensogl_core::display::layout::Alignment;
use ensogl_core::display::scene::Scene;
use ensogl_core::display::symbol::material::Material;
use ensogl_core::display::symbol::shader::builder::CodeTemplate;
use ensogl_core::system::gpu;
use ensogl_core::system::gpu::texture;
use font::Font;
use font::GlyphRenderInfo;
use owned_ttf_parser::GlyphId;



// =============
// === Glyph ===
// =============

/// Glyph texture. Contains all letters encoded in MSDF format.
pub type Texture = gpu::Texture<texture::GpuOnly, texture::Rgb, u8>;

/// A glyph rendered on screen.
///
/// The underlying sprite's size is automatically adjusted depending on char and font size set.
#[derive(Clone, CloneRef, Debug, Deref)]
pub struct Glyph {
    data: Rc<GlyphData>,
}

/// Internal structure of [`Glyph`].
#[allow(missing_docs)]
#[derive(Debug)]
pub struct GlyphData {
    pub glyph_id:    Cell<GlyphId>,
    pub sprite:      Sprite,
    pub context:     Context,
    pub font:        Font,
    pub properties:  Cell<font::family::NonVariableFaceHeader>,
    pub variations:  RefCell<VariationAxes>,
    pub font_size:   Attribute<f32>,
    pub color:       Attribute<Vector4<f32>>,
    pub sdf_bold:    Attribute<f32>,
    pub atlas_index: Attribute<f32>,
    pub atlas:       Uniform<Texture>,
}


// === Properties getters and setters ===

macro_rules! define_prop_setters_and_getters {
    ($prop:ident ($($variant:ident),* $(,)?)) => { paste! {
        #[doc = "Setter of the glyph `"]
        #[doc = stringify!($prop)]
        #[doc = "` property."]
        pub fn [<set_ $prop:snake:lower>](&self, value: font::$prop) {
            self.properties.modify(|p| p.[<$prop:snake:lower>] = value);
            self.variations.borrow_mut().[<set_ $prop:snake:lower>](value);
            self.refresh();
        }

        $(
            #[doc = "Set the `"]
            #[doc = stringify!($prop)]
            #[doc = "` property to `"]
            #[doc = stringify!($variant)]
            #[doc = "`."]
            pub fn [<set_ $prop:snake:lower _ $variant:snake:lower>](&self) {
                self.[<set_ $prop:snake:lower>](font::$prop::$variant)
            }

            #[doc = "Checks whether the `"]
            #[doc = stringify!($prop)]
            #[doc = "` property is set to `"]
            #[doc = stringify!($variant)]
            #[doc = "`."]
            pub fn [<is_ $prop:snake:lower _ $variant:snake:lower>](&self) -> bool {
                self.properties.get().[<$prop:snake:lower>] == font::$prop::$variant
            }
        )*
    }};
}

impl Glyph {
    define_prop_setters_and_getters![Weight(
        Thin, ExtraLight, Light, Normal, Medium, SemiBold, Bold, ExtraBold, Black
    )];

    define_prop_setters_and_getters![Width(
        UltraCondensed,
        ExtraCondensed,
        Condensed,
        SemiCondensed,
        Normal,
        SemiExpanded,
        Expanded,
        ExtraExpanded,
        UltraExpanded
    )];

    define_prop_setters_and_getters![Style(Normal, Italic, Oblique)];

    /// Color getter.
    pub fn color(&self) -> Rgba {
        self.color.get().into()
    }

    /// Color setter.
    pub fn set_color(&self, color: impl Into<Rgba>) {
        self.color.set(color.into().into())
    }

    /// SDF-based glyph thickness adjustment. Values greater than 0 make the glyph thicker, while
    /// values lower than 0 makes it thinner.
    pub fn sdf_bold(&self) -> f32 {
        self.sdf_bold.get()
    }

    /// SDF-based glyph thickness getter.
    pub fn set_sdf_bold(&self, value: f32) {
        self.sdf_bold.set(value);
    }

    /// Size getter.
    pub fn font_size(&self) -> f32 {
        self.font_size.get()
    }

    /// Size setter.
    pub fn set_font_size(&self, size: f32) {
        self.font_size.set(size);
        let opt_glyph_info = self.font.glyph_info(
            self.properties.get(),
            &self.variations.borrow(),
            self.glyph_id.get(),
        );
        if let Some(glyph_info) = opt_glyph_info {
            self.sprite.size.set(glyph_info.scale.scale(size))
        }
    }

    /// Change the displayed character.
    pub fn set_char(&self, ch: char) {
        let opt_glyph_id =
            self.font.glyph_id_of_code_point(self.properties.get(), &self.variations.borrow(), ch);
        if let Some(glyph_id) = opt_glyph_id {
            self.set_glyph_id(glyph_id)
        }
        // FIXME[WD]: display not found char. https://www.pivotaltracker.com/story/show/182746060
    }

    /// Change the displayed character.
    pub fn set_glyph_id(&self, glyph_id: GlyphId) {
        self.glyph_id.set(glyph_id);
        let opt_glyph_info =
            self.font.glyph_info(self.properties.get(), &self.variations.borrow(), glyph_id);
        if let Some(glyph_info) = opt_glyph_info {
            self.atlas_index.set(glyph_info.msdf_texture_glyph_id as f32);
            self.update_atlas();
            let font_size = self.font_size();
            self.sprite.size.set(glyph_info.scale.scale(font_size));
        } else {
            // FIXME[WD]: This should display a bad character. https://www.pivotaltracker.com/story/show/182746060
            panic!()
        }
    }

    /// Check whether the CPU-bound texture changed and if so, upload it to GPU.
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

    /// Check whether a new glyph should be baked to the atlas and reload the texture if needed.
    /// This is useful for example after changing the width of the glyph.
    fn refresh(&self) {
        self.set_glyph_id(self.glyph_id.get());
    }
}

impl display::Object for Glyph {
    fn display_object(&self) -> &display::object::Instance {
        self.sprite.display_object()
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
    font_size:     Buffer<f32>,
    color:         Buffer<Vector4<f32>>,
    sdf_bold:      Buffer<f32>,
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
        // FIXME: The following line is unsafe. It can fail if the context was lost before calling
        //        this function. Also, the texture will not be restored after context restoration.
        let context = scene.context.borrow().as_ref().unwrap().clone_ref();
        let sprite_system = SpriteSystem::new(scene);
        let symbol = sprite_system.symbol();
        let texture = Texture::new(&context, (0, 0));
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
            font_size: mesh.instance_scope().add_buffer("font_size"),
            color: mesh.instance_scope().add_buffer("color"),
            sdf_bold: mesh.instance_scope().add_buffer("sdf_bold"),
            atlas_index: mesh.instance_scope().add_buffer("atlas_index"),
        }
    }

    /// Create new glyph. In the returned glyph the further parameters (position,size,character)
    /// may be set.
    pub fn new_glyph(&self) -> Glyph {
        let context = self.context.clone();
        let sprite = self.sprite_system.new_instance();
        let instance_id = sprite.instance_id;
        let font_size = self.font_size.at(instance_id);
        let color = self.color.at(instance_id);
        let sdf_bold = self.sdf_bold.at(instance_id);
        let atlas_index = self.atlas_index.at(instance_id);
        let font = self.font.clone_ref();
        let atlas = self.atlas.clone();
        let glyph_id = default();
        let properties = default();
        let variations = default();
        color.set(Vector4::new(0.0, 0.0, 0.0, 0.0));
        atlas_index.set(0.0);
        Glyph {
            data: Rc::new(GlyphData {
                sprite,
                context,
                font,
                font_size,
                color,
                sdf_bold,
                atlas_index,
                atlas,
                glyph_id,
                properties,
                variations,
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
        material.add_input("sdf_bold", 0.0);
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
