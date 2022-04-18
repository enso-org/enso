//! This module defines glyphs and glyphs systems. All glyphs in a glyph system share the same font,
//! but can differ in all other aspects.

use crate::prelude::*;
use ensogl_core::display::world::*;

use crate::typeface::font;

use const_format::concatcp;
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



// =================
// === Constants ===
// =================

mod style_flag {
    use const_format::concatcp;

    pub const BOLD: i32 = 1 << 0;

    pub const GLSL_DEFINITIONS: &str = concatcp!("const int STYLE_BOLD_FLAG = ", BOLD, ";\n");
}



// =============
// === Glyph ===
// =============

/// Glyph texture. Contains all letters encoded in MSDF format.
pub type Texture = gpu::Texture<texture::GpuOnly, texture::Rgb, u8>;

/// A glyph rendered on screen.
///
/// The underlying sprite's size is automatically adjusted depending on char and font size set.
#[derive(Clone, CloneRef, Debug)]
pub struct Glyph {
    sprite:      Sprite,
    context:     Context,
    font:        Font,
    font_size:   Attribute<f32>,
    color:       Attribute<Vector4<f32>>,
    style:       Attribute<i32>,
    sdf_bold:    Attribute<f32>,
    atlas_index: Attribute<f32>,
    atlas:       Uniform<Texture>,
    char:        Rc<Cell<char>>,
}

impl Glyph {
    /// Glyph color attribute accessor.
    pub fn color(&self) -> Rgba {
        self.color.get().into()
    }

    pub fn set_color(&self, color: impl Into<Rgba>) {
        self.color.set(color.into().into())
    }

    pub fn is_bold(&self) -> bool {
        self.style.get() & style_flag::BOLD != 0
    }

    pub fn set_bold(&self, value: bool) {
        self.style.modify(|v| if value { *v |= style_flag::BOLD } else { *v &= !style_flag::BOLD });
    }

    pub fn sdf_bold(&self) -> f32 {
        self.sdf_bold.get()
    }

    pub fn set_sdf_bold(&self, value: f32) {
        self.sdf_bold.set(value);
    }

    pub fn font_size(&self) -> f32 {
        self.font_size.get()
    }

    pub fn set_font_size(&self, size: f32) {
        self.font_size.set(size);
        let glyph_info = self.font.glyph_info(self.char.get());
        self.sprite.size.set(glyph_info.scale.scale(size));
    }

    /// Change the displayed character.
    pub fn set_char(&self, ch: char) {
        self.char.set(ch);
        let glyph_info = self.font.glyph_info(ch);
        self.atlas_index.set(glyph_info.msdf_texture_glyph_id as f32);
        self.update_msdf_texture();
        let font_size = self.font_size();
        self.sprite.size.set(glyph_info.scale.scale(font_size));
    }

    // FIXME: How does it work? Replace with better checking.
    fn update_msdf_texture(&self) {
        let texture_changed = self.atlas.with_content(|texture| {
            texture.storage().height != self.font.msdf_texture_rows() as i32
        });
        if texture_changed {
            let width = font::msdf::Texture::WIDTH as i32;
            let height = self.font.msdf_texture_rows() as i32;
            let texture = Texture::new(&self.context, (width, height));
            self.font.with_borrowed_msdf_texture_data(|data| texture.reload_with_content(data));
            self.atlas.set(texture);
        }
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
    style:         Buffer<i32>,
    sdf_bold:      Buffer<f32>,
    atlas_index:   Buffer<f32>,
    atlas:         Uniform<Texture>,
}

impl System {
    /// Constructor.
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
            style: mesh.instance_scope().add_buffer("style"),
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
        let style = self.style.at(instance_id);
        let sdf_bold = self.sdf_bold.at(instance_id);
        let atlas_index = self.atlas_index.at(instance_id);
        let font = self.font.clone_ref();
        let atlas = self.atlas.clone();
        let char = default();
        color.set(Vector4::new(0.0, 0.0, 0.0, 0.0));
        atlas_index.set(0.0);
        Glyph { sprite, context, font, font_size, color, style, sdf_bold, atlas_index, atlas, char }
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
const FUNCTIONS: &str =
    concatcp!(style_flag::GLSL_DEFINITIONS, include_str!("glsl/glyph_mac.glsl"));
#[cfg(not(target_os = "macos"))]
const FUNCTIONS: &str = concatcp!(style_flag::GLSL_DEFINITIONS, include_str!("glsl/glyph.glsl"));

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
        material.add_input("style", 0);
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
