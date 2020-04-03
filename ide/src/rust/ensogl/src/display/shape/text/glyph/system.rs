//! A main glyph system implementation.

use crate::prelude::*;

use crate::display::layout::types::*;
use crate::display::shape::text::glyph::font::FontHandle;
use crate::display::shape::text::glyph::font::GlyphRenderInfo;
use crate::display::shape::text::glyph::pen::PenIterator;
use crate::display::shape::text::glyph::msdf::MsdfTexture;
use crate::display::symbol::material::Material;
use crate::display::symbol::shader::builder::CodeTemplate;
use crate::display::world::*;
use crate::system::gpu::texture::*;
use crate::system::gpu::types::*;
use crate::display::object::traits::*;

use nalgebra::Vector2;
use nalgebra::Vector4;
use crate::display;



// =============
// === Glyph ===
// =============

/// A glyph rendered on screen. The displayed character will be stretched to fit the entire bbox of
/// underlying sprite.
#[derive(Debug,Shrinkwrap)]
pub struct Glyph {
    #[shrinkwrap(main_field)]
    sprite          : Sprite,
    context         : Context,
    msdf_index_attr : Attribute<f32>,
    color_attr      : Attribute<Vector4<f32>>,
    font            : FontHandle,
    msdf_uniform    : Uniform<Texture<GpuOnly,Rgb,u8>>,
}

impl Glyph {
    /// Glyph color attribute accessor.
    pub fn color(&self) -> Attribute<Vector4<f32>> {
        self.color_attr.clone_ref()
    }

    /// Change the displayed character.
    pub fn set_glyph(&mut self, ch:char) {
        let glyph_info = self.font.get_glyph_info(ch);
        self.msdf_index_attr.set(glyph_info.msdf_texture_glyph_id as f32);
        self.update_msdf_texture();
    }

    fn update_msdf_texture(&mut self) {
        let texture_changed = self.msdf_uniform.with_content(|texture| {
            texture.storage().height != self.font.msdf_texture_rows() as i32
        });
        if texture_changed {
            let width   = MsdfTexture::WIDTH  as i32;
            let height  = self.font.msdf_texture_rows() as i32;
            let texture = Texture::<GpuOnly,Rgb,u8>::new(&self.context,(width,height));
            self.font.with_borrowed_msdf_texture_data(|data| {
                texture.reload_with_content(data);
            });
            self.msdf_uniform.set(texture);
        }
    }
}



// ============
// === Line ===
// ============

/// A structure keeping line of glyphs with proper alignment.
///
/// Not all the glyphs in `glyphs` vector may be actually in use; this structure is meant to keep
/// changing text, and for best performance it re-uses the created Glyphs (what means the specific
/// buffer space). Therefore there is a cap for line length. See also `GlyphSystem::new_empty_line`.
#[derive(Debug)]
pub struct Line {
    glyphs         : Vec<Glyph>,
    baseline_start : Vector2<f32>,
    base_color     : Vector4<f32>,
    height         : f32,
    font           : FontHandle,
}

impl Line {
    /// Replace currently visible text.
    ///
    /// The replacing strings will reuse glyphs which increases performance of rendering text.
    pub fn replace_text<Chars>(&mut self, chars:Chars)
    where Chars : Iterator<Item=char> + Clone {
        let chars_count = chars.clone().count();
        let font        = self.font.clone_ref();
        let pen         = PenIterator::new(self.baseline_start,self.height,chars,font);

        for (glyph,(ch,position)) in self.glyphs.iter_mut().zip(pen) {
            let glyph_info = self.font.get_glyph_info(ch);
            let size       = glyph_info.scale.scale(self.height);
            let offset     = glyph_info.offset.scale(self.height);
            let x = position.x + offset.x;
            let y = position.y + offset.y;
            glyph.set_position(Vector3::new(x,y,0.0));
            glyph.set_glyph(ch);
            glyph.color().set(self.base_color);
            glyph.size().set(size);
        }
        for glyph in self.glyphs.iter_mut().skip(chars_count) {
            glyph.size().set(Vector2::new(0.0,0.0));
        }
    }

    /// Set the baseline start point for this line.
    pub fn set_baseline_start(&mut self, new_start:Vector2<f32>) {
        let offset = new_start - self.baseline_start;
        for glyph in self.glyphs.iter_mut() {
            glyph.mod_position(|pos| *pos += Vector3::new(offset.x,offset.y,0.0));
        }
        self.baseline_start = new_start;
    }
}


// === Getters ===

impl Line {
    /// The starting point of this line's baseline.
    pub fn baseline_start(&self) -> &Vector2<f32> {
        &self.baseline_start
    }

    /// Line's height in pixels.
    pub fn height(&self) -> f32 {
        self.height
    }

    /// Number of glyphs, giving the maximum length of displayed line.
    pub fn length(&self) -> usize {
        self.glyphs.len()
    }

    /// Font used for rendering this line.
    pub fn font(&self) -> FontHandle {
        self.font.clone_ref()
    }
}



/// ===================
/// === GlyphSystem ===
/// ===================

/// A system for displaying glyphs.
#[derive(Debug)]
pub struct GlyphSystem {
    context          : Context,
    sprite_system    : SpriteSystem,
    font             : FontHandle,
    color            : Buffer<Vector4<f32>>,
    glyph_msdf_index : Buffer<f32>,
    msdf_uniform     : Uniform<Texture<GpuOnly,Rgb,u8>>,
}

impl GlyphSystem {
    /// Constructor.
    pub fn new(world:&World, font:FontHandle) -> Self {
        let msdf_width    = MsdfTexture::WIDTH as f32;
        let msdf_height   = MsdfTexture::ONE_GLYPH_HEIGHT as f32;
        let scene         = world.scene();
        let context       = scene.context.clone_ref();
        let sprite_system = SpriteSystem::new(world);
        let symbol        = sprite_system.symbol();
        let texture       = Texture::<GpuOnly,Rgb,u8>::new(&context,(0,0));
        let mesh          = symbol.surface();

        sprite_system.set_material(Self::material());
        sprite_system.set_alignment(HorizontalAlignment::Left,VerticalAlignment::Bottom);
        scene.variables.add("msdf_range",GlyphRenderInfo::MSDF_PARAMS.range as f32);
        scene.variables.add("msdf_size",Vector2::new(msdf_width,msdf_height));
        Self {context,sprite_system,font,
            msdf_uniform       : symbol.variables().add_or_panic("msdf_texture",texture),
            color              : mesh.instance_scope().add_buffer("color"),
            glyph_msdf_index   : mesh.instance_scope().add_buffer("glyph_msdf_index"),
        }
    }

    /// Create new glyph. In the returned glyph the further parameters (position, bbox, character)
    /// may be set.
    pub fn new_glyph(&mut self) -> Glyph {
        let context         = self.context.clone();
        let sprite          = self.sprite_system.new_instance();
        let instance_id     = sprite.instance_id;
        let color_attr      = self.color.at(instance_id);
        let msdf_index_attr = self.glyph_msdf_index.at(instance_id);
        let font            = self.font.clone_ref();
        let msdf_uniform    = self.msdf_uniform.clone();
        color_attr.set(Vector4::new(0.0,0.0,0.0,0.0));
        msdf_index_attr.set(0.0);

        Glyph {context,sprite,msdf_index_attr,color_attr,font,msdf_uniform}
    }

    /// Create an empty "line" structure with defined number of glyphs. In the returned `Line`
    /// structure you can set specific strings with no more than `length` characters.
    ///
    /// For details, see also `Line` structure documentation.
    pub fn new_empty_line
    (&mut self, baseline_start:Vector2<f32>, height:f32, length:usize, color:Vector4<f32>)
    -> Line {
        let glyphs     = (0..length).map(|_| self.new_glyph()).collect();
        let base_color = color;
        let font       = self.font.clone_ref();
        Line {glyphs,baseline_start,height,base_color,font}
    }

    /// Create a line of glyphs with proper alignment.
    ///
    /// For details, see also `Line` structure documentation.
    pub fn new_line
    (&mut self, baseline_start:Vector2<f32>, height:f32, text:&str, color:Vector4<f32>)
    -> Line {
        let length   = text.chars().count();
        let mut line = self.new_empty_line(baseline_start,height,length,color);
        line.replace_text(text.chars());
        line
    }

    /// Get underlying sprite system.
    pub fn sprite_system(&self) -> &SpriteSystem {
        &self.sprite_system
    }
}

impl<'t> From<&'t GlyphSystem> for &'t display::object::Node {
    fn from(glyph_system:&'t GlyphSystem) -> Self {
        glyph_system.sprite_system.display_object()
    }
}


// === Private ===

impl GlyphSystem {
    /// Defines a default material of this system.
    fn material() -> Material {
        let mut material = Material::new();
        material.add_input_def::<FloatSampler>("msdf_texture");
        material.add_input_def::<Vector2<f32>>("msdf_size");
        material.add_input_def::<f32>         ("glyph_msdf_index");
        material.add_input("pixel_ratio", 1.0);
        material.add_input("zoom"       , 1.0);
        material.add_input("msdf_range" , GlyphRenderInfo::MSDF_PARAMS.range as f32);
        material.add_input("color"      , Vector4::new(0.0,0.0,0.0,1.0));
        // FIXME We need to use this output, as we need to declare the same amount of shader
        // FIXME outputs as the number of attachments to framebuffer. We should manage this more
        // FIXME intelligent. For example, we could allow defining output shader fragments,
        // FIXME which will be enabled only if pass of given attachment type was enabled.
        material.add_output("id", Vector4::<u32>::new(0,0,0,0));

        let code = CodeTemplate::new(BEFORE_MAIN.to_string(),MAIN.to_string(),"".to_string());
        material.set_code(code);
        material
    }
}

const BEFORE_MAIN : &str = include_str!("glyph.glsl");
const MAIN        : &str = "output_color = color_from_msdf(); output_id=uvec4(0,0,0,0);";
