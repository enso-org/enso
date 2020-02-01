//! A main glyph system implementation.

use crate::prelude::*;

use crate::display::layout::types::*;
use crate::display::shape::glyph::font::FontId;
use crate::display::shape::glyph::font::FontRenderInfo;
use crate::display::shape::glyph::font::FontRegistry;
use crate::display::shape::glyph::pen::PenIterator;
use crate::display::shape::glyph::msdf::MsdfTexture;
use crate::display::symbol::material::Material;
use crate::display::symbol::shader::builder::CodeTemplate;
use crate::display::world::*;
use crate::system::gpu::texture::*;
use crate::system::gpu::types::*;

use nalgebra::Vector2;
use nalgebra::Vector4;


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
    font_id         : FontId,
    msdf_uniform    : Uniform<Texture<GpuOnly,Rgb,u8>>,
}

impl Glyph {
    /// Glyph color attribute accessor.
    pub fn color(&self) -> Attribute<Vector4<f32>> {
        self.color_attr.clone_ref()
    }

    /// Change the displayed character.
    pub fn set_glyph(&mut self, ch:char, fonts:&mut FontRegistry) {
        let font       = fonts.get_render_info(self.font_id);
        let glyph_info = font.get_glyph_info(ch);
        self.msdf_index_attr.set(glyph_info.msdf_texture_glyph_id as f32);
        self.update_msdf_texture(fonts);
    }

    fn update_msdf_texture(&mut self, fonts:&mut FontRegistry) {
        let font = fonts.get_render_info(self.font_id);
        let texture_changed = self.msdf_uniform.with_content(|texture| {
            texture.storage().height != font.msdf_texture().rows() as i32
        });
        if texture_changed {
            let msdf_texture = font.msdf_texture();
            let data         = msdf_texture.data.as_slice();
            let width        = MsdfTexture::WIDTH  as i32;
            let height       = msdf_texture.rows() as i32;
            let texture = Texture::<GpuOnly,Rgb,u8>::new(&self.context,(width,height));
            texture.reload_with_content(data);
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
    font_id        : FontId,
}

impl Line {
    /// Replace currently visible text.
    ///
    /// The replacing strings will reuse glyphs which increases performance of rendering text.
    pub fn replace_text<Chars>(&mut self, chars:Chars, fonts:&mut FontRegistry)
    where Chars : Iterator<Item=char> + Clone {
        let font = fonts.get_render_info(self.font_id);
        let pen  = PenIterator::new(self.baseline_start,self.height,chars.clone(),font);

        for (glyph,(_,position)) in self.glyphs.iter_mut().zip(pen) {
            glyph.set_position(Vector3::new(position.x,position.y,0.0));
        }
        for (glyph,ch) in self.glyphs.iter_mut().zip(chars) {
            let font       = fonts.get_render_info(self.font_id);
            let glyph_info = font.get_glyph_info(ch);
            let size       = glyph_info.scale.scale(self.height);
            let offset     = glyph_info.offset.scale(self.height);
            glyph.set_glyph(ch,fonts);
            glyph.color().set(self.base_color);
            glyph.mod_position(|pos| { *pos += Vector3::new(offset.x,offset.y,0.0); });
            glyph.size().set(size);
        }
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
    font_id          : FontId,
    color            : Buffer<Vector4<f32>>,
    glyph_msdf_index : Buffer<f32>,
    msdf_uniform     : Uniform<Texture<GpuOnly,Rgb,u8>>,
}

impl GlyphSystem {
    /// Constructor.
    pub fn new(world:&World, font_id:FontId) -> Self {
        let msdf_width    = MsdfTexture::WIDTH as f32;
        let msdf_height   = MsdfTexture::ONE_GLYPH_HEIGHT as f32;
        let scene         = world.scene();
        let context       = scene.context();
        let sprite_system = SpriteSystem::new(world);
        let symbol        = sprite_system.symbol();
        let texture       = Texture::<GpuOnly,Rgb,u8>::new(&context,(0,0));
        let mesh          = symbol.surface();

        sprite_system.set_material(Self::material());
        sprite_system.set_alignment(HorizontalAlignment::Left,VerticalAlignment::Bottom);
        scene.variables().add("msdf_range",FontRenderInfo::MSDF_PARAMS.range as f32);
        scene.variables().add("msdf_size",Vector2::new(msdf_width,msdf_height));
        Self {context,sprite_system,font_id,
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
        let instance_id     = sprite.instance_id();
        let color_attr      = self.color.at(instance_id);
        let msdf_index_attr = self.glyph_msdf_index.at(instance_id);
        let font_id         = self.font_id;
        let msdf_uniform    = self.msdf_uniform.clone();
        color_attr.set(Vector4::new(0.0,0.0,0.0,0.0));
        msdf_index_attr.set(0.0);

        Glyph {context,sprite,msdf_index_attr,color_attr,font_id,msdf_uniform}
    }

    /// Create an empty "line" structure with defined number of glyphs. In the returned `Line`
    /// structure you can set specific strings with no more than `length` characters.
    ///
    /// For details, see also `Line` structure documentation.
    pub fn new_empty_line
    ( &mut self
    , baseline_start : Vector2<f32>
    , height         : f32
    , length         : usize
    , color          : Vector4<f32>) -> Line {
        let glyphs     = (0..length).map(|_| self.new_glyph()).collect();
        let base_color = color;
        let font_id    = self.font_id;
        Line {glyphs,baseline_start,height,base_color,font_id}
    }

    /// Create a line of glyphs with proper alignment.
    ///
    /// For details, see also `Line` structure documentation.
    pub fn new_line
    ( &mut self
    , baseline_start : Vector2<f32>
    , height         : f32
    , text           : &str
    , color          : Vector4<f32>
    , fonts          : &mut FontRegistry) -> Line {
        let length   = text.chars().count();
        let mut line = self.new_empty_line(baseline_start,height,length,color);
        line.replace_text(text.chars(),fonts);
        line
    }

    /// Get underlying sprite system.
    pub fn sprite_system(&self) -> &SpriteSystem {
        &self.sprite_system
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
        material.add_input("msdf_range" , FontRenderInfo::MSDF_PARAMS.range as f32);
        material.add_input("color"      , Vector4::new(0.0,0.0,0.0,1.0));

        let code = CodeTemplate::new(BEFORE_MAIN.to_string(),MAIN.to_string(),"".to_string());
        material.set_code(code);
        material
    }
}

const BEFORE_MAIN : &str = include_str!("glyph.glsl");
const MAIN        : &str = "output_color = color_from_msdf();";
