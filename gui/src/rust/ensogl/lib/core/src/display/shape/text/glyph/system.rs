//! A main glyph system implementation.

use crate::prelude::*;

use crate::display::layout::Alignment;
use crate::display::shape::text::glyph::font;
use crate::display::shape::text::glyph::font::GlyphRenderInfo;
use crate::display::shape::text::glyph::pen::PenIterator;
use crate::display::shape::text::glyph::msdf;
use crate::display::symbol::material::Material;
use crate::display::symbol::shader::builder::CodeTemplate;
use crate::display::world::*;
use crate::display::scene::Scene;
use crate::system::gpu::texture::*;
use crate::system::gpu::types::*;
use crate::display::object::traits::*;
use crate::data::color;

use nalgebra::Vector2;
use nalgebra::Vector4;
use crate::display;



// =============
// === Glyph ===
// =============

/// A glyph rendered on screen. The displayed character will be stretched to fit the entire size of
/// underlying sprite.
#[derive(Clone,CloneRef,Debug,Shrinkwrap)]
pub struct Glyph {
    #[shrinkwrap(main_field)]
    sprite          : Sprite,
    context         : Context,
    font            : font::Handle,
    color_attr      : Attribute<Vector4<f32>>,
    msdf_index_attr : Attribute<f32>,
    msdf_uniform    : Uniform<Texture<GpuOnly,Rgb,u8>>,
}

impl Glyph {
    /// Glyph color attribute accessor.
    pub fn color(&self) -> Attribute<Vector4<f32>> {
        self.color_attr.clone_ref()
    }

    /// Change the displayed character.
    pub fn set_glyph(&self, ch:char) {
        let glyph_info = self.font.get_glyph_info(ch);
        self.msdf_index_attr.set(glyph_info.msdf_texture_glyph_id as f32);
        self.update_msdf_texture();
    }

    fn update_msdf_texture(&self) {
        let texture_changed = self.msdf_uniform.with_content(|texture| {
            texture.storage().height != self.font.msdf_texture_rows() as i32
        });
        if texture_changed {
            let width   = msdf::Texture::WIDTH as i32;
            let height  = self.font.msdf_texture_rows() as i32;
            let texture = Texture::<GpuOnly,Rgb,u8>::new(&self.context,(width,height));
            self.font.with_borrowed_msdf_texture_data(|data| {
                texture.reload_with_content(data);
            });
            self.msdf_uniform.set(texture);
        }
    }
}

impl display::Object for Glyph {
    fn display_object(&self) -> &display::object::Instance {
        self.sprite.display_object()
    }
}



// ============
// === Line ===
// ============

/// A structure keeping line of glyphs with proper alignment.
///
/// Not all the glyphs in `glyphs` vector may be actually in use. This structure is meant to keep
/// changing text, and for best performance it re-uses the created Glyphs (what means the specific
/// buffer space). Therefore you can set a cap for line length by using the `set_fixed_capacity`
/// method.
#[derive(Clone,CloneRef,Debug)]
pub struct Line {
    display_object : display::object::Instance,
    glyph_system   : GlyphSystem,
    content        : Rc<RefCell<String>>,
    glyphs         : Rc<RefCell<Vec<Glyph>>>,
    font_color     : Rc<Cell<color::Rgba>>,
    font_size      : Rc<Cell<f32>>,
    fixed_capacity : Rc<Cell<Option<usize>>>,
}

impl Line {
    /// Constructor.
    pub fn new(logger:impl AnyLogger, glyph_system:&GlyphSystem) -> Self {
        let logger         = Logger::sub(logger,"line");
        let display_object = display::object::Instance::new(logger);
        let glyph_system   = glyph_system.clone_ref();
        let font_size      = Rc::new(Cell::new(11.0));
        let font_color     = Rc::new(Cell::new(color::Rgba::new(0.0,0.0,0.0,1.0)));
        let content        = default();
        let glyphs         = default();
        let fixed_capacity = default();
        Line {display_object,glyph_system,glyphs,font_size,font_color,content,fixed_capacity}
    }

    /// Replace currently visible text.
    pub fn set_text<S:Into<String>>(&self, content:S) {
        *self.content.borrow_mut() = content.into();
        self.redraw();
    }
}


// === Setters ===

#[allow(missing_docs)]
impl Line {
    pub fn set_font_color<C:Into<color::Rgba>>(&self, color:C) {
        let color = color.into();
        self.font_color.set(color);
        for glyph in &*self.glyphs.borrow() {
            glyph.color().set(color.into());
        }
    }

    pub fn set_font_size(&self, size:f32) {
        self.font_size.set(size);
        self.redraw();
    }

    pub fn change_fixed_capacity(&self, count:Option<usize>) {
        self.fixed_capacity.set(count);
        self.resize();
    }

    pub fn set_fixed_capacity(&self, count:usize) {
        self.change_fixed_capacity(Some(count));
    }

    pub fn unset_fixed_capacity(&self) {
        self.change_fixed_capacity(None);
    }
}


// === Getters ===

#[allow(missing_docs)]
impl Line {
    pub fn font_size(&self) -> f32 {
        self.font_size.get()
    }

    pub fn length(&self) -> usize {
        self.content.borrow().len()
    }

    pub fn font(&self) -> font::Handle {
        self.glyph_system.font.clone_ref()
    }
}


// === Internal API ===

impl Line {
    /// Resizes the line to contain enough glyphs to display the full `content`. In case the
    /// `fixed_capacity` was set, it will add or remove the glyphs to match it.
    fn resize(&self) {
        let content_len        = self.content.borrow().len();
        let target_glyph_count = self.fixed_capacity.get().unwrap_or(content_len);
        let glyph_count        = self.glyphs.borrow().len();
        if target_glyph_count > glyph_count {
            let new_count  = target_glyph_count - glyph_count;
            let new_glyphs = (0..new_count).map(|_| {
                let glyph = self.glyph_system.new_glyph();
                self.add_child(&glyph);
                glyph
            });
            self.glyphs.borrow_mut().extend(new_glyphs)
        }
        if glyph_count > target_glyph_count {
            self.glyphs.borrow_mut().truncate(target_glyph_count)
        }
    }

    /// Updates properties of all glyphs, including characters they display, size, and colors.
    fn redraw(&self) {
        self.resize();

        let content     = self.content.borrow();
        let font        = self.glyph_system.font.clone_ref();
        let font_size   = self.font_size.get();
        let chars       = content.chars();
        let pen         = PenIterator::new(font_size,chars,font);
        let content_len = content.len();
        let color       = self.font_color.get().into();

        for (glyph,(chr,x_offset)) in self.glyphs.borrow().iter().zip(pen) {
            let glyph_info   = self.glyph_system.font.get_glyph_info(chr);
            let size         = glyph_info.scale.scale(font_size);
            let glyph_offset = glyph_info.offset.scale(font_size);
            let glyph_x      = x_offset + glyph_offset.x;
            let glyph_y      = glyph_offset.y;
            glyph.set_position(Vector3::new(glyph_x,glyph_y,0.0));
            glyph.set_glyph(chr);
            glyph.color().set(color);
            glyph.size.set(size);
        }

        for glyph in self.glyphs.borrow().iter().skip(content_len) {
            glyph.size.set(Vector2::new(0.0,0.0));
        }
    }
}


// === Display Object ===

impl display::Object for Line {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}



// ===================
// === GlyphSystem ===
// ===================

/// A system for displaying glyphs.
#[derive(Clone,CloneRef,Debug)]
pub struct GlyphSystem {
    logger           : Logger,
    context          : Context,
    sprite_system    : SpriteSystem,
    font             : font::Handle,
    color            : Buffer<Vector4<f32>>,
    glyph_msdf_index : Buffer<f32>,
    msdf_uniform     : Uniform<Texture<GpuOnly,Rgb,u8>>,
}

impl GlyphSystem {
    /// Constructor.
    pub fn new<'t,S:Into<&'t Scene>>(scene:S, font:font::Handle) -> Self {
        let logger        = Logger::new("glyph_system");
        let msdf_width    = msdf::Texture::WIDTH as f32;
        let msdf_height   = msdf::Texture::ONE_GLYPH_HEIGHT as f32;
        let scene         = scene.into();
        let context       = scene.context.clone_ref();
        let sprite_system = SpriteSystem::new(scene);
        let symbol        = sprite_system.symbol();
        let texture       = Texture::<GpuOnly,Rgb,u8>::new(&context,(0,0));
        let mesh          = symbol.surface();

        sprite_system.set_material(Self::material());
        sprite_system.set_alignment(Alignment::bottom_left());
        scene.variables.add("msdf_range",GlyphRenderInfo::MSDF_PARAMS.range as f32);
        scene.variables.add("msdf_size",Vector2::new(msdf_width,msdf_height));
        Self {logger,context,sprite_system,font,
            msdf_uniform     : symbol.variables().add_or_panic("msdf_texture",texture),
            color            : mesh.instance_scope().add_buffer("color"),
            glyph_msdf_index : mesh.instance_scope().add_buffer("glyph_msdf_index"),
        }
    }

    /// Create new glyph. In the returned glyph the further parameters (position, size, character)
    /// may be set.
    pub fn new_glyph(&self) -> Glyph {
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

    /// Create a new `Line` of text.
    pub fn new_line(&self) -> Line {
        Line::new(&self.logger,self)
    }

    /// Get underlying sprite system.
    pub fn sprite_system(&self) -> &SpriteSystem {
        &self.sprite_system
    }
}

impl display::Object for GlyphSystem {
    fn display_object(&self) -> &display::object::Instance {
        self.sprite_system.display_object()
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
        material.add_input("z_zoom_1"   , 1.0);
        material.add_input("msdf_range" , GlyphRenderInfo::MSDF_PARAMS.range as f32);
        material.add_input("color"      , Vector4::new(0.0,0.0,0.0,1.0));
        // FIXME We need to use this output, as we need to declare the same amount of shader
        // FIXME outputs as the number of attachments to framebuffer. We should manage this more
        // FIXME intelligent. For example, we could allow defining output shader fragments,
        // FIXME which will be enabled only if pass of given attachment type was enabled.
        material.add_output("id", Vector4::<f32>::new(0.0,0.0,0.0,0.0));

        let code = CodeTemplate::new(FUNCTIONS,MAIN,"");
        material.set_code(code);
        material
    }
}

const FUNCTIONS : &str = include_str!("glyph.glsl");
const MAIN      : &str = "output_color = color_from_msdf(); output_id=vec4(0.0,0.0,0.0,0.0);";
