//! This module defines a geometry which always covers the whole screen. An example use case is
//! render pass implementation - rendering to framebuffer and then using the result with some
//! post-processing effect by applying the previous output to a screen covering geometry.


use crate::prelude::*;

use crate::display::symbol::geometry::Sprite;
use crate::display::symbol::geometry::SpriteSystem;
use crate::display::symbol::material::Material;
use crate::display::world::World;
use crate::system::gpu::data::texture;
use crate::system::gpu::data::types::*;



/// Defines a system containing shapes. It is a specialized `SpriteSystem` version.
#[derive(Clone,CloneRef,Debug)]
pub struct Screen {
    sprite        : Sprite,
    sprite_system : SpriteSystem,
}

impl Screen {
    /// Constructor.
    pub fn new(world:&World) -> Self {
        let sprite_system = SpriteSystem::new(world);
        sprite_system.set_geometry_material(Self::geometry_material());
        sprite_system.set_material(Self::surface_material());
        let sprite = sprite_system.new_instance();
        Self {sprite,sprite_system}
    }

    /// Hide the symbol. Hidden symbols will not be rendered.
    pub fn hide(&self) {
        self.sprite_system.hide();
    }

    /// Show the symbol. It will be rendered on next render call.
    pub fn show(&self) {
        self.sprite_system.show();
    }

    /// Local variables used by the screen object.
    pub fn variables(&self) -> UniformScope {
        self.sprite_system.symbol().variables().clone() // FIXME remove clone
    }

    /// Render the shape.
    pub fn render(&self) {
        self.sprite_system.render()
    }
}


// === Materials ===

impl Screen {
    fn geometry_material() -> Material {
        let mut material = Material::new();
        material.add_input_def::<Vector2<f32>>("uv");
        material.set_main("gl_Position = vec4((input_uv-0.5)*2.0,0.0,1.0);");
        material
    }

    fn surface_material() -> Material {
        let mut material = Material::new();
        material.add_input_def::<texture::FloatSampler>("pass_color");
        material.set_main("
        vec4 sample_color = texture(input_pass_color, input_uv);
        output_color = sample_color;
        ");
        material
    }
}
