//! This module defines a "shape system". It is a wrapper over a "sprite system" and it defines
//! the required default material parameters.

use crate::prelude::*;

use crate::display::symbol::geometry::SpriteSystem;
use crate::display::symbol::material::Material;
use crate::display::shape::primitive::shader;
use crate::display::shape::primitive::def::class::Shape;
use crate::display::object::*;


/// Defines a system containing shapes. It is a specialized `SpriteSystem` version.
#[derive(Debug,Shrinkwrap)]
pub struct ShapeSystem {
    /// The underlying `SpriteSystem`.
    pub sprite_system: SpriteSystem
}

impl ShapeSystem {
    /// Constructor.
    pub fn new<S:Shape>(shape:&S) -> Self {
        let sprite_system = SpriteSystem::new();
        sprite_system.set_material(Self::material(shape));
        Self {sprite_system}
    }

    /// Defines a default material of this system.
    fn material<S:Shape>(shape:&S) -> Material {
        let mut material = Material::new();
        material.add_input("pixel_ratio"  , 1.0);
        material.add_input("zoom"         , 1.0);
        material.add_input("time"         , 0.0);
        material.add_input("display_mode" , 0);
        let code = shader::builder::Builder::run(shape);
        material.set_code(code);
        material
    }
}

impl From<&ShapeSystem> for DisplayObjectData {
    fn from(t:&ShapeSystem) -> Self {
        (&t.sprite_system).into()
    }
}
