//! Example scene showing the usage of display object auto-layout.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// FIXME: remove
#![feature(local_key_cell_methods)]

use ensogl_core::display::material::Material;
use ensogl_core::display::shape::*;
use ensogl_core::display::world::*;
use ensogl_core::prelude::*;

use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::texture::FloatSampler;


// ==============
// === Shapes ===
// ==============

mod icon1 {
    use super::*;
    ensogl_core::cached_shape! { 32 x 32;
        () {
            let shape = Circle(16.px()).fill(color::Rgba::green());
            shape.into()
        }
    }
}

mod icon2 {
    use super::*;
    ensogl_core::cached_shape! { 202 x 312;
        () {
            let shape = Rect((200.px(), 310.px())).fill(color::Rgba::red());
            shape.into()
        }
    }
}



// ======================
// === TexturePreview ===
// ======================


#[derive(Debug, Clone, Copy)]
#[allow(missing_docs)]
pub struct TextureSystemData {}

impl TextureSystemData {
    /// Defines a default material of this system.
    fn material() -> Material {
        let mut material = Material::new();
        let shader = "output_color = vec4(input_uv.x, input_uv.y, 0.0, 1.0); output_id=vec4(0.0,0.0,0.0,0.0);";
        let shader = "output_color = texture(input_pass_cached_shapes,input_uv); output_id=vec4(0.0,0.0,0.0,0.0);";
        material.add_input_def::<FloatSampler>("pass_cached_shapes");
        material.add_output("id", Vector4::<f32>::new(0.0, 0.0, 0.0, 0.0));
        material.set_main(shader);
        material
    }
}

mod texture {
    use super::*;
    ensogl_core::shape_old! {
        type SystemData = TextureSystemData;
        (style: Style,) {
            // The shape does not matter. The [`SystemData`] defines custom GLSL code.
            Plane().into()
        }
    }
}

mod background {
    use super::*;
    ensogl_core::shape! {
        below = [texture, icon1, icon2];
        (style: Style,) {
            Rect((1024.0.px(), 1024.0.px())).fill(color::Rgba::white()).into()
        }
    }
}

impl CustomSystemData<texture::Shape> for TextureSystemData {
    fn new(data: &ShapeSystemStandardData<texture::Shape>, (): &()) -> Self {
        *data.model.material.borrow_mut() = Self::material();
        *data.model.geometry_material.borrow_mut() = SpriteSystem::default_geometry_material();
        data.model.do_not_use_shape_definition.set(true);

        TextureSystemData {}
    }
}



// ===================
// === Entry Point ===
// ===================

/// The example entry point.
/// foo bar baz
#[entry_point]
#[allow(dead_code)]
pub fn main() {
    // icon1::cached_shape_system_definition::register_cached_shape();
    // icon2::cached_shape_system_definition::register_cached_shape();
    let world = World::new().displayed_in("root");
    let scene = &world.default_scene;
    let camera = scene.camera().clone_ref();
    let navigator = Navigator::new(scene, &camera);

    let background = background::View::new();
    background.set_size(Vector2(1024.0, 1024.0));
    background.set_xy(Vector2(100.0, 0.0));
    world.default_scene.add_child(&background);
    world.default_scene.layers.main.add(&background);

    let texture_preview = texture::View::new();
    texture_preview.set_size(Vector2(1024.0, 1024.0));
    texture_preview.set_xy(Vector2(100.0, 0.0));
    world.default_scene.add_child(&texture_preview);
    world.default_scene.layers.main.add(&texture_preview);

    let icon1 = icon1::View::new();
    icon1.set_size(Vector2(32.0, 32.0));
    icon1.set_xy(Vector2(-50.0, -50.0));
    world.default_scene.add_child(&icon1);
    world.default_scene.layers.main.add(&icon1);

    let icon2 = icon2::View::new();
    icon2.set_size(Vector2(20.0, 20.0));
    icon2.set_xy(Vector2(50.0, 50.0));
    world.default_scene.add_child(&icon2);
    world.default_scene.layers.main.add(&icon2);

    world.keep_alive_forever();
    mem::forget(navigator);
    mem::forget(background);
    mem::forget(texture_preview);
    mem::forget(icon1);
    mem::forget(icon2);
}
