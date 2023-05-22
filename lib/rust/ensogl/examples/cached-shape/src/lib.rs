//! Example scene showing the usage of display object auto-layout.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use ensogl_core::display::shape::*;
use ensogl_core::display::world::*;
use ensogl_core::prelude::*;

use ensogl_core::data::color;
use ensogl_core::display::material::Material;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::texture::FloatSampler;



// ==============
// === Shapes ===
// ==============

mod icon1 {
    use super::*;
    ensogl_core::cached_shape! {
        size = (32, 32);
        alignment = center;
        (_style: Style) {
            let shape = Circle(16.px()).fill(color::Rgba::red());
            shape.recolorize(color::Rgba::green(), color::Rgba::red(), color::Rgba::blue()).into()
        }
    }
}

mod icon2 {
    use super::*;
    ensogl_core::cached_shape! {
        size = (200, 310);
        alignment = center;
        (_style: Style) {
            let shape = Rect((200.px(), 310.px())).fill(color::Rgba::red());
            shape.into()
        }
    }
}

/// A rounded rectangle with an arrow pointing in from the left.
pub mod data_input {
    use super::*;
    use std::f32::consts::PI;

    /// An arrow shape consisting of a straight line and a triangular head. The arrow points upwards
    /// and the tip is positioned at the origin.
    pub fn arrow(length: f32, width: f32, head_length: f32, head_width: f32) -> AnyShape {
        // We overlap the line with the head by this amount to make sure that the renderer does not
        // display a gap between them.
        const OVERLAP: f32 = 1.0;
        let line_length = length - head_length + OVERLAP;
        let line = Rect((width.px(), line_length.px()));
        let line = line.translate_y((-line_length / 2.0 - head_length + OVERLAP).px());
        let head = Triangle(head_width, head_length).translate_y((-head_length / 2.0).px());
        (line + head).into()
    }

    ensogl_core::cached_shape! {
        size = (16, 16);
        alignment = center;
        (style: Style) {
            let vivid_color: Var<color::Rgba> = "srgba(1.0, 0.0, 0.0, 1.0)".into();
            let dull_color: Var<color::Rgba> = "srgba(0.0, 1.0, 0.0, 1.0)".into();

            // === Rectangle ===

            let rect = Rect((11.0.px(),12.0.px())).corners_radius(2.0.px());
            let rect = rect.translate_x(2.5.px());
            let rect = rect.fill(dull_color);


            // === Arrow ===

            let arrow = arrow(11.0,2.0,4.0,6.0).rotate((PI/2.0).radians());
            let arrow = arrow.translate_x(4.0.px());
            let arrow = arrow.fill(vivid_color);


            // === Shape ===

            (rect + arrow).into()
        }
    }
}

mod shape {
    use super::*;
    ensogl_core::shape! {
        alignment = center;
        (_style: Style, shape: cached::AnyCachedShape, color: Vector4) {
            let bg = Rect((100.px(), 100.px())).fill(color::Rgba::white());
            let vivid_color: Var<color::Rgba> = color.into();
            let dull_color = vivid_color.clone().multiply_alpha(&Var::Static(0.5));
            let colored = shape.recolorize(vivid_color, dull_color, color::Rgba::default());
            let with_bg = &bg + &colored;
            with_bg.into()
        }
    }
}


// ======================
// === TexturePreview ===
// ======================

/// The system which sets the material rendering pass_cached_shapes texture. Used here for debugging
/// purposes.
#[derive(Debug, Clone, Copy)]
#[allow(missing_docs)]
pub struct TextureSystemData {}

impl TextureSystemData {
    fn material() -> Material {
        let mut material = Material::new();
        let shader = "output_color = texture(input_pass_cached_shapes,input_uv); output_color.rgb *= output_color.a; output_id=vec4(0.0,0.0,0.0,0.0);";
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
        (style: Style) {
            // The shape does not matter. The [`SystemData`] defines custom GLSL code.
            Plane().into()
        }
    }
}

mod background {
    use super::*;
    ensogl_core::shape! {
        below = [texture, icon1, icon2];
        alignment = center;
        (style: Style) {
            Rect((296.0.px(), 326.0.px())).fill(color::Rgba::black()).into()
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

#[entry_point]
#[allow(dead_code)]
pub fn main() {
    let world = World::new().displayed_in("root");
    let scene = &world.default_scene;
    let camera = scene.camera().clone_ref();
    let navigator = Navigator::new(scene, &camera);

    let background = background::View::new();
    background.set_size(Vector2(296.0, 326.0));
    background.set_xy(Vector2(0.0, -1000.0));
    world.default_scene.add_child(&background);
    world.default_scene.layers.main.add(&background);

    let texture_preview = texture::View::new();
    texture_preview.set_size(Vector2(296.0, 326.0));
    texture_preview.set_xy(Vector2(0.0, -1000.0));
    world.default_scene.add_child(&texture_preview);
    world.default_scene.layers.main.add(&texture_preview);

    let shapes = [
        shape::View::new(),
        shape::View::new(),
        shape::View::new(),
        shape::View::new(),
        shape::View::new(),
    ];
    for (index, shape) in shapes.iter().enumerate() {
        shape.set_x(-100.0 + 50.0 * index as f32);
        shape.set_size(Vector2(40.0, 40.0));
        shape.color.set(color::Rgba::black().into());
        world.default_scene.add_child(shape);
        world.default_scene.layers.main.add(shape);
    }
    shapes[0].shape.set(icon1::Shape::any_cached_shape_parameter());
    shapes[1].shape.set(icon2::Shape::any_cached_shape_parameter());
    shapes[2].shape.set(data_input::Shape::any_cached_shape_parameter());
    shapes[3].shape.set(data_input::Shape::any_cached_shape_parameter());
    shapes[3].color.set(color::Rgb::from_base_255(43.0, 117.0, 239.0).with_alpha(1.0).into());
    shapes[4].shape.set(data_input::Shape::any_cached_shape_parameter());
    shapes[4].color.set(color::Rgb::from_base_255(134.0, 135.0, 43.0).with_alpha(1.0).into());

    let icon1 = icon1::View::new();
    icon1.set_size(Vector2(100.0, 100.0));
    icon1.set_xy((-60.0, -120.0));
    world.default_scene.add_child(&icon1);
    world.default_scene.layers.main.add(&icon1);

    let icon2 = icon2::View::new();
    icon2.set_size(Vector2(100.0, 100.0));
    icon2.set_xy((60.0, -120.0));
    world.default_scene.add_child(&icon2);
    world.default_scene.layers.main.add(&icon2);

    let icon3 = data_input::View::new();
    icon3.set_size(Vector2(100.0, 100.0));
    icon3.set_xy((180.0, -120.0));
    world.default_scene.add_child(&icon3);
    world.default_scene.layers.main.add(&icon3);

    world.keep_alive_forever();
    mem::forget(navigator);
    mem::forget(background);
    mem::forget(texture_preview);
    mem::forget(shapes);
    mem::forget(icon1);
    mem::forget(icon2);
    mem::forget(icon3);
}
