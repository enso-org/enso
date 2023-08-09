//! Example scene showing the usage of built-in high-level shapes.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use ensogl_core::display::shape::compound::rectangle::*;
use ensogl_core::display::world::*;
use ensogl_core::prelude::*;

use ensogl_core::animation::TimeInfo;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;



// ===================
// === Entry Point ===
// ===================

/// The example entry point.
#[entry_point]
#[allow(dead_code)]
pub fn main() {
    let world = World::new().displayed_in("root");
    let scene = &world.default_scene;
    let camera = scene.camera().clone_ref();
    let navigator = Navigator::new(scene, &camera);

    let border_demo_1 = RoundedRectangle(10.0).build(|t| {
        t.set_size(Vector2::new(100.0, 100.0))
            .set_color(color::Rgba::new(0.8, 0.2, 0.2, 0.5))
            .set_border_color(color::Rgba::new(0.0, 0.5, 0.5, 1.0));
    });
    let border_demo_2 = RoundedRectangle(10.0).build(|t| {
        t.set_size(Vector2::new(100.0, 100.0))
            .set_color(color::Rgba::new(0.2, 0.8, 0.2, 0.5))
            .set_border_color(color::Rgba::new(0.2, 0.2, 0.5, 1.0));
    });
    let border_demo_3 = RoundedRectangle(10.0).build(|t| {
        t.set_size(Vector2::new(100.0, 100.0))
            .set_color(color::Rgba::new(0.2, 0.2, 0.8, 0.5))
            .set_border_color(color::Rgba::new(0.2, 0.5, 0.2, 1.0));
    });


    let shapes = [
        Circle().build(|t| {
            t.set_size(Vector2::new(100.0, 100.0))
                .set_color(color::Rgba::new(0.5, 0.0, 0.0, 0.3))
                .set_border_and_inset(5.0)
                .set_border_color(color::Rgba::new(0.0, 0.0, 1.0, 1.0))
                .keep_bottom_left_quarter();
        }),
        RoundedRectangle(10.0).build(|t| {
            t.set_size(Vector2::new(100.0, 100.0))
                .set_color(color::Rgba::new(0.5, 0.0, 0.0, 0.3))
                .set_border_and_inset(5.0)
                .set_border_color(color::Rgba::new(0.0, 0.0, 1.0, 1.0));
        }),
        RoundedRectangle(10.0).build(|t| {
            t.set_size(Vector2::new(100.0, 50.0))
                .set_color(color::Rgba::new(0.5, 0.0, 0.0, 0.3))
                .set_border_and_inset(5.0)
                .set_border_color(color::Rgba::new(0.0, 0.0, 1.0, 1.0))
                .keep_top_half();
        }),
        RoundedRectangle(10.0).build(|t| {
            t.set_size(Vector2::new(100.0, 50.0))
                .set_color(color::Rgba::new(0.5, 0.0, 0.0, 0.3))
                .set_border_and_inset(5.0)
                .set_border_color(color::Rgba::new(0.0, 0.0, 1.0, 1.0))
                .keep_bottom_half();
        }),
        RoundedRectangle(10.0).build(|t| {
            t.set_size(Vector2::new(50.0, 100.0))
                .set_color(color::Rgba::new(0.5, 0.0, 0.0, 0.3))
                .set_border_and_inset(5.0)
                .set_border_color(color::Rgba::new(0.0, 0.0, 1.0, 1.0))
                .keep_right_half();
        }),
        RoundedRectangle(10.0).build(|t| {
            t.set_size(Vector2::new(50.0, 100.0))
                .set_color(color::Rgba::new(0.5, 0.0, 0.0, 0.3))
                .set_border_and_inset(5.0)
                .set_border_color(color::Rgba::new(0.0, 0.0, 1.0, 1.0))
                .keep_left_half();
        }),
        SimpleTriangle::from_base_and_altitude(100.0, 25.0).into(),
        SimpleTriangle::from_base_and_altitude(100.0, 50.0).into(),
        SimpleTriangle::from_base_and_altitude(100.0, 100.0).into(),
        border_demo_1.clone(),
        border_demo_2.clone(),
        border_demo_3.clone(),
    ];

    let root = display::object::Instance::new();
    root.set_size(Vector2::new(300.0, 100.0));
    root.use_auto_layout().set_column_count(3).set_gap((10.0, 10.0));
    for shape in &shapes {
        root.add_child(shape);
    }
    world.add_child(&root);

    world
        .on
        .before_frame
        .add(move |time: TimeInfo| {
            let t = time.frame_start().as_s();
            let inset = 10.0 + (t * 1.3).sin() * 10.0;
            let width = 10.0 + (t * 2.77).cos() * 10.0;
            border_demo_1.set_inset(inset).set_border(width);
            border_demo_2.set_inset(inset).set_frame_border(width);
            border_demo_3.set_inner_border(width, inset);
        })
        .forget();

    world.keep_alive_forever();
    mem::forget(navigator);
    mem::forget(root);
    mem::forget(shapes);
}
