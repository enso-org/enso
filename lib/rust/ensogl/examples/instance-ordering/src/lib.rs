//! Example scene showing the usage of built-in high-level shapes.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use ensogl_core::display::world::*;
use ensogl_core::prelude::*;

use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::display::shape::compound::rectangle;
use ensogl_core::display::shape::compound::rectangle::Circle;
use rand::seq::SliceRandom;
use rand::Rng;



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

    let mut rng = rand::thread_rng();
    let root = display::object::Instance::new();
    world.add_child(&root);
    let main = &world.default_scene.layers.main;
    let red = main.create_symbol_partition::<rectangle::Shape>("red");
    let green = main.create_symbol_partition::<rectangle::Shape>("green");
    let blue = main.create_symbol_partition::<rectangle::Shape>("blue");
    let layers = vec![
        (red.clone(), color::Rgba::new(1.0, 0.5, 0.5, 0.9), color::Rgba::new(1.0, 0.0, 0.0, 1.0)),
        (green.clone(), color::Rgba::new(0.5, 1.0, 0.5, 0.9), color::Rgba::new(0.0, 1.0, 0.0, 1.0)),
        (blue.clone(), color::Rgba::new(0.5, 0.5, 1.0, 0.9), color::Rgba::new(0.0, 0.0, 1.0, 1.0)),
    ];
    let mut shapes = vec![];
    shapes.resize(128, default());
    let mut i = 0;
    let handle = world.on.before_frame.add(move |_time_info| {
        let (layer, color, border) = layers.choose(&mut rng).unwrap();
        let new = Circle().build(|t| {
            t.set_size(Vector2::new(64.0, 64.0))
                .set_color(*color)
                .set_inset_border(5.0)
                .set_border_color(*border);
        });
        let x = rng.gen_range(-512.0..512.0);
        new.set_x(x);
        new.set_y(-256.0);
        root.add_child(&new);
        layer.add(&new);
        let t = rng.gen_range(0.0..std::f32::consts::PI * 2.0);
        let (old, _, _, _) = mem::replace(&mut shapes[i], (new, -256.0, x, t));
        red.remove(&old);
        green.remove(&old);
        blue.remove(&old);
        i += 1;
        if i == shapes.len() {
            i = 0;
        }
        for (shape, y, x, t) in &mut shapes {
            *y += 4.0;
            *x += t.sin() * 8.0;
            *t += std::f32::consts::PI / 30.0;
            shape.set_y(*y);
            shape.set_x(*x);
        }
    });

    world.keep_alive_forever();
    mem::forget(navigator);
    mem::forget(handle);
}
