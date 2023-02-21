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
use ensogl_core::display;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;



// ==============
// === Shapes ===
// ==============

mod rectangle {
    use super::*;
    ensogl_core::shape! {
        (style: Style, color: Vector4<f32>) {
            let width = Var::<Pixels>::from("input_size.x");
            let height = Var::<Pixels>::from("input_size.y");
            let rect = Rect((&width, &height)).corners_radius(10.0.px());
            let shape = rect.fill(color::Rgba(0.0,0.0,0.0,0.2));
            shape.into()
        }
    }
}



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

    let rect1 = rectangle::View::new();
    rect1.set_size(Vector2::new(100.0, 100.0));
    rect1.color.set(color::Rgba::new(0.5, 0.0, 0.0, 0.3).into());

    let rect2 = rectangle::View::new();
    rect2.set_size(Vector2::new(100.0, 100.0));
    rect2.color.set(color::Rgba::new(0.5, 0.0, 0.0, 0.3).into());

    let root = display::object::Instance::new();
    root.set_size(Vector2::new(300.0, 100.0));
    root.use_auto_layout();
    root.add_child(&rect1);
    root.add_child(&rect2);
    world.add_child(&root);

    warn!("rect1: {:?}", rect1.display_object());

    let r = rect1.clone_ref();
    let mut i = 0;
    world
        .on
        .before_frame
        .add(move |_| {
            if i == 10 {
                warn!("rect1: {:?}", r.display_object());
                // warn!("rect1 sprite: {:?}", r.sprite.borrow().display_object());
            }
            i += 1;
        })
        .forget();

    world.keep_alive_forever();
    mem::forget(navigator);
    mem::forget(root);
    mem::forget(rect1);
    mem::forget(rect2);
}
