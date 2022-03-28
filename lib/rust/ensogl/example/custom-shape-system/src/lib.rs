//! Example scene showing simple usage of a shape system.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]

use ensogl_core::display::shape::*;
use ensogl_core::display::world::*;
use ensogl_core::prelude::*;
use wasm_bindgen::prelude::*;

use enso_frp as frp;
use ensogl_core::data::color;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;



// ==============
// === Shapes ===
// ==============

mod shape {
    use super::*;
    ensogl_core::define_shape_system! {
        (style:Style) {
            let circle1    = Circle(50.px());
            let circle_bg  = circle1.translate_x(-(50.0.px()));
            let circle_sub = circle1.translate_y(-(50.0.px()));
            let rect       = Rect((100.0.px(),100.0.px()));
            let shape      = circle_bg + rect - circle_sub;
            let shape      = shape.fill(color::Rgba::new(0.3, 0.3, 0.3, 1.0));
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
    let logger = Logger::new("ShapeView");

    let view1 = shape::View::new(&logger);
    view1.size.set(Vector2::new(300.0, 300.0));
    view1.mod_position(|t| *t = Vector3::new(50.0, 50.0, 0.0));

    world.add_child(&view1);
    world.keep_alive_forever();

    frp::new_network! { network
        trace view1.events.mouse_over;
        trace view1.events.mouse_out;
        trace view1.events.mouse_down;
    }

    world
        .on
        .before_frame
        .add(move |_time| {
            let _keep_alive = &network;
            let _keep_alive = &view1;
            let _keep_alive = &navigator;
        })
        .forget();
}
