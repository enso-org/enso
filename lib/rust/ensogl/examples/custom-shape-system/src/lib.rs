//! Example scene showing simple usage of a shape system.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use ensogl_core::display::shape::*;
use ensogl_core::display::world::*;
use ensogl_core::prelude::*;

use enso_frp as frp;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;



// ==============
// === Shapes ===
// ==============

mod shape {
    use super::*;
    ensogl_core::shape! {
        alignment = center;
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

    let view = shape::View::new();
    view.set_size((300.0, 300.0));
    view.set_position(Vector3::new(50.0, 50.0, 0.0));

    world.add_child(&view);
    world.keep_alive_forever();

    frp::new_network! { network
        trace view.events_deprecated.mouse_over;
        trace view.events_deprecated.mouse_out;
        trace view.events_deprecated.mouse_down;
    }

    let mut i = 0;
    world
        .on
        .before_frame
        .add(move |_time| {
            let _keep_alive = &network;
            let _keep_alive = &view;
            let _keep_alive = &navigator;
            i += 1;
            if i == 5 {
                if let Some(program) = view.sprite.borrow().symbol.shader.borrow_mut().program() {
                    debug!("\n\nVERTEX:\n{}", program.shader.vertex.code);
                    debug!("\n\nFRAGMENT:\n{}", program.shader.fragment.code);
                }
            }
        })
        .forget();
}
