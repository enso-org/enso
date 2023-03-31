//! Example scene showing the usage of display object auto-layout.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use ensogl_core::display::shape::*;
use ensogl_core::display::world::*;
use ensogl_core::prelude::*;

use ensogl_core::animation;
use ensogl_core::data::color::Rgba;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;



// ==============
// === Shapes ===
// ==============

mod rectangle {
    use super::*;
    ensogl_core::shape! {
        (style: Style, color: Vector4<f32>) {
            let color = Var::<color::Rgba>::from(color);
            let width = Var::<Pixels>::from("input_size.x");
            let height = Var::<Pixels>::from("input_size.y");
            let rect = Rect((&width, &height)).corners_radius(5.0.px());
            let shape = rect.fill(color);
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
    let world = World::new().displayed_in("root").leak();
    let scene = &world.default_scene;
    let camera = scene.camera().clone_ref();
    let _navigator = Navigator::new(scene, &camera).leak();

    let root = scene.new_child_named("root").leak();
    root.use_auto_layout().set_gap((5.0, 5.0));

    let rect1 = root.new_child_named("rect1").leak();
    let rect1_bg = rect1.new_child_named("rect1_bg").leak();
    rect1_bg.use_auto_layout().allow_grow().set_size((0.0, 0.0));

    let rect1_bg_shape = rectangle::View::new().leak();
    rect1_bg.add_child(&rect1_bg_shape);
    rect1_bg_shape.color.set(Rgba::new(0.0, 0.0, 0.0, 0.3).into());
    rect1_bg_shape.allow_grow().set_size((0.0, 0.0));

    let rect1_content = rect1.new_child_named("rect1_content").leak();
    rect1_content.use_auto_layout().set_gap((5.0, 5.0)).set_padding_all(5.0);

    let inner1 = rectangle::View::new().leak();
    rect1_content.add_child(&inner1);
    inner1.set_size((40, 40)).set_alignment_bottom();
    inner1.color.set(Rgba::new(1.0, 0.0, 0.0, 0.4).into());

    let inner2 = rectangle::View::new().leak();
    rect1_content.add_child(&inner2);
    inner2.set_size((40, 40)).set_alignment_center();
    inner2.color.set(Rgba::new(0.0, 1.0, 0.0, 0.4).into());

    let inner3 = rectangle::View::new().leak();
    rect1_content.add_child(&inner3);
    inner3.set_size((40, 40)).set_alignment_top();
    inner3.color.set(Rgba::new(0.0, 0.0, 1.0, 0.4).into());


    let rect2 = rectangle::View::new().leak();
    root.add_child(&rect2);
    rect2.set_size(Vector2::new(100.0, 100.0));
    rect2.color.set(Rgba::new(0.5, 0.0, 0.0, 0.3).into());

    let rect2_inner = rectangle::View::new().leak();
    rect2.add_child(&rect2_inner);
    rect2_inner.set_size((40, 40));
    rect2_inner.color.set(Rgba::new(1.0, 1.0, 1.0, 0.4).into());


    let mut logged = false;
    world
        .on
        .before_frame
        .add(move |t: animation::TimeInfo| {
            let s = t.since_animation_loop_started.as_s();
            let x = (s * std::f32::consts::PI).sin() * 30.0 + 50.0;
            let y = (s * 1.3 * std::f32::consts::PI).cos() * 40.0 + 50.0;
            inner2.set_size((x, y));

            if !logged {
                logged = true;
                warn!("rect1: {:?}", rect1.display_object());
                warn!("rect2: {:?}", rect2.display_object());
            }
        })
        .forget();
}
