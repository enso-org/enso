//! Example scene showing the usage of focusing objects in display object hierarchy.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use ensogl_core::display::shape::*;
use ensogl_core::display::world::*;
use ensogl_core::prelude::*;

use ensogl_core::data::color;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::event::FocusIn;
use ensogl_core::event::FocusOut;
use ensogl_core::Animation;



// ==============
// === Shapes ===
// ==============

const BORDER_SIZE: f32 = 4.0;
const RECT_SIZE: f32 = 140.0;
const RECT_DIFF: f32 = 40.0;

mod rectangle {
    use super::*;
    ensogl_core::shape! {
        (style: Style, color: Vector4<f32>, border_size: f32, border_color: Vector4<f32>) {
            let width = Var::<Pixels>::from("input_size.x");
            let height = Var::<Pixels>::from("input_size.y");
            let rect = Rect((&width, &height)).corners_radius(10.0.px());
            let inside = rect.shrink(BORDER_SIZE.px());
            let border = &inside.grow((border_size - 1.0).px()) - &inside;
            let shape = border.fill(border_color) + inside.fill(color::Rgba(0.0,0.0,0.0,0.2));
            shape.into()
        }
    }
}

mod rectangle2 {
    use super::*;
    ensogl_core::shape! {
        (style: Style) {
            let rect = Rect((10.px(), 10.px()));
            let shape = rect.fill(color::Rgba::new(0.0, 1.0, 0.0, 1.0));
            shape.into()
        }
    }
}



// ===================
// === Entry Point ===
// ===================

fn define_rect(width: f32, height: f32, network: &frp::Network) -> rectangle::View {
    let rect = rectangle::View::new();
    rect.set_size((width, height));
    rect.color.set(color::Rgba::new(0.0, 0.0, 0.0, 0.3).into());

    let border_size = Animation::<f32>::new(network);
    let border_color = color::Animation::new(network);

    // Please note that this clones [`rect`] refs to closures, so [`network`] keeps them alive.
    frp::extend! { network
        eval_ rect.events.mouse_down (rect.focus());

        eval border_size.value ((size) rect.border_size.set(*size));
        eval border_color.value ((color) rect.border_color.set(color::Rgba::from(color).into()));

        let rect_on_focus_in = rect.on_event::<FocusIn>();
        let rect_on_focus_out = rect.on_event::<FocusOut>();

        border_size_on_focus_in <- rect_on_focus_in.constant(BORDER_SIZE);
        border_size_on_focus_out <- rect_on_focus_out.constant(0.0);
        new_border_size <- any(&border_size_on_focus_in, &border_size_on_focus_out);
        border_size.target <+ new_border_size;

        new_color <- rect_on_focus_in.map (f!([rect] (e) {
            let is_target = e.target().as_ref() == Some(rect.display_object());
            if is_target {
                color::Lcha::from(color::Rgba::new(0.878, 0.25, 0.25, 1.0))
            } else {
                color::Lcha::from(color::Rgba::new(0.247, 0.407, 0.808, 1.0))
            }
        }));
        border_color.target <+ new_color;
    }
    rect
}

fn define_stack(network: &frp::Network) -> rectangle::View {
    let h0 = define_rect(RECT_SIZE, RECT_SIZE, network);
    let h1 = define_rect(RECT_SIZE - RECT_DIFF, RECT_SIZE - RECT_DIFF, network);
    let h2 = define_rect(RECT_SIZE - 2.0 * RECT_DIFF, RECT_SIZE - 2.0 * RECT_DIFF, network);
    h0.add_child(&h1);
    h1.add_child(&h2);
    h0
}

/// The example entry point.
#[entry_point]
#[allow(dead_code)]
pub fn main() {
    let world = World::new().displayed_in("root");
    let scene = &world.default_scene;
    let camera = scene.camera().clone_ref();
    let navigator = Navigator::new(scene, &camera);
    let network = &scene.frp.network;

    let container_size = RECT_SIZE + RECT_DIFF;
    let container = define_rect(container_size * 2.0, container_size, network);
    let left_stack = define_stack(network);
    let right_stack = define_stack(network);
    left_stack.update_x(|x| x - (container_size) / 2.0);
    right_stack.update_x(|x| x + (container_size) / 2.0);

    let rect = rectangle2::View::new();
    rect.set_size((2.0, 2.0));
    world.add_child(&rect);
    mem::forget(rect);

    world.add_child(&container);
    container.add_child(&left_stack);
    container.add_child(&right_stack);
    world.keep_alive_forever();
    mem::forget(navigator);
}
