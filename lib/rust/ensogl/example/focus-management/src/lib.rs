//! Example scene showing simple usage of a shape system.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use ensogl_core::display::shape::*;
use ensogl_core::display::world::*;
use ensogl_core::prelude::*;
use wasm_bindgen::prelude::*;

use ensogl_core::data::color;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::display::scene;
use ensogl_core::display::style::theme;



// ==============
// === Shapes ===
// ==============

mod rectangle {
    use super::*;
    ensogl_core::shape! {
        (style: Style, color: Vector4<f32>) {
            let width = Var::<Pixels>::from("input_size.x");
            let height = Var::<Pixels>::from("input_size.y");
            let rect = Rect((width, height));
            let shape = rect.fill(color);
            shape.into()
        }
    }
}

mod circle {
    use super::*;
    ensogl_core::shape! {
        above = [rectangle];
        (style:Style, color: Vector4<f32>) {
            let radius = Var::<Pixels>::from("input_size.x") / 2.0;
            let shape = Circle(radius);
            let shape = shape.fill(color);
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
    let theme_manager = theme::Manager::from(&scene.style_sheet);

    let rectangle = rectangle::View::new();
    rectangle.size.set(Vector2::new(140.0, 140.0));

    let circle = circle::View::new();
    circle.size.set(Vector2(100.0, 100.0));
    circle.color.set(color::Rgba::new(1.0, 0.0, 0.0, 1.0).into());

    let network = &scene.frp.network;

    let dp = rectangle.display_object();
    frp::extend! { network
        eval circle.events.mouse_down ((t) circle.display_object().emit_event.emit(SomeEvent::new(t.clone())));

        e <- rectangle.display_object().on_event.filter_map(|t| t.downcast::<f32>());

        e <- rectangle.on::<f32>();
        // trace rectangle.display_object().on_event;
        trace e;
    }

    world.add_child(&rectangle);
    rectangle.add_child(&circle);

    world.keep_alive_forever();

    mem::forget(navigator);
    mem::forget(circle);
    mem::forget(rectangle);
}

use ensogl_core::display::object::Event;
use ensogl_core::display::object::SomeEvent;


pub trait NetworkEventHandler {
    fn on<T: frp::node::Data>(
        &self,
        label: &'static str,
        src: impl ensogl_core::display::Object,
    ) -> frp::Stream<Event<T>>;
}

impl NetworkEventHandler for frp::Network {
    fn on<T: frp::node::Data>(
        &self,
        label: &'static str,
        src: impl ensogl_core::display::Object,
    ) -> frp::Stream<Event<T>> {
        self.filter_map(label, &src.display_object().on_event, |t| t.downcast::<T>())
    }
}
