//! Example scene showing simple shape component that logs all its mouse events.
//! A partially-transparent shape partially covering it is transparent to mouse events.

#![recursion_limit = "1024"]
// === Features ===
#![feature(associated_type_defaults)]
#![feature(drain_filter)]
#![feature(fn_traits)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(unboxed_closures)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use enso_frp as frp;
use ensogl_core::application;
use ensogl_core::application::Application;
use ensogl_core::data::color::Rgb;
use ensogl_core::data::color::Rgba;
use ensogl_core::display;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;
use ensogl_text_msdf::run_once_initialized;



// =============
// === Model ===
// =============

#[derive(Clone, CloneRef, Debug, display::Object)]
struct Model {
    display_object: display::object::Instance,
    shape:          Rectangle,
    cover:          Rectangle,
}

impl Model {
    fn new() -> Self {
        let display_object = display::object::Instance::new();
        let shape: Rectangle = default();
        shape.set_size(Vector2(300.0, 300.0));
        shape.set_color(Rgb(1.0, 0.0, 0.0).into());
        shape.set_corner_radius_max();
        display_object.add_child(&shape);
        let cover: Rectangle = default();
        // We need a value that will make the covering shape a bit smaller than the main shape.
        // Euclid found a good one.
        const INVERSE_PHI: f32 = 0.618_033;
        cover.set_size(Vector2(300.0 * INVERSE_PHI, 300.0 * INVERSE_PHI));
        cover.set_color(Rgba(0.0, 0.0, 0.0, 0.5));
        cover.set_corner_radius_max();
        cover.set_pointer_events(false);
        display_object.add_child(&cover);
        Self { display_object, shape, cover }
    }
}



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints! { [TRACE_ALL]
    Input {
    }
    Output {
    }
}



// ============
// === View ===
// ============

#[derive(Clone, CloneRef, Debug, Deref, display::Object)]
struct View {
    #[deref]
    frp:   Frp,
    #[display_object]
    model: Model,
}

impl View {
    /// Constructor.
    pub fn new() -> Self {
        let frp = Frp::new();
        let model = Model::new();
        let network = &frp.network;
        frp::extend! { network
            trace model.shape.events_deprecated.mouse_up;
            trace model.shape.events_deprecated.mouse_release;
            trace model.shape.events_deprecated.mouse_down;
            trace model.shape.events_deprecated.mouse_over;
            trace model.shape.events_deprecated.mouse_out;
            trace model.shape.events_deprecated.on_drop;
        }

        Self { frp, model }
    }
}

impl FrpNetworkProvider for View {
    fn network(&self) -> &frp::Network {
        &self.frp.network
    }
}

impl application::View for View {
    fn label() -> &'static str {
        "Circul"
    }

    fn new(_app: &Application) -> Self {
        View::new()
    }
}


// ===================
// === Entry Point ===
// ===================

/// The example entry point.
#[entry_point]
#[allow(dead_code)]
pub fn main() {
    run_once_initialized(|| {
        let app = Application::new("root");
        let shape: View = app.new_view();
        app.display.add_child(&shape);

        let scene = &app.display.default_scene;
        let camera = scene.camera().clone_ref();
        let navigator = Navigator::new(scene, &camera);

        std::mem::forget(shape);
        std::mem::forget(navigator);
        mem::forget(app);
    });
}
