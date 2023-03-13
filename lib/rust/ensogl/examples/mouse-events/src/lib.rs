//! Example scene showing simple shape component that logs all its mouse events.

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
use ensogl_core::display;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::shape;
use ensogl_text_msdf::run_once_initialized;



// ==============
// === Shapes ===
// ==============

mod shape {
    use super::*;

    shape! {
        (style: Style) {
            Circle(100.px()).fill(color::Rgb(1.0,0.0,0.0)).into()
        }
    }
}



// =============
// === Model ===
// =============

#[derive(Clone, CloneRef, Debug)]
struct Model {
    app:            Application,
    display_object: display::object::Instance,
    shape:          shape::View,
}

impl Model {
    fn new(app: &Application) -> Self {
        let app = app.clone_ref();
        let display_object = display::object::Instance::new();
        let shape = shape::View::new();
        shape.set_size(Vector2::new(100.0, 100.0));
        display_object.add_child(&shape);
        Self { app, display_object, shape }
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

#[derive(Clone, CloneRef, Debug)]
struct View {
    frp:   Frp,
    model: Model,
}

impl View {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let model = Model::new(app);
        let network = &frp.network;
        frp::extend! { network
            trace model.shape.events.mouse_up;
            trace model.shape.events.mouse_release;
            trace model.shape.events.mouse_down;
            trace model.shape.events.mouse_over;
            trace model.shape.events.mouse_out;
            trace model.shape.events.on_drop;
        }

        Self { frp, model }
    }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}

impl Deref for View {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
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
    fn new(app: &Application) -> Self {
        View::new(app)
    }
    fn app(&self) -> &Application {
        &self.model.app
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
        shape.model.shape.set_size((300.0, 300.0));
        app.display.add_child(&shape);

        let scene = &app.display.default_scene;
        let camera = scene.camera().clone_ref();
        let navigator = Navigator::new(scene, &camera);

        std::mem::forget(shape);
        std::mem::forget(navigator);
        mem::forget(app);
    });
}
