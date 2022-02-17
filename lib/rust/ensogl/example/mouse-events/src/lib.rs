//! Example scene showing simple shape component that logs all its mouse events.

#![feature(associated_type_defaults)]
#![feature(drain_filter)]
#![feature(fn_traits)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(unboxed_closures)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![recursion_limit = "1024"]

use ensogl_core::prelude::*;
use wasm_bindgen::prelude::*;

use ensogl_core::application;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::define_shape_system;
use ensogl_core::display;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::display::shape::*;
use ensogl_core::system::web;

use enso_frp as frp;
use ensogl_text_msdf_sys::run_once_initialized;

// ==============
// === Shapes ===
// ==============

mod shape {
    use super::*;

    define_shape_system! {
        () {
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
    logger:         DefaultTraceLogger,
    display_object: display::object::Instance,
    shape:          shape::View,
}

impl Model {
    fn new(app: &Application) -> Self {
        let app = app.clone_ref();
        let logger = DefaultTraceLogger::new("Button");
        let display_object = display::object::Instance::new(&logger);
        let shape = shape::View::new(&logger);
        shape.size.set(Vector2::new(100.0, 100.0));
        display_object.add_child(&shape);
        Self { app, logger, display_object, shape }
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
        let events = &model.shape.events;
        let network = &events.network;
        frp::extend! { network
            // FIXME [mwu] Currently only `mouse_over` and `mouse_out` events are delivered.
            //             See: https://github.com/enso-org/ide/issues/1477
            trace model.shape.events.mouse_up;
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

impl application::command::FrpNetworkProvider for View {
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
#[wasm_bindgen]
#[allow(dead_code)]
pub fn entry_point_mouse_events() {
    web::forward_panic_hook_to_console();
    run_once_initialized(|| {
        let app = Application::new(&web::get_html_element_by_id("root").unwrap());

        let shape: View = app.new_view();
        shape.model.shape.size.set(Vector2::new(300.0, 300.0));
        app.display.add_child(&shape);

        let scene = app.display.scene();
        let camera = scene.camera().clone_ref();
        let navigator = Navigator::new(scene, &camera);

        std::mem::forget(shape);
        std::mem::forget(navigator);
        mem::forget(app);
    });
}
