//! This is a visualization example scene which creates a sinusoidal graph.

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

use ensogl::prelude::*;

use ensogl::animation;
use ensogl::application::Application;
use ensogl_text_msdf::run_once_initialized;
use ide_view_execution_mode_selector as execution_mode_selector;



// ======================
// === Initialisation ===
// ======================

fn make_entries() -> execution_mode_selector::ExecutionModes {
    Rc::new(vec!["development".to_string(), "production".to_string()])
}

fn init(app: &Application) {
    let app = app.clone_ref();
    let world = &app.display;
    let _scene = &world.default_scene;

    let execution_mode_selector = execution_mode_selector::ExecutionModeSelector::new(&app);
    world.add_child(&execution_mode_selector);
    execution_mode_selector.set_available_execution_modes(make_entries());

    world
        .on
        .before_frame
        .add(move |_time_info: animation::TimeInfo| {
            let _keep_alive = &execution_mode_selector;
        })
        .forget();
}


// ===================
// === Entry Point ===
// ===================

/// Entry point for the demo scene.
#[entry_point]
#[allow(dead_code)]
pub fn main() {
    run_once_initialized(|| {
        let app = Application::new("root");
        init(&app);
        mem::forget(app);
    });
}
