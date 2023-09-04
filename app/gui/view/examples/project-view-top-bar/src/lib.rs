//! This is a debug scene for project view top bar component.

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

use ensogl::application::Application;
use ensogl::display::navigation::navigator::Navigator;
use ensogl_text_msdf::run_once_initialized;
use ide_view_execution_environment_selector as execution_environment_selector;
use ide_view_project_view_top_bar as project_view_top_bar;



// ======================
// === Initialisation ===
// ======================


fn init(app: &Application) {
    let app = app.clone_ref();
    let world = &app.display;
    let scene = &world.default_scene;
    let camera = scene.layers.panel.camera().clone_ref();
    let navigator = Navigator::new(scene, &camera);
    navigator.enable();

    let top_bar = project_view_top_bar::ProjectViewTopBar::new(&app);

    world.add_child(&top_bar);

    top_bar.project_name_with_environment_selector.selector.set_available_execution_environments(
        execution_environment_selector::make_dummy_execution_environments(),
    );

    mem::forget(top_bar);
    mem::forget(navigator);
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
