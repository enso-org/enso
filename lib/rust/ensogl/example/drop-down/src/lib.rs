//! A debug scene which shows the Dynamic drop-down component.

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

mod dropdown;

use ensogl_core::prelude::*;
use wasm_bindgen::prelude::*;

use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;
use ensogl_hardcoded_theme as theme;
use ensogl_text_msdf::run_once_initialized;



// ===================
// === Entry Point ===
// ===================

/// An entry point.
#[entry_point]
#[allow(dead_code)]
pub fn main() {
    run_once_initialized(|| {
        let app = Application::new("root");
        init(&app);
        mem::forget(app);
    });
}



// ========================
// === Init Application ===
// ========================

fn init(app: &Application) {
    theme::builtin::dark::register(app);
    theme::builtin::light::register(app);
    theme::builtin::light::enable(app);

    let world = &app.display;
    let scene = &world.default_scene;

    app.views.register::<dropdown::Dropdown>();
    let dropdown = setup_dropdown(app);
    world.add_child(&dropdown);

    dropdown.request_model_for_visible_entries();

    let navigator = Navigator::new(scene, &scene.camera());
    navigator.disable_wheel_panning();

    std::mem::forget((dropdown, navigator));
}


fn model_for_entry(row: usize) -> dropdown::EntryModel {
    dropdown::EntryModel::new(format!("Row: {row}"))
}

fn setup_dropdown(app: &Application) -> dropdown::Dropdown {
    let dropdown = app.new_view::<dropdown::Dropdown>();
    dropdown.set_position_xy(Vector2(0.0, 0.0));
    dropdown.set_number_of_entries(20);
    dropdown.set_color(color::Lcha::olive(0.5, 0.5));

    frp::new_network! { network
        dropdown.model_for_entry <+ dropdown.model_for_entry_needed.map(|row| (*row, model_for_entry(*row)));
    }

    std::mem::forget(network);
    dropdown
}
