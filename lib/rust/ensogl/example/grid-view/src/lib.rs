//! A debug scene which shows the Select Component. The chosen entries are logged in console.

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
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use ensogl_core::prelude::*;
use wasm_bindgen::prelude::*;

use enso_frp as frp;
use enso_text::unit::Bytes;
use ensogl_core::application::Application;
use ensogl_core::display::object::ObjectOps;
use ensogl_grid_view as grid_view;
use ensogl_grid_view::entry;
use ensogl_hardcoded_theme as theme;
use ensogl_text_msdf_sys::run_once_initialized;
use logger::TraceLogger as Logger;



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
    theme::builtin::dark::register(&app);
    theme::builtin::light::register(&app);
    theme::builtin::light::enable(&app);
    
    let grid_view = ensogl_grid_view::GridView::<entry::Label>::new(app);
    frp::new_network! { network
        
    }
    let params = entry::LabelParams {
        font: ,
        size: Default::default(),
        color: Default::default()
    }

    grid_view.set_entries_size(Vector2(130.0, 28.0));



    std::mem::forget(list_view);
    std::mem::forget(network);
}
