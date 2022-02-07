//! A debug scene which shows the number and range selector.

#![feature(associated_type_defaults)]
#![feature(drain_filter)]
#![feature(entry_insert)]
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

use ensogl_core::application::Application;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::system::web;
use ensogl_hardcoded_theme as theme;
use ensogl_label as label;
use ensogl_text_msdf_sys::run_once_initialized;



// ===================
// === Entry Point ===
// ===================

/// An entry point.
#[wasm_bindgen]
#[allow(dead_code)]
pub fn entry_point_stats() {
    web::forward_panic_hook_to_console();
    web::set_stack_trace_limit();
    run_once_initialized(|| {
        let app = Application::new(&web::get_html_element_by_id("root").unwrap());
        init(&app);
        Leak::new(app);
    });
}



// ========================
// === Init Application ===
// ========================

fn init(app: &Application) {
    theme::builtin::dark::register(&app);
    theme::builtin::light::register(&app);
    theme::builtin::light::enable(&app);

    let label = label::Label::new(app);
    app.display.add_child(&label);
    let label = Leak::new(label);
    label.inner().frp.set_content("Hello stats");
}

