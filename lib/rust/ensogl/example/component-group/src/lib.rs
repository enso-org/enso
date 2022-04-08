//! A debug scene which shows the Component Group View.

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

use ensogl_component_group_view as component_group_view;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display::object::ObjectOps;
use ensogl_hardcoded_theme as theme;
use ensogl_list_view as list_view;
use ensogl_text_msdf_sys::run_once_initialized;



// ===================
// === Entry Point ===
// ===================

/// An entry point.
#[wasm_bindgen]
pub fn entry_point_component_group() {
    run_once_initialized(|| {
        let app = Application::new("root");
        init(&app);
        mem::forget(app);
    });
}



// ====================
// === Mock Entries ===
// ====================

#[derive(Clone, Debug)]
struct MockEntries {
    entries: Vec<String>,
}

impl MockEntries {
    fn new(entries: Vec<String>) -> Self {
        Self { entries }
    }

    fn get_entry(&self, i: usize) -> Option<String> {
        self.entries.get(i).map(|s| s.clone())
    }
}

impl list_view::entry::ModelProvider<list_view::entry::Label> for MockEntries {
    fn entry_count(&self) -> usize {
        self.entries.len()
    }

    fn get(&self, id: usize) -> Option<String> {
        self.get_entry(id)
    }
}



// ========================
// === Init Application ===
// ========================

fn init(app: &Application) {
    theme::builtin::dark::register(&app);
    theme::builtin::light::register(&app);
    theme::builtin::light::enable(&app);

    let mock_entries = MockEntries::new(vec![
        "sample entry with long text overflowing the width".into(),
        "convert".into(),
        "table input".into(),
        "text input".into(),
        "number input".into(),
        "table input".into(),
        "data output".into(),
        "data input".into(),
    ]);


    let component_group_view = app.new_view::<component_group_view::View>();
    let provider = list_view::entry::AnyModelProvider::new(mock_entries);
    let group_name = "Group name with long text overflowing the width";
    component_group_view.set_header_text(group_name.to_string());
    component_group_view.set_entries(provider);
    component_group_view.set_size(Vector2(150.0, 200.0));
    component_group_view.set_background_color(color::Rgba(0.0, 1.0, 0.0, 1.0));
    app.display.add_child(&component_group_view);

    std::mem::forget(component_group_view);
}
