//! A debug scene which shows the Component Group visual component.

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

use ensogl_core::application::Application;
use enso_frp as frp;
use ensogl_core::data::color;
use ensogl_core::display::object::ObjectOps;
use ensogl_hardcoded_theme as theme;
use ensogl_list_view as list_view;
use ensogl_selector as selector;
use ensogl_selector::Bounds;
use ensogl_text_msdf_sys::run_once_initialized;
use ide_view_component_group as component_group;
use list_view::entry::AnyModelProvider;



// ===================
// === Entry Point ===
// ===================

/// An entry point.
#[entry_point]
pub fn main() {
    run_once_initialized(|| {
        let app = Application::new("root");
        init(&app);
        mem::forget(app);
    });
}



// ====================
// === Mock Entries ===
// ====================

#[derive(Clone, Debug, Default)]
struct MockEntries {
    entries: Vec<String>,
}

impl MockEntries {
    fn new(entries: Vec<String>) -> Self {
        Self { entries }
    }

    fn get_entry(&self, i: usize) -> Option<String> {
        self.entries.get(i).cloned()
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

const PREPARED_ITEMS: &[&'static str; 8] = &[
    "long sample entry with text overflowing the width",
    "convert",
    "table input",
    "text input",
    "number input",
    "table output",
    "data output",
    "data input",
];

fn mock_entries(count: usize) -> MockEntries {
    let items = PREPARED_ITEMS.iter().cycle().take(count).map(ToString::to_string).collect(); 
    MockEntries::new(items)
}



// ========================
// === Init Application ===
// ========================

fn init(app: &Application) {
    theme::builtin::dark::register(&app);
    theme::builtin::light::register(&app);
    theme::builtin::light::enable(&app);

    let slider = app.new_view::<selector::NumberPicker>();
    app.display.add_child(&slider);
    slider.frp.resize(Vector2(400.0, 50.0));
    slider.frp.allow_click_selection(false);
    slider.frp.set_bounds(Bounds::new(0.0, 10.0));
    slider.set_position_y(250.0);
    slider.frp.set_caption(Some("Items count:".to_string()));
    slider.frp.set_value(5.0);

    let component_group = app.new_view::<component_group::View>();
    let group_name = "Long group name with text overflowing the width";
    component_group.set_header(group_name.to_string());
    component_group.set_size(Vector2(150.0, 200.0));
    component_group.set_background_color(color::Rgba(0.927, 0.937, 0.913, 1.0));
    app.display.add_child(&component_group);

    let network = frp::Network::new("ComponentGroupDemo");
    frp::extend! { network
        int_value <- slider.frp.output.value.map(|v| *v as usize);
        entries <- int_value.map(|count| AnyModelProvider::new(mock_entries(*count)));
        component_group.set_entries <+ entries;
    }

    std::mem::forget(slider);
    std::mem::forget(component_group);
    std::mem::forget(network);
}
