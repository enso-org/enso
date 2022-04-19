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

use enso_frp as frp;
use ensogl_core::application::Application;
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

const PREPARED_ITEMS: &[&str; 8] = &[
    "long sample entry with text overflowing the width",
    "convert",
    "table input",
    "text input",
    "number input",
    "table output",
    "data output",
    "data input",
];

#[derive(Debug)]
struct MockEntries {
    entries: Vec<String>,
    count:   Cell<usize>,
}

impl MockEntries {
    fn new(count: usize) -> Rc<Self> {
        Rc::new(Self {
            entries: PREPARED_ITEMS.iter().cycle().take(count).map(ToString::to_string).collect(),
            count:   Cell::new(count),
        })
    }

    fn set_count(&self, count: usize) {
        if self.entries.len() >= count {
            self.count.set(count);
        }
    }

    fn get_entry(&self, id: list_view::entry::Id) -> Option<String> {
        self.entries.get(id).cloned()
    }
}

impl list_view::entry::ModelProvider<list_view::entry::Label> for MockEntries {
    fn entry_count(&self) -> usize {
        self.count.get()
    }

    fn get(&self, id: list_view::entry::Id) -> Option<String> {
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

    let slider = app.new_view::<selector::NumberPicker>();
    app.display.add_child(&slider);
    slider.frp.resize(Vector2(400.0, 50.0));
    slider.frp.allow_click_selection(true);
    slider.frp.set_bounds(Bounds::new(0.0, 15.0));
    slider.set_position_y(250.0);
    slider.frp.set_caption(Some("Items count:".to_string()));

    let component_group = app.new_view::<component_group::View>();
    let group_name = "Long group name with text overflowing the width";
    component_group.set_header(group_name.to_string());
    component_group.set_size(Vector2(150.0, 400.0));
    component_group.set_position_x(-300.0);
    component_group.set_background_color(color::Rgba(0.927, 0.937, 0.913, 1.0));
    app.display.add_child(&component_group);

    let wide_component_group = app.new_view::<component_group::wide_component_group::View>();
    wide_component_group.set_position_x(100.0);
    wide_component_group.set_width(450.0);
    wide_component_group.set_background_color(color::Rgba(0.927, 0.937, 0.913, 1.0));
    app.display.add_child(&wide_component_group);

    let mock_entries = MockEntries::new(25);
    let model_provider = AnyModelProvider::from(mock_entries.clone_ref());
    let network = frp::Network::new("ComponentGroupDemo");
    frp::extend! { network
        int_value <- slider.frp.output.value.map(|v| *v as usize);
        eval int_value((i) mock_entries.set_count(*i));
        provider <- int_value.map(move |_| model_provider.clone_ref());
        component_group.set_entries <+ provider;
        wide_component_group.set_entries <+ provider;
    }
    slider.frp.set_value(5.0);

    std::mem::forget(slider);
    std::mem::forget(network);
    std::mem::forget(component_group);
    std::mem::forget(wide_component_group);
}
