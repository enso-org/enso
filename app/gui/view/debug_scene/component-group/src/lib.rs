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
use ensogl_core::data::color;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::frp;
use ensogl_core::Animation;
use ensogl_hardcoded_theme as theme;
use ensogl_list_view as list_view;
use ensogl_text_msdf_sys::run_once_initialized;
use ide_view_component_group as component_group;



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

#[derive(Clone, Debug)]
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



// ========================
// === Init Application ===
// ========================

fn init(app: &Application) {
    theme::builtin::dark::register(&app);
    theme::builtin::light::register(&app);
    theme::builtin::light::enable(&app);

    let mock_entries = MockEntries::new(vec![
        "long sample entry with text overflowing the width".into(),
        "convert".into(),
        "table input".into(),
        "text input".into(),
        "number input".into(),
        "table input".into(),
        "data output".into(),
        "data input".into(),
    ]);

    let network = frp::Network::new("Component Group Debug Scene");
    let selection = list_view::selection::View::new(Logger::new("Selection"));
    selection.color.set(color::Rgba(0.527, 0.554, 0.18, 1.0).into());
    selection.size.set(Vector2(150.0, list_view::entry::HEIGHT));
    selection.corner_radius.set(5.0);
    let selection_animation = Animation::<Vector2>::new(&network);
    let component_group = app.new_view::<component_group::View>();
    let provider = list_view::entry::AnyModelProvider::new(mock_entries);
    let group_name = "Long group name with text overflowing the width";
    component_group.set_header(group_name.to_string());
    component_group.set_entries(provider);
    component_group.set_width(150.0);
    component_group.set_background_color(color::Rgba(0.927, 0.937, 0.913, 1.0));
    app.display.add_child(&component_group);
    app.display.add_child(&selection);

    frp::extend! { network
        selection_animation.target <+ component_group.selection_position_target;
        eval selection_animation.value ((pos) selection.set_position_xy(*pos));

        eval component_group.suggestion_accepted ([](id) DEBUG!("Accepted Suggestion {id}"));
        eval component_group.expression_accepted ([](id) DEBUG!("Accepted Expression {id}"));
        eval_ component_group.header_accepted ([] DEBUG!("Accepted Header"));
    }
    selection_animation.target.emit(component_group.selection_position_target.value());
    selection_animation.skip.emit(());

    std::mem::forget(network);
    std::mem::forget(selection);
    std::mem::forget(component_group);
}
