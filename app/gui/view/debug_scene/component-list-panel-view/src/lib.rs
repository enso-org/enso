//! Example scene showing simple usage of a shape system.

// === Standard Linter Configuration ===
#![warn(unsafe_code)]

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;
use wasm_bindgen::prelude::*;

use ensogl_core::application::Application;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;
use ensogl_hardcoded_theme as theme;
use ensogl_list_view as list_view;
use ide_view_component_group as component_group;
use list_view::entry::AnyModelProvider;
use searcher_list_panel::ComponentBrowserPanel;



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
    entries: Vec<component_group::entry::Model>,
    count:   Cell<usize>,
}

impl MockEntries {
    fn new(count: usize) -> Rc<Self> {
        Rc::new(Self {
            entries: PREPARED_ITEMS.iter().cycle().take(count).map(|&label| label.into()).collect(),
            count:   Cell::new(count),
        })
    }

    fn get_entry(&self, id: list_view::entry::Id) -> Option<component_group::entry::Model> {
        self.entries.get(id).cloned()
    }
}

impl list_view::entry::ModelProvider<component_group::Entry> for MockEntries {
    fn entry_count(&self) -> usize {
        self.count.get()
    }

    fn get(&self, id: list_view::entry::Id) -> Option<component_group::entry::Model> {
        self.get_entry(id)
    }
}



// ===================
// === Entry Point ===
// ===================

/// The example entry point.
#[entry_point]
#[allow(dead_code)]
pub fn main() {
    let app = &Application::new("root");

    theme::builtin::light::register(&app);
    theme::builtin::light::enable(&app);

    let world = &app.display;
    let scene = &world.default_scene;
    let camera = scene.camera().clone_ref();
    let navigator = Navigator::new(scene, &camera);

    let searcher_list_panel = ComponentBrowserPanel::new(app);

    let mock_entries = MockEntries::new(90);
    let model_provider = AnyModelProvider::from(mock_entries.clone_ref());
    searcher_list_panel.set_favourites_section(model_provider.clone_ref());

    world.add_child(&searcher_list_panel);
    world.keep_alive_forever();

    world
        .on
        .before_frame
        .add(move |_time| {
            let _keep_alive = &mock_entries;
            let _keep_alive = &model_provider;
            let _keep_alive = &searcher_list_panel;
            let _keep_alive = &navigator;
        })
        .forget();
}
