//! Example scene showing simple usage of a shape system.

// === Standard Linter Configuration ===
#![warn(unsafe_code)]

use ensogl_core::prelude::*;
use wasm_bindgen::prelude::*;

use component_group::icon;
use ensogl_core::application::Application;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;
use ensogl_hardcoded_theme as theme;
use ensogl_list_view as list_view;
use ensogl_list_view::entry::GlyphHighlightedLabelModel;
use ide_view_component_group as component_group;
use list_view::entry::AnyModelProvider;
use searcher_list_panel::ComponentBrowserPanel;
use searcher_list_panel::LabeledAnyModelProvider;


// ====================
// === Mock Entries ===
// ====================

const PREPARED_ITEMS: &[(&str, icon::Id)] = &[
    ("long sample entry with text overflowing the width", icon::Id::Star),
    ("convert", icon::Id::Convert),
    ("table input", icon::Id::DataInput),
    ("text input", icon::Id::TextInput),
    ("number input", icon::Id::NumberInput),
    ("table output", icon::Id::TableEdit),
    ("dataframe clean", icon::Id::DataframeClean),
    ("data input", icon::Id::DataInput),
];

#[derive(Debug)]
struct MockEntries {
    entries: Vec<component_group::entry::Model>,
    count:   Cell<usize>,
}

impl MockEntries {
    fn new(count: usize) -> Rc<Self> {
        Rc::new(Self {
            entries: PREPARED_ITEMS
                .iter()
                .cycle()
                .take(count)
                .map(|&(label, icon)| component_group::entry::Model {
                    icon,
                    highlighted_text: GlyphHighlightedLabelModel {
                        label:       label.to_owned(),
                        highlighted: default(),
                    },
                })
                .collect(),
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
    ensogl_text_msdf_sys::run_once_initialized(|| {
        let app = &Application::new("root");

        theme::builtin::light::register(&app);
        theme::builtin::light::enable(&app);

        let world = &app.display;
        let scene = &world.default_scene;
        let camera = scene.camera().clone_ref();
        let navigator = Navigator::new(scene, &camera);
        navigator.disable_wheel_panning();

        let searcher_list_panel = ComponentBrowserPanel::new(app);

        let mock_entries = MockEntries::new(20);
        let model_provider = AnyModelProvider::from(mock_entries.clone_ref());
        searcher_list_panel.set_local_scope_section(model_provider.clone_ref());

        let local_scope_data = vec![
            MockEntries::new(5),
            MockEntries::new(3),
            MockEntries::new(2),
            MockEntries::new(4),
            MockEntries::new(2),
            MockEntries::new(5),
        ];
        let local_scope_data = local_scope_data
            .into_iter()
            .map(|mock_entries| LabeledAnyModelProvider {
                content: AnyModelProvider::from(mock_entries.clone_ref()),
                label:   "Header".into(),
            })
            .collect_vec();
        searcher_list_panel.set_favourites_section(local_scope_data);

        let sub_module_data = vec![
            MockEntries::new(4),
            MockEntries::new(6),
            MockEntries::new(8),
            MockEntries::new(6),
            MockEntries::new(4),
            MockEntries::new(4),
        ];
        let sub_module_data = sub_module_data
            .into_iter()
            .map(|mock_entries| LabeledAnyModelProvider {
                content: AnyModelProvider::from(mock_entries.clone_ref()),
                label:   "Header".into(),
            })
            .collect_vec();
        searcher_list_panel.set_sub_modules_section(sub_module_data);

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
    })
}
