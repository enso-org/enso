//! Example scene showing simple usage of a shape system.

#![recursion_limit = "512"]
// === Features ===
#![allow(incomplete_features)]
#![feature(negative_impls)]
#![feature(associated_type_defaults)]
#![feature(bool_to_option)]
#![feature(cell_update)]
#![feature(const_type_id)]
#![feature(drain_filter)]
#![feature(entry_insert)]
#![feature(fn_traits)]
#![feature(marker_trait_attr)]
#![feature(specialization)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(unboxed_closures)]
#![feature(trace_macros)]
#![feature(const_trait_impl)]
#![feature(slice_as_chunks)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

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
use js_sys::Math;
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



// ===============================
// === Initialisation Helpers ===
// ===============================

fn mock_data() -> Vec<LabeledAnyModelProvider> {
    // Items with random length but somewhat controlled distribution to get shorter and longer
    // entries.
    let random_entry = |n: usize| MockEntries::new(n + (Math::random() * 5.0) as usize);
    vec![
        // Three empty lists to check they are filtered correctly.
        MockEntries::new(0),
        MockEntries::new(0),
        MockEntries::new(0),
        random_entry(1),
        random_entry(1),
        random_entry(3),
        random_entry(3),
        random_entry(6),
        random_entry(6),
    ]
    .into_iter()
    .map(|mock_entries| LabeledAnyModelProvider {
        content: AnyModelProvider::from(mock_entries.clone_ref()),
        label:   "Header".into(),
    })
    .collect_vec()
}

fn init_sub_modules_section(searcher_list_panel: &ComponentBrowserPanel) {
    let sub_module_data = mock_data();
    searcher_list_panel.set_sub_modules_section(sub_module_data);
    // Doing this twice to reveal potential issues with setting new data.
    let sub_module_data = mock_data();
    searcher_list_panel.set_sub_modules_section(sub_module_data);
}

fn init_favourites_section(searcher_list_panel: &ComponentBrowserPanel) {
    let local_scope_data = mock_data();
    searcher_list_panel.set_favourites_section(local_scope_data);
    // Doing this twice to reveal potential issues with setting new data.
    let local_scope_data = mock_data();
    searcher_list_panel.set_favourites_section(local_scope_data);
}

fn init_local_cope_section(searcher_list_panel: &ComponentBrowserPanel) {
    let mock_entries = MockEntries::new(20);
    let model_provider = AnyModelProvider::from(mock_entries.clone_ref());
    searcher_list_panel.set_local_scope_section(model_provider.clone_ref());
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

        let searcher_list_panel = ComponentBrowserPanel::new(app);
        searcher_list_panel.model().set_navigator(Some(navigator.clone()));

        init_favourites_section(&searcher_list_panel);
        init_local_cope_section(&searcher_list_panel);
        init_sub_modules_section(&searcher_list_panel);

        world.add_child(&searcher_list_panel);
        world.keep_alive_forever();

        world
            .on
            .before_frame
            .add(move |_time| {
                let _keep_alive = &searcher_list_panel;
                let _keep_alive = &navigator;
            })
            .forget();
    })
}
