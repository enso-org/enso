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

use ensogl_core::prelude::*;
use wasm_bindgen::prelude::*;

use enso_text::index::Byte;
use ensogl_core::application::Application;
use ensogl_core::display::object::ObjectOps;
use ensogl_hardcoded_theme as theme;
use ensogl_list_view as list_view;
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



// ====================
// === Mock Entries ===
// ====================

#[derive(Clone, Debug)]
struct MockEntries {
    entries_count: usize,
}

impl MockEntries {
    fn new(entries_count: usize) -> Self {
        Self { entries_count }
    }
}

impl list_view::entry::ModelProvider<list_view::entry::GlyphHighlightedLabel> for MockEntries {
    fn entry_count(&self) -> usize {
        self.entries_count
    }

    fn get(&self, id: usize) -> Option<list_view::entry::GlyphHighlightedLabelModel> {
        if id >= self.entries_count {
            None
        } else {
            let label = iformat!("Entry {id}");
            let highlighted = if id == 10 { vec![(Byte(1)..Byte(3)).into()] } else { vec![] };
            Some(list_view::entry::GlyphHighlightedLabelModel { label, highlighted })
        }
    }
}



// ========================
// === Init Application ===
// ========================

fn init(app: &Application) {
    theme::builtin::dark::register(app);
    theme::builtin::light::register(app);
    theme::builtin::light::enable(app);

    let list_view = app.new_view::<list_view::ListView<list_view::entry::GlyphHighlightedLabel>>();
    let provider = list_view::entry::AnyModelProvider::new(MockEntries::new(1000));
    list_view.frp.resize(Vector2(100.0, 160.0));
    list_view.frp.set_entries(provider);
    list_view.focus();
    app.display.add_child(&list_view);
    // FIXME[WD]: This should not be needed after text gets proper depth-handling.
    app.display.default_scene.layers.below_main.add_exclusive(&list_view);

    let network = enso_frp::Network::new("test");
    enso_frp::extend! {network
        eval list_view.chosen_entry([](entry) {
            info!("Chosen entry {entry:?}")
        });
    }

    std::mem::forget(list_view);
    std::mem::forget(network);
}
