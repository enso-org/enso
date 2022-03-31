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

use enso_text::unit::Bytes;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::display::shape::PixelDistance;
use ensogl_core::display::shape::Rect;
use ensogl_core::display::shape::ShapeOps;
use ensogl_core::display::shape::ShapeSystem;
use ensogl_core::display::Sprite;
use ensogl_component_group_view as component_group_view;
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
    entries_count: usize,
}

impl MockEntries {
    fn new(entries_count: usize) -> Self {
        Self { entries_count }
    }
}

impl list_view::entry::ModelProvider<list_view::entry::Label> for MockEntries {
    fn entry_count(&self) -> usize {
        self.entries_count
    }

    fn get(&self, id: usize) -> Option<String> {
        if id >= self.entries_count {
            None
        } else {
            let label = iformat!("Entry {id}");
            // // TODO[AO]: The highlight below should use SDF-based bold mechanism
            // // (see: https://www.pivotaltracker.com/story/show/181641027)
            // let highlighted = if id == 5 { vec![(Bytes(1)..Bytes(3)).into()] } else { vec![] };
            // Some(list_view::entry::GlyphHighlightedLabelModel { label, highlighted })
            Some(label)
        }
    }
}



// ========================
// === Init Application ===
// ========================

fn init(app: &Application) {
    theme::builtin::dark::register(&app);
    theme::builtin::light::register(&app);
    theme::builtin::light::enable(&app);

    let scene = &app.display.default_scene;
    // scene.camera().set_position_xy(Vector2(100.0, -100.0));


    // === Background ===

    // let background_color = color::Rgba::new(0.9, 0.9, 0.5, 1.0);
    // let background_size = (200.px(), 200.px());
    // let background_shape = Rect(background_size).corners_radius(5.5.px()).fill(background_color);
    // let background_system = ShapeSystem::new(scene, background_shape);
    // let background: Sprite = background_system.new_instance();
    // scene.add_child(&background_system);
    // background.size.set(Vector2::new(200.0, 200.0));
    // background.set_position_x(100.0);
    // background.set_position_y(-100.0);
    // std::mem::forget(background);


    // === Component Group View ===

    let component_group_view = app.new_view::<component_group_view::View>();
    let provider = list_view::entry::AnyModelProvider::new(MockEntries::new(1000));
    component_group_view.resize(Vector2(100.0, 160.0));
    component_group_view.set_entries(provider);
    // component_group_view.set_header_text("Input / Output very long name".to_string());
    component_group_view.set_header_text("Input / Output".to_string());
    app.display.add_child(&component_group_view);

    std::mem::forget(component_group_view);

    // === List View ===

    // let list_view = app.new_view::<list_view::ListView<list_view::entry::GlyphHighlightedLabel>>();
    // let provider = list_view::entry::AnyModelProvider::new(MockEntries::new(1000));
    // list_view.frp.resize(Vector2(100.0, 160.0));
    // list_view.frp.set_entries(provider);
    // app.display.add_child(&list_view);
    // // list_view.set_position_x(100.0);
    // // list_view.set_position_y(-100.0);
    // // FIXME[WD]: This should not be needed after text gets proper depth-handling.
    // // app.display.default_scene.layers.below_main.add_exclusive(&list_view);

    // let logger: Logger = Logger::new("SelectDebugScene");
    // let network = enso_frp::Network::new("test");
    // enso_frp::extend! {network
    //     eval list_view.chosen_entry([logger](entry) {
    //         info!(logger, "Chosen entry {entry:?}")
    //     });
    // }

    // std::mem::forget(list_view);
}
