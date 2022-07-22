//! A debug scene which shows the Scrollable Grid View component.

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

use enso_frp as frp;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;
use ensogl_grid_view as grid_view;
use ensogl_grid_view::Viewport;
use ensogl_hardcoded_theme as theme;
use ensogl_text_msdf_sys::run_once_initialized;



// ===================
// === Entry Point ===
// ===================

/// An entry point.
#[entry_point]
#[allow(dead_code)]
pub fn main() {
    run_once_initialized(|| {
        init_tracing(TRACE);
        let app = Application::new("root");
        init(&app);
        mem::forget(app);
    });
}



// ========================
// === Init Application ===
// ========================

fn init(app: &Application) {
    theme::builtin::dark::register(&app);
    theme::builtin::light::register(&app);
    theme::builtin::light::enable(&app);

    let grid_view = grid_view::simple::SimpleSelectableGridView::new(app);
    app.display.default_scene.layers.node_searcher.add_exclusive(&grid_view);
    let text_layer = app.display.default_scene.layers.node_searcher.create_sublayer();
    let selection_layer = app.display.default_scene.layers.node_searcher.create_sublayer();
    grid_view.set_text_layer(Some(text_layer.downgrade()));
    grid_view.selection_highlight_frp().setup_masked_layer(Some(selection_layer.downgrade()));
    frp::new_network! { network
        requested_entry <- grid_view.model_for_entry_needed.map(|(row, col)| {
            let model = grid_view::simple::EntryModel {
                text:     format!("Entry ({row}, {col})").into(),
                disabled: Immutable(row == col),
            };
            (*row, *col, model)
        });
        grid_view.model_for_entry <+ requested_entry;
        entry_hovered <- grid_view.entry_hovered.filter_map(|l| *l);
        entry_selected <- grid_view.entry_selected.filter_map(|l| *l);
        eval entry_hovered ([]((row, col)) tracing::debug!("Hovered entry ({row}, {col})."));
        eval entry_selected ([]((row, col)) tracing::debug!("Selected entry ({row}, {col})."));
        eval grid_view.entry_accepted ([]((row, col)) tracing::debug!("ACCEPTED entry ({row}, {col})."));
    }
    grid_view.set_entries_size(Vector2(130.0, 28.0));
    let params = grid_view::simple::EntryParams {
        bg_color: color::Rgba(0.8, 0.8, 0.9, 1.0),
        bg_margin: 1.0,
        ..default()
    };
    let selected_params = grid_view::simple::EntryParams {
        bg_color: color::Rgba(0.3, 0.3, 0.3, 1.0),
        text_color: color::Rgba::white(),
        ..params.clone()
    };
    grid_view.set_entries_params(params);
    grid_view.selection_highlight_frp().set_entries_params(selected_params);
    grid_view.reset_entries(1000, 1000);
    // grid_view.scroll_frp().resize(Vector2(400.0, 300.0));
    grid_view.set_viewport(Viewport { left: 0.0, right: 400.0, top: 0.0, bottom: -400.0 });

    app.display.add_child(&grid_view);
    let navigator = Navigator::new(
        &app.display.default_scene,
        &app.display.default_scene.layers.node_searcher.camera(),
    );
    navigator.disable_wheel_panning();

    std::mem::forget(grid_view);
    std::mem::forget(text_layer);
    std::mem::forget(selection_layer);
    std::mem::forget(network);
    std::mem::forget(navigator);
}
