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
use ensogl_grid_view::Col;
use ensogl_grid_view::Row;
use ensogl_hardcoded_theme as theme;
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

fn entry_model(row: Row, col: Col) -> grid_view::simple::EntryModel {
    grid_view::simple::EntryModel {
        text:           format!("Entry ({row}, {col})").into(),
        disabled:       Immutable(row == col),
        override_width: Immutable(if col == 1 && row == 5 { Some(180.0) } else { None }),
    }
}

fn setup_grid_view(
    app: &Application,
) -> grid_view::simple::SimpleScrollableSelectableGridViewWithHeaders {
    let view = grid_view::simple::SimpleScrollableSelectableGridViewWithHeaders::new(app);
    let header_frp = view.header_frp();
    frp::new_network! { network
        requested_entry <-
            view.model_for_entry_needed.map(|&(row, col)| (row, col, entry_model(row, col)));
        requested_section <- header_frp.section_info_needed.map(|&(row, col)| {
            let sections_size = 2 + col;
            let section_start = row - (row % sections_size);
            let section_end = section_start + sections_size;
            let model = entry_model(section_start, col);
            (section_start..section_end, col, model)
        });
        view.model_for_entry <+ requested_entry;
        header_frp.section_info <+ requested_section;
        entry_hovered <- view.entry_hovered.filter_map(|l| *l);
        entry_selected <- view.entry_selected.filter_map(|l| *l);
        eval entry_hovered ([]((row, col)) tracing::debug!("Hovered entry ({row}, {col})."));
        eval entry_selected ([]((row, col)) tracing::debug!("Selected entry ({row}, {col})."));
        eval view.entry_accepted ([]((row, col)) tracing::debug!("ACCEPTED entry ({row}, {col})."));
    }
    view.set_entries_size(Vector2(130.0, 28.0));
    let params = grid_view::simple::EntryParams {
        bg_color: color::Rgba(0.8, 0.8, 0.9, 1.0),
        bg_margin: 1.0,
        hover_color: color::Rgba(0.0, 1.0, 0.0, 1.0),
        selection_color: color::Rgba(1.0, 0.0, 0.0, 1.0),
        ..default()
    };
    view.set_entries_params(params);
    view.scroll_frp().resize(Vector2(400.0, 300.0));
    view.reset_entries(1000, 1000);
    std::mem::forget(network);
    app.display.add_child(&view);
    view
}



// ========================
// === Init Application ===
// ========================

fn init(app: &Application) {
    theme::builtin::dark::register(&app);
    theme::builtin::light::register(&app);
    theme::builtin::light::enable(&app);

    let main_layer = &app.display.default_scene.layers.node_searcher;
    let grids_layer = main_layer.create_sublayer();
    let hover_layer = main_layer.create_sublayer();
    let selection_layer = main_layer.create_sublayer();

    let grid_views = std::iter::repeat_with(|| setup_grid_view(app)).take(3).collect_vec();
    let with_hover_mask = [&grid_views[2]];
    let with_selection_mask = [&grid_views[1], &grid_views[2]];
    let positions = itertools::iproduct!([-450.0, 50.0], [350.0, -50.0]);

    for (view, (x, y)) in grid_views.iter().zip(positions) {
        grids_layer.add_exclusive(view);
        view.set_position_xy(Vector2(x, y));
    }

    let view = &grid_views[0];
    for i in (0..1000).step_by(2) {
        view.set_column_width((i, 60.0));
    }

    for view in with_hover_mask {
        view.hover_highlight_frp().setup_masked_layer(Some(hover_layer.downgrade()));
        let params = grid_view::simple::EntryParams {
            bg_color: color::Rgba(0.7, 0.7, 0.9, 1.0),
            bg_margin: 0.0,
            text_offset: 8.0,
            text_color: color::Rgba(0.9, 0.9, 0.9, 1.0),
            ..default()
        };
        view.hover_highlight_frp().set_entries_params(params);
    }

    for view in with_selection_mask {
        view.selection_highlight_frp().setup_masked_layer(Some(selection_layer.downgrade()));
        let params = grid_view::simple::EntryParams {
            bg_color: color::Rgba(0.5, 0.5, 0.5, 1.0),
            bg_margin: 0.0,
            text_color: color::Rgba(1.0, 1.0, 1.0, 1.0),
            text_offset: 8.0,
            ..default()
        };
        view.selection_highlight_frp().set_entries_params(params);
    }

    let navigator = Navigator::new(
        &app.display.default_scene,
        &app.display.default_scene.layers.node_searcher.camera(),
    );
    navigator.disable_wheel_panning();

    std::mem::forget(grid_views);
    std::mem::forget(grids_layer);
    std::mem::forget(hover_layer);
    std::mem::forget(selection_layer);
    std::mem::forget(navigator);
}
