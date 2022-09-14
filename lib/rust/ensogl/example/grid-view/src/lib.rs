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
use ensogl_core::application::View;
use ensogl_core::data::color;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;
use ensogl_grid_view as grid_view;
use ensogl_grid_view::Col;
use ensogl_grid_view::Margins;
use ensogl_grid_view::Row;
use ensogl_hardcoded_theme as theme;
use ensogl_text_msdf::run_once_initialized;



// =================
// === Constants ===
// =================

const ENTRY_HEIGHT: f32 = 28.0;
const VIEWPORT_HEIGHT: f32 = 300.0;
const BASE_SCROLL_MARGIN: f32 = 10.0;



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

fn configure_simple_grid_view(
    view: &grid_view::simple::SimpleGridView,
) -> frp::Network {
    frp::new_network! { network
        requested_entry <-
            view.model_for_entry_needed.map(|&(row, col)| (row, col, entry_model(row, col)));
        view.model_for_entry <+ requested_entry;
        entry_hovered <- view.entry_hovered.filter_map(|l| *l);
        entry_selected <- view.entry_selected.filter_map(|l| *l);
        eval entry_hovered ([]((row, col)) tracing::debug!("Hovered entry ({row}, {col})."));
        eval entry_selected ([]((row, col)) tracing::debug!("Selected entry ({row}, {col})."));
        eval view.entry_accepted ([]((row, col)) tracing::debug!("ACCEPTED entry ({row}, {col})."));
        eval view.selection_movement_out_of_grid_prevented ([](dir)
            if let Some(dir) = dir {
                let msg = iformat!(
                    "An attempt to select an entry outside of the grid in " dir;?
                    " direction was prevented."
                );
                tracing::debug!("{msg}");
            }
        );
    }
    view.set_entries_size(Vector2(130.0, ENTRY_HEIGHT));
    let params = grid_view::simple::EntryParams {
        bg_color: color::Rgba(0.8, 0.8, 0.9, 1.0),
        bg_margin: 1.0,
        hover_color: color::Rgba(0.0, 1.0, 0.0, 1.0),
        selection_color: color::Rgba(1.0, 0.0, 0.0, 1.0),
        ..default()
    };
    view.set_entries_params(params);
    // view.reset_entries(1000, 1000);
    view.reset_entries(100, 100);
    network
}

fn configure_scrollable_grid_view<InnerGridView>(
    view: &grid_view::scrollable::GridViewTemplate<InnerGridView>,
) {
    view.scroll_frp().resize(Vector2(400.0, VIEWPORT_HEIGHT));
    let scroll_margins = Margins {
        top:    VIEWPORT_HEIGHT - BASE_SCROLL_MARGIN - ENTRY_HEIGHT,
        bottom: BASE_SCROLL_MARGIN,
        left:   BASE_SCROLL_MARGIN,
        right:  BASE_SCROLL_MARGIN,
    };
    view.extra_scroll_frp().set_preferred_margins_around_entry(scroll_margins);
}

fn setup_simple_grid_view(
    app: &Application,
) -> grid_view::simple::SimpleScrollableSelectableGridView {
    let view = grid_view::simple::SimpleScrollableSelectableGridView::new(app);
    let network = configure_simple_grid_view(&view);
    configure_scrollable_grid_view(&view);
    std::mem::forget(network);
    app.display.add_child(&view);
    view
}

fn setup_grid_view_with_headers(
    app: &Application,
) -> grid_view::simple::SimpleScrollableSelectableGridViewWithHeaders {
    let view = grid_view::simple::SimpleScrollableSelectableGridViewWithHeaders::new(app);
    let network = configure_simple_grid_view(&view);
    configure_scrollable_grid_view(&view);
    let header_frp = view.header_frp();
    frp::extend! { network
        requested_section <- header_frp.section_info_needed.map(|&(row, col)| {
            let sections_size = 2 + col;
            let section_start = row - (row % sections_size);
            let section_end = section_start + sections_size;
            let model = entry_model(section_start, col);
            (section_start..section_end, col, model)
        });
        header_frp.section_info <+ requested_section;
    }
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

    let grid_view_no_headers = setup_simple_grid_view(app);
    grid_view_no_headers.frp().focus();
    let grid_views_with_headers = std::iter::repeat_with(|| setup_grid_view_with_headers(app)).take(3).collect_vec();
    let with_hover_mask = [&grid_views_with_headers[2]];
    let with_selection_mask = [&grid_views_with_headers[1], &grid_views_with_headers[2]];
    // grid_views_with_headers[2].frp().focus();
    // let grid_views = std::iter::repeat_with(|| setup_grid_view(app)).take(1).collect_vec();
    // let with_hover_mask = [&grid_views[0]];
    // let with_selection_mask = [&grid_views[0]];
    // grid_views[0].frp().focus();
    let pair_to_vector = |(x, y)| Vector2(x, y);
    let mut positions = itertools::iproduct!([-450.0, 50.0], [350.0, -50.0]).map(pair_to_vector);

    grids_layer.add_exclusive(&grid_view_no_headers);
    grid_view_no_headers.set_position_xy(positions.next().unwrap());
    for (view, position) in grid_views_with_headers.iter().zip(positions) {
        grids_layer.add_exclusive(view);
        // view.set_position_xy(Vector2(x, y));
        view.set_position_xy(position);
    }

    let view = &grid_views_with_headers[0];
    // for i in (0..1000).step_by(2) {
    for i in (0..100).step_by(2) {
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

    std::mem::forget(grid_view_no_headers);
    std::mem::forget(grid_views_with_headers);
    std::mem::forget(grids_layer);
    std::mem::forget(hover_layer);
    std::mem::forget(selection_layer);
    std::mem::forget(navigator);
}
