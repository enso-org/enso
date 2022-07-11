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
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use ensogl_core::prelude::*;
use rand::Rng;
use wasm_bindgen::prelude::*;

use enso_frp as frp;
use enso_text::unit::Bytes;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::display::shape::*;
use ensogl_grid_view as grid_view;
use ensogl_grid_view::entry;
use ensogl_grid_view::VisibleArea;
use ensogl_hardcoded_theme as theme;
use ensogl_text_msdf_sys::run_once_initialized;
use logger::TraceLogger as Logger;



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

// ==========================
// === Visible Area Shape ===
// ==========================

mod visible_area {
    use super::*;

    ensogl_core::define_shape_system! {
        (style:Style) {
            let width  : Var<Pixels> = "input_size.x".into();
            let height : Var<Pixels> = "input_size.y".into();
            let outer = Rect((width, height));
            let inner = outer.shrink(2.0.px());
            let shape = (outer - inner).fill(color::Rgba::new(0.4, 0.4, 0.4, 1.0));
            shape.into()
        }
    }

    pub fn update(view: &View, data: VisibleArea) {
        view.size.set(data.size);
        view.set_position_xy((data.left_top + data.right_bottom() / 2.0));
    }
}



// ========================
// === Init Application ===
// ========================

fn init(app: &Application) {
    theme::builtin::dark::register(&app);
    theme::builtin::light::register(&app);
    theme::builtin::light::enable(&app);

    let grid_view = ensogl_grid_view::GridView::<entry::Label>::new(app);
    app.display.default_scene.layers.node_searcher.add_exclusive(&grid_view);
    frp::new_network! { network
        requested_entry <- grid_view.model_for_entry_needed.map(|(row, col)| {
            (*row, *col, ImString::from(format!("Entry ({row}, {col})")))
        });
        grid_view.model_for_entry <+ requested_entry;
    }
    grid_view.set_entries_size(Vector2(130.0, 28.0));
    grid_view.set_entries_params(
        entry::LabelParams::default()
            .with_layer(&app.display.default_scene.layers.node_searcher_text),
    );
    grid_view.reset_entries(1000, 1000);
    let mut visible_area =
        VisibleArea { left_top: Vector2(0.0, 0.0), size: Vector2(400.0, 300.0) };
    let mut visible_area_change = Vector2(0.2, -0.2);
    grid_view.set_visible_area(visible_area);

    app.display.add_child(&grid_view);
    let navigator = Navigator::new(
        &app.display.default_scene,
        &app.display.default_scene.layers.node_searcher.camera(),
    );

    let visible_area_shape = visible_area::View::new(DefaultWarningLogger::new("visible_area"));
    visible_area::update(&visible_area_shape, visible_area);
    app.display.add_child(&visible_area_shape);
    app.display.default_scene.layers.node_searcher.add_exclusive(&visible_area_shape);

    let mut rng = rand::thread_rng();
    app.display
        .on
        .after_frame
        .add(move |_| {
            visible_area.left_top += visible_area_change;
            grid_view.set_visible_area(visible_area);
            visible_area::update(&visible_area_shape, visible_area);
            if rng.gen_bool(0.0005) {
                visible_area_change.x *= -1.0;
            }
            if rng.gen_bool(0.0005) {
                visible_area_change.y *= -1.0;
            }
        })
        .forget();

    std::mem::forget(network);
    std::mem::forget(navigator);
}
