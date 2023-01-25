//! This is a visualization example scene which creates a sinusoidal graph.

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

use ensogl::prelude::*;

use crate::text_visualization::TextGrid;

use ensogl::animation;
use ensogl::application::Application;
use ensogl::display::navigation::navigator::Navigator;
use ensogl::system::web;
use ensogl::system::web::traits::DocumentOps;
use ensogl::system::web::traits::ElementOps;
use ensogl_text_msdf::run_once_initialized;
use ide_view::graph_editor::builtin::visualization::native::text_visualization;
use ide_view::graph_editor::builtin::visualization::native::text_visualization::text_provider;



fn sample_text() -> String {
    let mut text = String::new();
    for n in (0..1001).rev() {
        match n {
            0 => {
                text.push_str("No more bottles of beer on the wall, no more bottles of beer.");
                text.push_str("Go to the store and buy some more, 99 bottles of beer on the wall.");
            }
            1 => {
                text.push_str("1 bottle of beer on the wall, 1 bottle of beer.");
                text.push_str(
                    "Take one down and pass it around, no more bottles of beer on the wall.\n",
                );
            }
            _ => {
                text.push_str(&format!(
                    "{0:?} bottles of beer on the wall, {0:?} bottles of beer.",
                    n
                ));
                text.push_str(&format!(
                    "Take one down and pass it around, {} bottles of beer on the wall.\n",
                    n - 1
                ));
            }
        }
    }
    text
}

#[entry_point]
#[allow(dead_code, missing_docs)]
pub fn main() {
    run_once_initialized(|| {
        let app = Application::new("root");
        init(&app);
        mem::forget(app);
    });
}

fn init(app: &Application) {
    let app = app.clone_ref();

    let font_tag = web::document.create_element_or_panic("link");
    font_tag.set_attribute_or_warn("rel", "stylesheet");
    font_tag.set_attribute_or_warn("media", "screen");
    font_tag.set_attribute_or_warn("type", "text/css");
    font_tag.set_attribute_or_warn("href", "https://fontlibrary.org/face/dejavu-sans-mono");

    web::document
        .head()
        .unwrap()
        .append_child(&font_tag)
        .expect("Failed to add font to HTML body.");

    let closure = ensogl::system::web::Closure::new(move |_| {
        let world = &app.display;
        let scene = &world.default_scene;
        let camera = scene.camera();
        let navigator = Navigator::new(scene, &camera);

        let sample_text_data = sample_text();
        let text_source = text_provider::StringTextProvider::new(sample_text_data);

        let grid = TextGrid::new(app.clone_ref());
        grid.set_text_provider(text_source);
        grid.frp.set_size.emit(Vector2::new(200.0, 200.0));

        let mut was_rendered = false;
        let mut loader_hidden = false;

        scene.add_child(&grid);

        world
            .on
            .before_frame
            .add(move |_time_info: animation::TimeInfo| {
                let _keep_alive = &navigator;
                let _keep_alive = &grid;
                if was_rendered && !loader_hidden {
                    web::document
                        .get_element_by_id("loader")
                        .map(|t| t.parent_node().map(|p| p.remove_child(&t).unwrap()));
                    loader_hidden = true;
                }
                was_rendered = true;
            })
            .forget();
    });
    let _result = web::document.fonts().ready().unwrap().then(&closure);
    // This extends the lifetime of the closure which is what we want here. Otherwise, the closure
    // would be destroyed and the callback cannot be called.
    mem::forget(closure);
}
