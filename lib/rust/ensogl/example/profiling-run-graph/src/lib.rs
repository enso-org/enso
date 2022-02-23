//! Demo scene showing a sample flame graph.

#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use ensogl_core::prelude::*;
use wasm_bindgen::prelude::*;

use enso_profiler as profiler;
use enso_profiler::profile;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::display::style::theme;
use ensogl_core::display::Scene;
use ensogl_core::system::web;
use ensogl_flame_graph as flame_graph;



// ===================
// === Entry Point ===
// ===================

/// The example entry point.
#[wasm_bindgen]
#[allow(dead_code)]
pub fn entry_point_profiling_run_graph() {
    web::forward_panic_hook_to_console();
    web::set_stack_trace_limit();

    let app = &Application::new(&web::get_html_element_by_id("root").unwrap());
    let world = &app.display;
    let scene = world.scene();
    let camera = scene.camera().clone_ref();
    let navigator = Navigator::new(scene, &camera);

    init_theme(scene);

    // Generate Test data
    start_project(profiler::APP_LIFETIME);

    let measurements = profiler::flame_graph::FlameGraph::take_from_log();

    let flame_graph = flame_graph::FlameGraph::from_data(measurements, app);

    world.add_child(&flame_graph);
    scene.add_child(&flame_graph);
    scene.layers.main.add_exclusive(&flame_graph);

    world.keep_alive_forever();
    let scene = world.scene().clone_ref();

    world
        .on_frame(move |_time| {
            let _keep_alive = &navigator;
            let _keep_alive = &scene;
            let _keep_alive = &flame_graph;
        })
        .forget();
}

fn init_theme(scene: &Scene) {
    let theme_manager = theme::Manager::from(&scene.style_sheet);

    let theme = theme::Theme::new();
    theme.set("flame_graph_color", color::Rgb::new(1.0, 45.0 / 255.0, 0.0));

    theme_manager.register("theme", theme);

    theme_manager.set_enabled(&["theme".to_string()]);

    let style_watch = ensogl_core::display::shape::StyleWatch::new(&scene.style_sheet);
    style_watch.get("flame_graph_color");
}


// ==========================
// === Dummy Computations ===
// ==========================

/// A dummy computation that is intended to take some time based on input (where a higher number
///takes longer).
fn work(n: u32) {
    let mut m = n;
    for x in 0..n {
        for y in 0..n {
            for z in 0..n {
                m = m.wrapping_add(x * y * z)
            }
        }
    }
    // Create a side effect to avoid optimising away the computation.
    println!("{}", m % 7)
}

#[profile]
fn start_project(_profiler: profiler::Objective) {
    wake_dragon(_profiler);
    feed_troll(_profiler);
    ride_rainbow(_profiler);
}
#[profile]
fn ride_rainbow(_profiler: profiler::Objective) {
    work(777)
}
#[profile]
fn feed_troll(_profiler: profiler::Objective) {
    gather_herbs_and_spices(_profiler);
    cook_troll_food(_profiler);
    run_away(_profiler);
}
#[profile]
fn run_away(_profiler: profiler::Objective) {
    work(100)
}
#[profile]
fn cook_troll_food(_profiler: profiler::Objective) {
    work(100)
}
#[profile]
fn gather_herbs_and_spices(_profiler: profiler::Objective) {
    walk_to_woods(_profiler);
    search_stuff(_profiler);
    find_stuff(_profiler);
    gather_stuff(_profiler);
}
#[profile]
fn gather_stuff(_profiler: profiler::Objective) {
    work(100)
}
#[profile]
fn find_stuff(_profiler: profiler::Objective) {
    work(100)
}
#[profile]
fn search_stuff(_profiler: profiler::Objective) {
    work(100)
}
#[profile]
fn walk_to_woods(_profiler: profiler::Objective) {
    work(100)
}
#[profile]
fn wake_dragon(_profiler: profiler::Objective) {
    gather_gold(_profiler);
    bake_gold_cake(_profiler);
    start_tea_party(_profiler);
}
#[profile]
fn start_tea_party(_profiler: profiler::Objective) {
    work(100)
}
#[profile]
fn bake_gold_cake(_profiler: profiler::Objective) {
    work(100)
}
#[profile]
fn gather_gold(_profiler: profiler::Objective) {
    work(100)
}
