//! Demo scene showing a sample flame graph.

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

use enso_profiler as profiler;
use enso_profiler::profile;
use enso_profiler_flame_graph as profiler_flame_graph;
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
#[entry_point]
#[allow(dead_code)]
pub fn main() {
    web::forward_panic_hook_to_console();
    web::set_stack_trace_limit();

    let app = &Application::new("root");
    let world = &app.display;
    let scene = &world.default_scene;
    let camera = scene.camera().clone_ref();
    let navigator = Navigator::new(scene, &camera);

    init_theme(scene);

    // Generate Test data
    futures::executor::block_on(start_project());

    let measurements = profiler_flame_graph::Graph::take_from_log();
    let flame_graph = flame_graph::FlameGraph::from_data(measurements, app);

    world.add_child(&flame_graph);
    scene.add_child(&flame_graph);
    scene.layers.main.add_exclusive(&flame_graph);

    world.keep_alive_forever();
    let scene = world.default_scene.clone_ref();

    world
        .on
        .before_frame
        .add(move |_time| {
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

#[profile(Objective)]
async fn start_project() {
    wake_dragon().await;
    feed_troll();
    ride_rainbow();
}
#[profile(Objective)]
fn ride_rainbow() {
    work(333)
}
#[profile(Objective)]
fn feed_troll() {
    gather_herbs_and_spices();
    cook_troll_food();
    run_away();
}
#[profile(Objective)]
fn run_away() {
    work(100)
}
#[profile(Objective)]
fn cook_troll_food() {
    work(100)
}
#[profile(Objective)]
fn gather_herbs_and_spices() {
    walk_to_woods();
    search_stuff();
    find_stuff();
    gather_stuff();
}
#[profile(Objective)]
fn gather_stuff() {
    work(100)
}
#[profile(Objective)]
fn find_stuff() {
    work(100)
}
#[profile(Objective)]
fn search_stuff() {
    work(100)
}
#[profile(Objective)]
fn walk_to_woods() {
    work(100)
}
#[profile(Objective)]
async fn wake_dragon() {
    gather_gold().await;
    bake_gold_cake().await;
    start_tea_party().await;
}
#[profile(Objective)]
async fn start_tea_party() {
    work(100)
}
#[profile(Objective)]
async fn bake_gold_cake() {
    work(100)
}
#[profile(Objective)]
fn pick_coin() {
    work(75)
}
#[profile(Objective)]
async fn gather_gold() {
    for _ in 0..5 {
        pick_coin()
    }
}
