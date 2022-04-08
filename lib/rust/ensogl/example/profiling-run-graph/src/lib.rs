//! Demo scene showing a sample flame graph. Can be used to display a log file, if you have one.
//! To do so, set the `PROFILER_LOG_NAME` to contain the profiling log name and it
//! will be used for rendering the visualisation. See the docs of `PROFILER_LOG_NAME` for more
//! information.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![allow(unused_qualifications)]

use ensogl_core::prelude::*;
use wasm_bindgen::prelude::*;

use enso_profiler as profiler;
use enso_profiler::profile;
use enso_profiler_data::Profile;
use enso_profiler_flame_graph as profiler_flame_graph;
use enso_profiler_metadata::Metadata;
use enso_profiler_flame_graph::COLOR_BLOCK_ACTIVE;
use enso_profiler_flame_graph::COLOR_BLOCK_PAUSED;
use enso_profiler_flame_graph::COLOR_MARK_DEFAULT;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::display::style::theme;
use ensogl_core::display::Scene;
use ensogl_core::profiler::log_rpc_event;
use ensogl_core::profiler::Metadata;
use ensogl_core::system::web;
use ensogl_flame_graph as flame_graph;



// =================
// === Constants ===
// =================

/// Content of a profiler log, that will be rendered. If this is `None` some dummy data will be
/// generated and rendered.  The file must be located in the assets subdirectory that is
/// served by the webserver, i.e. `enso/dist/content` or `app/ide-desktop/lib/content/assets`.
/// For example use `Some("profile.json"))`.
const PROFILER_LOG_NAME: Option<&str> = None;

const COLOR_FPS_GOOD: &str = "flame_graph_block_color_fps_good";
const COLOR_FPS_MEDIUM: &str = "flame_graph_block_color_fps_medium";
const COLOR_FPS_BAD: &str = "flame_graph_block_color_fps_bad";



// ===================
// === Entry Point ===
// ===================

/// The example entry point.
#[entry_point]
#[allow(dead_code)]
pub async fn main() {
    web::forward_panic_hook_to_console();
    web::set_stack_trace_limit();

    let app = &Application::new("root");
    let world = &app.display;
    let scene = &world.default_scene;
    let camera = scene.camera().clone_ref();
    let navigator = Navigator::new(scene, &camera);

    init_theme(scene);

    let profile =
        if let Some(profile) = get_log_data().await { profile } else { create_dummy_data().await };

    let measurements = profiler_flame_graph::Graph::new_hybrid_graph(&profile);

    let mut flame_graph = flame_graph::FlameGraph::from_data(measurements, app);

    let marks = make_marks_from_profile(&profile);
    flame_graph.add_marks(marks.into_iter());

    let performance_blocks = make_rendering_performance_blocks(&profile);
    flame_graph.add_blocks(performance_blocks.into_iter());

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
    theme.set(COLOR_BLOCK_ACTIVE, color::Lcha::blue_green(0.5, 0.8));
    theme.set(COLOR_BLOCK_PAUSED, color::Lcha::blue_green(0.8, 0.0));
    theme.set(COLOR_FPS_BAD, color::Lcha::red(0.4, 0.5));
    theme.set(COLOR_FPS_GOOD, color::Lcha::green(0.8, 0.5));
    theme.set(COLOR_FPS_MEDIUM, color::Lcha::yellow(0.6, 0.5));
    theme.set(COLOR_MARK_DEFAULT, color::Lcha::blue_green(0.9, 0.1));

    theme_manager.register("theme", theme);
    theme_manager.set_enabled(&["theme".to_string()]);
}



// ===========================
// === Metadata Processing ===
// ===========================


// Create marks for metadata. This will skip `RenderStats` as they are added separately as blocks.
fn make_marks_from_profile(profile: &Profile<Metadata>) -> Vec<profiler_flame_graph::Mark> {
    profile
        .iter_metadata()
        .filter_map(|metadata: &enso_profiler_data::Metadata<ensogl_core::profiler::Metadata>| {
            let position = metadata.mark.into_ms();
            match metadata.data {
                Metadata::RenderStats(_) => None,
                _ => {
                    let label = metadata.data.to_string();
                    Some(profiler_flame_graph::Mark { position, label })
                }
            }
        })
        .collect()
}

fn make_rendering_performance_blocks(
    profile: &Profile<Metadata>,
) -> Vec<profiler_flame_graph::Block> {
    let mut blocks = Vec::default();
    let render_stats = profile
        .iter_metadata()
        .filter(|metadata| matches!(metadata.data, Metadata::RenderStats(_)));
    for (prev, current) in render_stats.tuple_windows() {
        if let Metadata::RenderStats(data) = current.data {
            let start = prev.mark.into_ms();
            let end = current.mark.into_ms();
            let row = -1;
            let label = format!("{:#?}", data);
            let theme_color = match data.fps {
                fps if fps > 55.0 => COLOR_FPS_GOOD,
                fps if fps > 25.0 => COLOR_FPS_MEDIUM,
                _ => COLOR_FPS_BAD,
            };
            let block = profiler_flame_graph::Block { start, end, row, label, theme_color };
            blocks.push(block);
        }
    }
    blocks
}




// ============================
// === Profiler Log Reading ===
// ============================

/// Read the `PROFILER_LOG_NAME` data from a file.
async fn get_data_raw() -> Option<String> {
    use wasm_bindgen::JsCast;

    let url = &["assets/", PROFILER_LOG_NAME?].concat();
    let mut opts = web_sys::RequestInit::new();
    opts.method("GET");
    opts.mode(web_sys::RequestMode::Cors);
    let request = web_sys::Request::new_with_str_and_init(url, &opts).unwrap();
    request.headers().set("Accept", "application/json").unwrap();
    let window = web_sys::window().unwrap();
    let response = window.fetch_with_request(&request);
    let response = wasm_bindgen_futures::JsFuture::from(response).await.unwrap();
    assert!(response.is_instance_of::<web_sys::Response>());
    let response: web_sys::Response = response.dyn_into().unwrap();
    let data = response.text().unwrap();
    let data = wasm_bindgen_futures::JsFuture::from(data).await.unwrap();
    data.as_string()
}

async fn get_log_data() -> Option<Profile<Metadata>> {
    let data = get_data_raw().await;
    data.and_then(|data| data.parse().ok())
}



// ==========================
// === Dummy Computations ===
// ==========================

async fn create_dummy_data() -> Profile<Metadata> {
    start_project().await;

    let log = profiler::internal::take_log();
    let profile: Result<Profile<Metadata>, _> = log.parse();
    profile.expect("Failed to deserialize profiling event log.")
}

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
    log_rpc_event("Dragon Awake");
    feed_troll();
    log_rpc_event("Troll Fed");
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
