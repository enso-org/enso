//! Demo scene showing a sample flame graph. Can be used to display a log file, if you have one.
//! To do so, set a query parameter in the url to contain `file=<name _of_log_file>` and it
//! will be used for rendering the visualisation. Note that the log file needs to be located in
//! the assets subdirectory that is served by the webserver, i.e. `enso/dist/content` or
//! `app/ide-desktop/lib/content/assets`. If no name is given a file named `profile.json` will
//! be loaded by default. If that file is not present, some dummy data will be displayed.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===
#![allow(unused_qualifications)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]

use ensogl_core::prelude::*;
use wasm_bindgen::prelude::*;

use enso_profiler as profiler;
use enso_profiler::profile;
use enso_profiler_data::parse_multiprocess_profile;
use enso_profiler_data::Profile;
use enso_profiler_enso_data::Metadata;
use enso_profiler_flame_graph as profiler_flame_graph;
use enso_profiler_flame_graph::Performance;
use ensogl_core::application::command::FrpNetworkProvider;
use ensogl_core::application::tooltip;
use ensogl_core::application::tooltip::Placement;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::display::style::theme;
use ensogl_core::display::Scene;
use ensogl_core::frp;
use ensogl_flame_graph as flame_graph;
use ensogl_flame_graph::COLOR_BLOCK_ACTIVE;
use ensogl_flame_graph::COLOR_BLOCK_PAUSED;
use ensogl_flame_graph::COLOR_MARK_DEFAULT;
use ensogl_flame_graph::COLOR_PERFORMANCE_BAD;
use ensogl_flame_graph::COLOR_PERFORMANCE_GOOD;
use ensogl_flame_graph::COLOR_PERFORMANCE_MEDIUM;
use ensogl_sequence_diagram::SequenceDiagram;



// =================
// === Constants ===
// =================

const DEFAULT_LOG_NAME: &str = "profile.json";
const SHOW_RPC_EVENT_MARKS: bool = true;
const SHOW_BACKEND_MESSAGE_MARKS: bool = true;



// ===================
// === Entry Point ===
// ===================


/// The example entry point.
#[entry_point]
#[allow(dead_code)]
pub async fn main() {
    let app = &Application::new("root");
    let world = &app.display;
    let scene = &world.default_scene;
    let camera = scene.camera().clone_ref();
    let navigator = Navigator::new(scene, &camera);
    let network = app.frp.network();

    init_theme(scene);

    let profiles = get_log_data().await;

    let base_profile = &profiles[0];
    let flame_graph = profile_to_graph(base_profile, app);
    scene.add_child(&flame_graph);
    scene.layers.main.add_exclusive(&flame_graph);

    let sequence_diagram = SequenceDiagram::new(app);
    sequence_diagram.set_profile(profiles);

    let graph_height: f32 = flame_graph.height();
    let sequence_diagram_offset = graph_height + sequence_diagram.height.value();
    sequence_diagram.set_position_y(-sequence_diagram_offset);

    scene.add_child(&sequence_diagram);
    scene.layers.main.add_exclusive(&sequence_diagram);

    let tooltip = ensogl_tooltip::Tooltip::new(app);
    scene.add_child(&tooltip);
    tooltip.frp.set_placement.emit(Placement::Right);
    frp::extend! { network
        tooltip.frp.set_style <+ app.frp.tooltip.map(|tt| tt.clone().with_placement(tooltip::Placement::Right));
    }

    world.keep_alive_forever();
    let scene = world.default_scene.clone_ref();

    world
        .on
        .before_frame
        .add(move |_time| {
            let _keep_alive = &navigator;
            let _keep_alive = &scene;
            let _keep_alive = &flame_graph;
            let _keep_alive = &sequence_diagram;
            let _keep_alive = &tooltip;
        })
        .forget();
}

fn init_theme(scene: &Scene) {
    let theme_manager = theme::Manager::from(&scene.style_sheet);

    let theme = theme::Theme::new();
    theme.set(COLOR_BLOCK_ACTIVE, color::Lcha::blue_green(0.5, 0.8));
    theme.set(COLOR_BLOCK_PAUSED, color::Lcha::blue_green(0.8, 0.0));
    theme.set(COLOR_PERFORMANCE_BAD, color::Lcha::red(0.4, 0.5));
    theme.set(COLOR_PERFORMANCE_GOOD, color::Lcha::green(0.8, 0.5));
    theme.set(COLOR_PERFORMANCE_MEDIUM, color::Lcha::yellow(0.6, 0.5));
    theme.set(COLOR_MARK_DEFAULT, color::Lcha::blue_green(0.9, 0.1));
    theme.set("component.label.text", color::Lcha::black());

    theme_manager.register("theme", theme);
    theme_manager.set_enabled(&["theme".to_string()]);
}



mod js {
    use super::*;

    #[wasm_bindgen(inline_js = "
export function get_url() {
    return window.location.href
}

")]
    extern "C" {
        #[allow(unsafe_code)]
        pub fn get_url() -> String;
    }
}

fn get_target_file_from_url() -> Option<String> {
    let url = js::get_url();
    let url = url::Url::parse(&url).ok()?;
    let query = url.query()?;
    let query = qstring::QString::from(query);
    query.get("file").map(|s| s.to_owned())
}


// ===========================
// === Metadata Processing ===
// ===========================

fn profile_to_graph(profile: &Profile<Metadata>, app: &Application) -> flame_graph::FlameGraph {
    let mut measurements = profiler_flame_graph::Graph::new_hybrid_graph(profile);
    let marks = make_marks_from_profile(profile);
    measurements.marks = marks;

    let performance_blocks = make_rendering_performance_blocks(profile);
    measurements.performance_blocks = performance_blocks;

    flame_graph::FlameGraph::from_data(measurements, app)
}

// Create marks for metadata. This will skip `RenderStats` as they are added separately as blocks.
fn make_marks_from_profile(profile: &Profile<Metadata>) -> Vec<profiler_flame_graph::Mark> {
    profile
        .metadata()
        .filter_map(|metadata: &enso_profiler_data::Timestamped<Metadata>| match metadata.data {
            Metadata::RenderStats(_) => None,
            Metadata::RpcEvent(_) if !SHOW_RPC_EVENT_MARKS => None,
            Metadata::BackendMessage(_) if !SHOW_BACKEND_MESSAGE_MARKS => None,
            _ => {
                let position = metadata.time.into_ms();
                let label = metadata.data.to_string();
                Some(profiler_flame_graph::Mark { position, label })
            }
        })
        .collect()
}

fn make_rendering_performance_blocks(
    profile: &Profile<Metadata>,
) -> Vec<profiler_flame_graph::Block<Performance>> {
    let mut blocks = Vec::default();
    let render_stats = profile.metadata().filter_map(|metadata| match metadata.data {
        Metadata::RenderStats(data) => Some(metadata.as_ref().map(|_| data)),
        _ => None,
    });
    for (prev, current) in render_stats.tuple_windows() {
        let start = prev.time.into_ms();
        let end = current.time.into_ms();
        let row = -1;
        let label = format!("{:#?}", current.data);
        let block_type = match current.data.fps {
            fps if fps > 55.0 => Performance::Good,
            fps if fps > 25.0 => Performance::Medium,
            _ => Performance::Bad,
        };
        let block = profiler_flame_graph::Block { start, end, row, label, block_type };
        blocks.push(block);
    }
    blocks
}



// ============================
// === Profiler Log Reading ===
// ============================

/// Read the data from a file specified on the command line.
async fn get_data_file() -> Option<String> {
    let files = enso_debug_api::load_profiles()?.await;
    if files.len() > 1 {
        ERROR!("Entry point profiling-run-graph doesn't support multiple profile file arguments.");
    }
    files.into_iter().next()
}

/// Read the `PROFILER_LOG_NAME` data from a file.
async fn get_data_http() -> Option<String> {
    use wasm_bindgen::JsCast;

    let file_name = get_target_file_from_url();
    let file_name = file_name.as_deref().unwrap_or(DEFAULT_LOG_NAME);
    let url = &["assets/", file_name].concat();
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

async fn get_log_data() -> Vec<Profile<Metadata>> {
    let data = match get_data_file().await {
        Some(data) => Some(data),
        None => get_data_http().await,
    };
    let data = data.map(|data| {
        parse_multiprocess_profile(&data)
            .filter_map(|result| match result {
                Ok(profile) => Some(profile),
                Err(e) => {
                    ERROR!(e);
                    None
                }
            })
            .collect()
    });
    match data {
        Some(data) => data,
        None => {
            let dummy_data = create_dummy_data().await;
            vec![dummy_data]
        }
    }
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
