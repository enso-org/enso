//! Renders profiling data, obtained from a file, as a flame graph.

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

use enso_profiler_data as profiler_data;
use ensogl_core::application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::navigation::navigator;
use ensogl_core::display::style::theme;
use ensogl_flame_graph as flame_graph;



// ===================
// === Entry Point ===
// ===================

/// Render a graph of a profile file.
#[wasm_bindgen]
#[allow(dead_code)]
pub async fn entry_point_render_profile() {
    use ensogl_core::display::object::ObjectOps;
    let app = application::Application::new("root");
    let world = &app.display;
    let scene = &world.default_scene;
    let navigator = navigator::Navigator::new(scene, &scene.camera());
    init_theme(scene);
    let data = get_data().await;
    let measurements: profiler_data::Measurement<profiler_data::OpaqueMetadata> =
        data.parse().unwrap();
    let flame_graph = flame_graph::FlameGraph::from_data(measurements.into(), &app);
    scene.add_child(&flame_graph);
    scene.layers.main.add_exclusive(&flame_graph);
    world.keep_alive_forever();
    world
        .on
        .before_frame
        .add(move |_time| {
            let _keep_alive = &navigator;
            let _keep_alive = &flame_graph;
        })
        .forget();
}

/// Read profile data from a file. The file must be located at `/profile.json` in the root of the
/// directory that will be made available by the webserver, i.e. `enso/dist/content`.
async fn get_data() -> String {
    use wasm_bindgen::JsCast;

    let url = "/profile.json";
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
    data.as_string().unwrap()
}

fn init_theme(scene: &display::Scene) {
    let theme_manager = theme::Manager::from(&scene.style_sheet);
    let theme = theme::Theme::new();
    const COLOR_PATH: &str = "flame_graph_color";
    theme.set(COLOR_PATH, color::Rgb::new(1.0, 45.0 / 255.0, 0.0));
    theme_manager.register("theme", theme);
    theme_manager.set_enabled(&["theme".to_string()]);
    let style_watch = ensogl_core::display::shape::StyleWatch::new(&scene.style_sheet);
    style_watch.get(COLOR_PATH);
}
