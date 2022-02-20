//! Example scene showing simple usage of a shape system.

pub mod flame_graph;
pub mod measurements;

use ensogl_core::prelude::*;

use enso_profiler as profiler;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::display::style::theme;
use ensogl_core::display::Scene;
use ensogl_core::system::web;
use measurements::Measurements;
use wasm_bindgen::prelude::*;


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

    let measurements = dummy_measurements();
    let blocks: flame_graph::data::FlameGraph = measurements.into();
    let flamegraph = flame_graph::FlameGraph::from_data(blocks, app);

    world.add_child(&flamegraph);
    scene.add_child(&flamegraph);
    scene.layers.main.add_exclusive(&flamegraph);

    world.keep_alive_forever();
    let scene = world.scene().clone_ref();

    world
        .on_frame(move |_time| {
            let _keep_alive = &navigator;
            let _keep_alive = &scene;
            let _keep_alive = &flamegraph;
        })
        .forget();
}

fn init_theme(scene: &Scene) {
    let theme_manager = theme::Manager::from(&scene.style_sheet);

    let theme1 = theme::Theme::new();
    theme1.set("base_color", color::Rgb::new(1.0, 45.0 / 255.0, 0.0));

    theme_manager.register("theme1", theme1);

    theme_manager.set_enabled(&["theme1".to_string()]);

    let style_watch = ensogl_core::display::shape::StyleWatch::new(&scene.style_sheet);
    style_watch.get("base_color");

    // mem::forget(theme_manager);
    // mem::forget(style_watch);
}



fn dummy_measurements() -> Measurements {
    use enso_profiler::*;

    let app_loading = profiler::Measurement {
        profiler: ProfilerId::new(),
        parent:   ProfilerId::root(),
        start:    Some(Timestamp::from_ms(0.0)),
        end:      Timestamp::from_ms(500.0),
        label:    "App Loading",
    };

    let app_loading_foo = profiler::Measurement {
        profiler: ProfilerId::new(),
        parent:   app_loading.profiler,
        start:    None,
        end:      Timestamp::from_ms(200.0),
        label:    "App Loading Foo",
    };
    let app_loading_bar = profiler::Measurement {
        profiler: ProfilerId::new(),
        parent:   app_loading.profiler,
        start:    Some(Timestamp::from_ms(200.0)),
        end:      Timestamp::from_ms(250.0),
        label:    "App Loading Bar",
    };
    let app_loading_baz = profiler::Measurement {
        profiler: ProfilerId::new(),
        parent:   app_loading.profiler,
        start:    Some(Timestamp::from_ms(250.0)),
        end:      Timestamp::from_ms(500.0),
        label:    "App Loading Baz",
    };

    let project_opening = profiler::Measurement {
        profiler: ProfilerId::new(),
        parent:   ProfilerId::root(),
        start:    Some(Timestamp::from_ms(505.0)),
        end:      Timestamp::from_ms(1000.0),
        label:    "Opening Project",
    };

    let project_opening_a = profiler::Measurement {
        profiler: ProfilerId::new(),
        parent:   project_opening.profiler,
        start:    None,
        end:      Timestamp::from_ms(600.0),
        label:    "Cooking Eggs",
    };

    let project_opening_a_a = profiler::Measurement {
        profiler: ProfilerId::new(),
        parent:   project_opening_a.profiler,
        start:    None,
        end:      Timestamp::from_ms(567.0),
        label:    "Breaking Eggs",
    };

    let project_opening_a_b = profiler::Measurement {
        profiler: ProfilerId::new(),
        parent:   project_opening_a.profiler,
        start:    Some(Timestamp::from_ms(567.0)),
        end:      Timestamp::from_ms(580.0),
        label:    "Scrambling Eggs",
    };

    let project_opening_a_c = profiler::Measurement {
        profiler: ProfilerId::new(),
        parent:   project_opening_a.profiler,
        start:    Some(Timestamp::from_ms(588.0)),
        end:      Timestamp::from_ms(590.0),
        label:    "Frying Eggs",
    };


    vec![
        app_loading,
        app_loading_foo,
        app_loading_bar,
        app_loading_baz,
        project_opening,
        project_opening_a,
        project_opening_a_a,
        project_opening_a_b,
        project_opening_a_c,
    ]
    .into()
}
