//! Defines profilable workflows, and an entry point that runs a specified workflow.

use crate::integration_test::prelude::*;
use wasm_bindgen::prelude::*;

use enso_debug_api as debug_api;
use enso_web as web;



// ===================
// === Entry point ===
// ===================

/// Startup function for running and profiling a test workflow.
#[wasm_bindgen]
#[allow(dead_code)] // Used from JavaScript.
pub async fn entry_point_profile() {
    web::forward_panic_hook_to_console();

    // Run selected workflow.
    let need_workflow = "`profile` entry point requires --workflow argument. \
    Try --workflow=help to see a list of options.";
    let selected = enso_config::ARGS.test_workflow.as_ref().expect(need_workflow);
    reflect_match!(match selected as options {
        "collapse_nodes" => profile_collapse_nodes().await,
        "create_node" => profile_create_node().await,
        "enter_collapsed_node" => profile_enter_collapsed_node().await,
        "new_project" => profile_new_project().await,
        "open_visualization" => profile_open_visualization().await,
        _ => panic!("Unknown workflow: {selected}. Must be one of: {options:?}."),
    });

    // Emit profile and exit.
    debug_api::save_profile(&profiler::internal::take_log());
    debug_api::LifecycleController::new().expect("Workflows run in Electron").quit();
}



// ================
// === Metadata ===
// ================

enso_profiler::metadata_logger!("ProfileRegion", end_profile_region(()));



// ============================
// === Workflow definitions ===
// ============================

async fn profile_create_node() {
    let test = Fixture::create_project().await;
    let _profiler = profiler::start_objective!(profiler::APP_LIFETIME, "@highlight");
    test.create_node("1").await;
}

async fn profile_collapse_nodes() {
    let test = Fixture::create_project().await;
    test.graph_editor().select_all_nodes();
    let _profiler = profiler::start_objective!(profiler::APP_LIFETIME, "@highlight");
    test.collapse_selected_nodes().await;
}

async fn profile_enter_collapsed_node() {
    let test = Fixture::create_project().await;
    test.graph_editor().select_all_nodes();
    let id = test.collapse_selected_nodes().await;
    test.graph_editor().select_node(id);
    let _profiler = profiler::start_objective!(profiler::APP_LIFETIME, "@highlight");
    test.enter_selected_node().await;
}

async fn profile_new_project() {
    let _profiler = profiler::start_objective!(profiler::APP_LIFETIME, "@highlight");
    let _ = Fixture::create_project().await;
}

async fn profile_open_visualization() {
    let test = Fixture::create_project().await;
    let graph_editor = test.graph_editor();
    let node = InitialNodes::obtain_from_graph_editor(&graph_editor).below.0;
    graph_editor.select_node(node);
    let _profiler = profiler::start_objective!(profiler::APP_LIFETIME, "@highlight");
    test.visualize_selected_nodes().await;
}
