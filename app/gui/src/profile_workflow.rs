//! Defines profilable workflows, and an entry point that runs a specified workflow.

use crate::integration_test::prelude::*;

use enso_debug_api as debug_api;



// ===================
// === Entry point ===
// ===================

/// Startup function for running and profiling a test workflow.
#[entry_point(profile)]
#[allow(dead_code)] // Used from JavaScript.
pub async fn main() {
    // Run selected workflow.
    let need_workflow = "`profile` entry point requires --workflow argument. \
    Try --workflow=help to see a list of options.";
    let selected = enso_config::ARGS.test_workflow.as_ref().expect(need_workflow);
    reflect_match!(match selected as options {
        "collapse_nodes" => profile_collapse_nodes().await,
        "create_node" => profile_create_node().await,
        "enter_collapsed_node" => profile_enter_collapsed_node().await,
        "new_project" => profile_new_project().await,
        "open_project_orders" => profile_open_project_orders().await,
        "open_visualization" => profile_open_visualization().await,
        _ => panic!("Unknown workflow: {selected}. Must be one of: {options:?}."),
    });

    // Emit profile and exit.
    debug_api::save_profile(&profiler::internal::get_log());
    debug_api::LifecycleController::new().expect("Workflows run in Electron").quit();
}



// ============================
// === Workflow definitions ===
// ============================

async fn profile_create_node() {
    let test = Fixture::create_project().await;
    let _profiler = profiler::start_objective!(profiler::APP_LIFETIME, "@highlight");
    profiler::await_!(test.create_node("1"), _profiler);
}

async fn profile_collapse_nodes() {
    let test = Fixture::create_project().await;
    test.graph_editor().select_all_nodes();
    let _profiler = profiler::start_objective!(profiler::APP_LIFETIME, "@highlight");
    profiler::await_!(test.collapse_selected_nodes(), _profiler);
}

async fn profile_enter_collapsed_node() {
    let test = Fixture::create_project().await;
    test.graph_editor().select_all_nodes();
    let id = test.collapse_selected_nodes().await;
    test.graph_editor().select_node(id);
    let _profiler = profiler::start_objective!(profiler::APP_LIFETIME, "@highlight");
    profiler::await_!(test.enter_selected_node(), _profiler);
}

async fn profile_new_project() {
    let _profiler = profiler::start_objective!(profiler::APP_LIFETIME, "@highlight");
    let _ = profiler::await_!(Fixture::create_project(), _profiler);
}

async fn profile_open_project_orders() {
    let _profiler = profiler::start_objective!(profiler::APP_LIFETIME, "@highlight");
    let _ = profiler::await_!(Fixture::open_project_orders(), _profiler);
}

async fn profile_open_visualization() {
    let test = Fixture::create_project().await;
    let graph_editor = test.graph_editor();
    let node = InitialNodes::obtain_from_graph_editor(&graph_editor).below.0;
    graph_editor.select_node(node);
    let _profiler = profiler::start_objective!(profiler::APP_LIFETIME, "@highlight");
    profiler::await_!(test.visualize_selected_nodes(), _profiler);
}
