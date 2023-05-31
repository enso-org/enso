//! The test suite of IDE-engine communication. The view is not instantiated, and controllers
//! may be used for convenience.

use enso_gui::integration_test::prelude::*;

use engine_protocol::language_server::ExplicitCall;
use engine_protocol::language_server::MethodPointer;
use engine_protocol::language_server::StackItem;
use enso_executor::setup_global_executor;
use enso_executor::web::EventLoopExecutor;
use enso_gui::controller::project::MAIN_DEFINITION_NAME;
use enso_web::sleep;
use std::time::Duration;
use wasm_bindgen_test::wasm_bindgen_test;



// =======================================
// === TestOnNewProjectControllersOnly ===
// =======================================

struct TestOnNewProjectControllersOnly {
    _ide:      controller::Ide,
    project:   model::Project,
    _executor: EventLoopExecutor,
}

impl TestOnNewProjectControllersOnly {
    async fn set_up() -> Self {
        let executor = setup_global_executor();
        let config = enso_gui::config::Startup::default();
        info!("Setting up the project.");
        let initializer = enso_gui::Initializer::new(config);
        let error_msg = "Couldn't open project.";
        let ide = initializer.initialize_ide_controller().await.expect(error_msg);
        ide.manage_projects().unwrap().create_new_project(None, None).await.unwrap();
        let project = ide.current_project().unwrap();
        Self { _ide: ide, project, _executor: executor }
    }
}



// =============
// === Tests ===
// =============

wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

// This test requires 2022.1.1-nightly.2022-04-26 or later version of Engine.
#[wasm_bindgen_test]
async fn getting_component_groups() {
    let test = TestOnNewProjectControllersOnly::set_up().await;
    let ls_json_connection = test.project.json_rpc();
    let main_module = test.project.main_module().to_string();
    let execution_ctx = ls_json_connection.create_execution_context().await.unwrap();
    let frame = StackItem::ExplicitCall(ExplicitCall {
        method_pointer:                   MethodPointer {
            module:          main_module.clone(),
            defined_on_type: main_module,
            name:            MAIN_DEFINITION_NAME.to_owned(),
        },
        this_argument_expression:         None,
        positional_arguments_expressions: vec![],
    });
    ls_json_connection.push_to_execution_context(&execution_ctx.context_id, &frame).await.unwrap();
    sleep(Duration::from_secs(15)).await;
    let groups = ls_json_connection.get_component_groups(&execution_ctx.context_id).await.unwrap();
    debug!("{groups:?}");
}
