use enso_prelude::*;

use enso_gui::executor::test_utils::TestWithLocalPoolExecutor;
use enso_gui::initializer::setup_global_executor;
use enso_gui::Ide;
use wasm_bindgen_test::wasm_bindgen_test;


wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test]
async fn create_new_project_and_add_nodes() {
    let executor = setup_global_executor();
    let div = enso_web::create_div();
    div.set_id("root");
    enso_web::body().append_with_node_1(&div).expect("Failed to add root div element.");

    let initializer = enso_gui::ide::Initializer::new(default());
    let application = initializer.start().await.expect("Failed to initialize the application.");
}
