use enso_frp::future::EventOutputExt;
use enso_integration_test::IntegrationTest;
use wasm_bindgen_test::wasm_bindgen_test;


wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test]
async fn create_new_project_and_add_nodes() {
    let test = IntegrationTest::setup().await;
    let ide = &test.ide;
    let project = ide.presenter.view().project();
    let graph_editor = project.graph();
    let controller = ide.presenter.controller();
    let project_management =
        controller.manage_projects().expect("Cannot access Managing Project API");

    let expect_prompt = project.show_prompt.next_event();
    project_management.create_new_project(None).await.expect("Failed to create new project");
    expect_prompt.await;

    assert_eq!(graph_editor.model.nodes.all.len(), 2);
    let expect_node_added = graph_editor.node_added.next_event();
    graph_editor.add_node();
    let added_node_id = expect_node_added.expect();
    assert_eq!(graph_editor.model.nodes.all.len(), 3);

    let added_node =
        graph_editor.model.nodes.get_cloned_ref(&added_node_id).expect("Added node is not added");
    assert_eq!(added_node.view.expression.value().to_string(), "");
}
