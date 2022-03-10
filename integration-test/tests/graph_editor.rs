// === Non-Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]

use enso_integration_test::prelude::*;

use approx::assert_abs_diff_eq;
use enso_web::sleep;
use ensogl::display::navigation::navigator::ZoomEvent;
use std::time::Duration;



wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test]
async fn create_new_project_and_add_nodes() {
    let test = IntegrationTestOnNewProject::setup().await;
    let graph_editor = test.graph_editor();

    assert_eq!(graph_editor.model.nodes.all.len(), 2);
    let expect_node_added = graph_editor.node_added.next_event();
    graph_editor.add_node();
    let added_node_id = expect_node_added.expect();
    assert_eq!(graph_editor.model.nodes.all.len(), 3);

    let added_node =
        graph_editor.model.nodes.get_cloned_ref(&added_node_id).expect("Added node is not added");
    assert_eq!(added_node.view.expression.value().to_string(), "");
}

#[wasm_bindgen_test]
async fn debug_mode() {
    let test = IntegrationTestOnNewProject::setup().await;
    let project = test.project_view();
    let graph_editor = test.graph_editor();

    assert!(!graph_editor.debug_mode.value());

    // Turning On
    let expect_mode = project.debug_mode.next_event();
    let expect_popup_message = project.debug_mode_popup().label().show.next_event();
    project.enable_debug_mode();
    assert!(expect_mode.expect());
    let message = expect_popup_message.expect();
    assert!(
        message.contains("Debug Mode enabled"),
        "Message \"{}\" does not mention enabling Debug mode",
        message
    );
    assert!(
        message.contains(enso_gui::view::debug_mode_popup::DEBUG_MODE_SHORTCUT),
        "Message \"{}\" does not inform about shortcut to turn mode off",
        message
    );
    assert!(graph_editor.debug_mode.value());

    // Turning Off
    let expect_mode = project.debug_mode.next_event();
    let expect_popup_message = project.debug_mode_popup().label().show.next_event();
    project.disable_debug_mode();
    assert!(!expect_mode.expect());
    let message = expect_popup_message.expect();
    assert!(
        message.contains("Debug Mode disabled"),
        "Message \"{}\" does not mention disabling of debug mode",
        message
    );
    assert!(!graph_editor.debug_mode.value());
}

#[wasm_bindgen_test]
async fn zooming() {
    let test = IntegrationTestOnNewProject::setup().await;
    let project = test.project_view();
    let graph_editor = test.graph_editor();
    let camera = test.ide.ensogl_app.display.default_scene.layers.main.camera();
    let navigator = &graph_editor.model.navigator;

    let zoom_on_center = |amount: f32| ZoomEvent { focus: Vector2(0.0, 0.0), amount };
    let zoom_duration_ms = Duration::from_millis(1000);

    // Without debug mode
    navigator.emit_zoom_event(zoom_on_center(-1.0));
    sleep(zoom_duration_ms).await;
    assert_abs_diff_eq!(camera.zoom(), 1.0, epsilon = 0.001);
    navigator.emit_zoom_event(zoom_on_center(1.0));
    sleep(zoom_duration_ms).await;
    assert!(camera.zoom() < 1.0, "Camera zoom {} must be less than 1.0", camera.zoom());
    navigator.emit_zoom_event(zoom_on_center(-2.0));
    sleep(zoom_duration_ms).await;
    assert_abs_diff_eq!(camera.zoom(), 1.0, epsilon = 0.001);

    // With debug mode
    project.enable_debug_mode();
    navigator.emit_zoom_event(zoom_on_center(-1.0));
    sleep(zoom_duration_ms).await;
    assert!(camera.zoom() > 1.0, "Camera zoom {} must be greater than 1.0", camera.zoom());
    navigator.emit_zoom_event(zoom_on_center(5.0));
    sleep(zoom_duration_ms).await;
    assert!(camera.zoom() < 1.0, "Camera zoom {} must be less than 1.0", camera.zoom());
    navigator.emit_zoom_event(zoom_on_center(-5.0));
    sleep(zoom_duration_ms).await;
    assert!(camera.zoom() > 1.0, "Camera zoom {} must be greater than 1.0", camera.zoom());
}
