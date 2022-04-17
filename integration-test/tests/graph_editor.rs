// === Non-Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]

use enso_integration_test::prelude::*;

use approx::assert_abs_diff_eq;
use enso_frp::future::FutureEvent;
use enso_frp::io::mouse::PrimaryButton;
use enso_gui::view::graph_editor::component::node as node_view;
use enso_gui::view::graph_editor::component::node::test_utils::NodeModelExt;
use enso_gui::view::graph_editor::component::node::Expression;
use enso_gui::view::graph_editor::GraphEditor;
use enso_gui::view::graph_editor::Node;
use enso_gui::view::graph_editor::NodeId;
use enso_gui::view::graph_editor::NodeSource;
use enso_web::sleep;
use ensogl::display::navigation::navigator::ZoomEvent;
use ensogl::display::scene::test_utils::MouseExt;
use ensogl::display::Scene;
use ordered_float::OrderedFloat;
use std::time::Duration;



wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test]
async fn create_new_project_and_add_nodes() {
    let test = IntegrationTestOnNewProject::setup().await;
    let graph_editor = test.graph_editor();

    assert_eq!(graph_editor.nodes().all.len(), 2);
    let expect_node_added = graph_editor.node_added.next_event();
    graph_editor.add_node();
    let (added_node_id, source_node, _) = expect_node_added.expect();
    assert_eq!(source_node, None);
    assert_eq!(graph_editor.nodes().all.len(), 3);

    let added_node =
        graph_editor.nodes().get_cloned_ref(&added_node_id).expect("Added node is not added");
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

#[wasm_bindgen_test]
async fn adding_node_with_add_node_button() {
    const INITIAL_NODE_COUNT: usize = 2;
    let test = IntegrationTestOnNewProject::setup().await;
    let graph_editor = test.graph_editor();
    let scene = &test.ide.ensogl_app.display.default_scene;

    let InitialNodes { below: (_, bottom_node), .. } =
        InitialNodes::obtain_from_graph_editor(&graph_editor);
    let bottom_node_pos = bottom_node.position();

    // Node is created below the bottom-most one.
    let (first_node_id, node_source, first_node) =
        add_node_with_add_node_button(&graph_editor, "1 + 1").await;
    assert!(node_source.is_none());
    assert_eq!(graph_editor.nodes().all.len(), INITIAL_NODE_COUNT + 1);
    let node_position = first_node.position();
    assert!(
        first_node.position().y < bottom_node_pos.y,
        "Expected that {node_position}.y < {bottom_node_pos}.y"
    );

    // Selected node is used as a `source` node.
    graph_editor.nodes().deselect_all();
    graph_editor.nodes().select(first_node_id);
    let (_, node_source, _) = add_node_with_add_node_button(&graph_editor, "+ 1").await;
    assert_eq!(node_source, Some(NodeSource { node: first_node_id }));
    assert_eq!(graph_editor.nodes().all.len(), INITIAL_NODE_COUNT + 2);

    // If there is a free space, the new node is created in the center of screen.
    let camera = scene.layers.main.camera();
    camera.mod_position_xy(|pos| pos + Vector2(1000.0, 1000.0));
    wait_a_frame().await;
    graph_editor.nodes().deselect_all();
    let (node_id, node_source, _) = add_node_with_add_node_button(&graph_editor, "1").await;
    assert!(node_source.is_none());
    assert_eq!(graph_editor.nodes().all.len(), INITIAL_NODE_COUNT + 3);
    let node_position = graph_editor.model.get_node_position(node_id).expect(
        "Node was not
added",
    );
    let center_of_screen = scene.screen_to_scene_coordinates(Vector3::zeros());
    assert_abs_diff_eq!(node_position.x, center_of_screen.x, epsilon = 10.0);
    assert_abs_diff_eq!(node_position.y, center_of_screen.y, epsilon = 10.0);
}

#[wasm_bindgen_test]
async fn adding_node_by_clicking_on_the_output_port() {
    let test = IntegrationTestOnNewProject::setup().await;
    let graph_editor = test.graph_editor();
    let (node_1_id, _, node_1) = add_node_with_internal_api(&graph_editor, "1 + 1").await;

    let method = |editor: &GraphEditor| {
        let port = node_1.model().output_port_shape().expect("No output port");
        port.events.mouse_over.emit(());
        editor.start_node_creation_from_port();
    };
    let (_, source, node_2) = add_node(&graph_editor, "+ 1", method).await;

    assert_eq!(source.unwrap(), NodeSource { node: node_1_id });
    assert!(node_2.position().y < node_1.position().y);
}

#[wasm_bindgen_test]
async fn new_nodes_placement_with_nodes_selected() {
    let test = IntegrationTestOnNewProject::setup().await;
    let graph_editor = test.graph_editor();
    let InitialNodes { above: (node_1_id, node_1), below: (node_2_id, node_2) } =
        InitialNodes::obtain_from_graph_editor(&graph_editor);

    // Scenario 1. Creating a new node with one node selected.
    graph_editor.nodes().select(node_2_id);
    let (node_3_id, _, node_3) = add_node_with_add_node_button(&graph_editor, "+ 1").await;
    assert_eq!(
        node_3.position().x,
        node_2.position().x,
        "New node is not left-aligned to the selected one."
    );
    assert!(node_3.position().y < node_2.position().y, "New node is not below the selected one.");

    graph_editor.nodes().deselect_all();
    graph_editor.nodes().select(node_2_id);
    let (node_4_id, _, node_4) = add_node_with_shortcut(&graph_editor, "+ 1").await;
    assert_eq!(
        node_4.position().y,
        node_3.position().y,
        "New node is not vertically aligned to the previous one."
    );
    assert!(
        node_4.position().x < node_3.position().x,
        "New node is not to the left of the previous one."
    );

    graph_editor.remove_node(node_3_id);
    graph_editor.remove_node(node_4_id);
    graph_editor.nodes().deselect_all();

    // Scenario 2. Creating a new node with multiple nodes selected.
    node_1.set_position(Vector3(-100.0, 0.0, 0.0));
    wait_a_frame().await;
    graph_editor.nodes().select(node_1_id);
    graph_editor.nodes().select(node_2_id);

    let (.., node_5) = add_node_with_shortcut(&graph_editor, "+ 1").await;
    assert_eq!(
        node_5.position().x,
        node_1.position().x,
        "New node is not left-aligned to the first selected one."
    );
    assert!(node_5.position().y < node_2.position().y, "New node is not below the bottom node.");

    graph_editor.nodes().deselect_all();
    graph_editor.nodes().select(node_1_id);
    graph_editor.nodes().select(node_2_id);

    let (node_6_id, _, node_6) = add_node_with_shortcut(&graph_editor, "+ 1").await;
    assert_eq!(
        node_6.position().y,
        node_5.position().y,
        "New node is not vertically aligned to the previous one."
    );
    assert!(
        node_6.position().x < node_5.position().x,
        "New node is not to the left of the previous one."
    );

    // Scenario 3. Creating a new node with enabled visualization.
    graph_editor.nodes().deselect_all();
    graph_editor.nodes().select(node_6_id);
    let (node_7_id, _, node_7) = add_node_with_shortcut(&graph_editor, "+ 1").await;
    let pos_without_visualization = node_7.position().y;
    graph_editor.remove_node(node_7_id);
    graph_editor.nodes().deselect_all();
    graph_editor.nodes().select(node_6_id);
    node_6.enable_visualization();
    wait_a_frame().await;
    let (.., node_7) = add_node_with_shortcut(&graph_editor, "+ 1").await;
    assert!(
        node_7.position().y < pos_without_visualization,
        "New node is not below the visualization."
    );
}

async fn add_node(
    graph_editor: &GraphEditor,
    expression: &str,
    method: impl Fn(&GraphEditor),
) -> (NodeId, Option<NodeSource>, Node) {
    let node_added = graph_editor.node_added.next_event();
    method(graph_editor);
    let (node_id, source_node, _) = node_added.expect();
    let node = graph_editor.nodes().get_cloned_ref(&node_id).expect("Node was not added");
    node.set_expression(Expression::new_plain(expression));
    graph_editor.stop_editing();
    wait_a_frame().await;
    (node_id, source_node, node)
}

async fn add_node_with_internal_api(
    graph_editor: &GraphEditor,
    expression: &str,
) -> (NodeId, Option<NodeSource>, Node) {
    let method = |editor: &GraphEditor| editor.add_node();
    add_node(graph_editor, expression, method).await
}

async fn add_node_with_shortcut(
    graph_editor: &GraphEditor,
    expression: &str,
) -> (NodeId, Option<NodeSource>, Node) {
    let method = |editor: &GraphEditor| editor.start_node_creation();
    add_node(graph_editor, expression, method).await
}

async fn add_node_with_add_node_button(
    graph_editor: &GraphEditor,
    expression: &str,
) -> (NodeId, Option<NodeSource>, Node) {
    let add_node_button = &graph_editor.model.add_node_button;
    let method = |_: &GraphEditor| add_node_button.click();
    add_node(graph_editor, expression, method).await
}

#[wasm_bindgen_test]
async fn mouse_oriented_node_placement() {
    struct Case {
        scene:             Scene,
        graph_editor:      GraphEditor,
        source_node:       Node,
        mouse_position:    Vector2,
        expected_position: Vector2,
    }

    impl Case {
        fn run(&self) {
            self.check_tab_key();
            self.check_edge_drop();
        }

        fn check_searcher_opening_place(
            &self,
            added_node: FutureEvent<(NodeId, Option<NodeSource>, bool)>,
        ) {
            let (new_node_id, _, _) = added_node.expect();
            let new_node_pos =
                self.graph_editor.model.get_node_position(new_node_id).map(|v| v.xy());
            assert_eq!(new_node_pos, Some(self.expected_position));
            self.graph_editor.stop_editing();
            assert_eq!(self.graph_editor.nodes().all.len(), 2);
        }

        fn check_tab_key(&self) {
            self.scene.mouse.frp.position.emit(self.mouse_position);
            let added_node = self.graph_editor.node_added.next_event();
            self.graph_editor.start_node_creation();
            self.check_searcher_opening_place(added_node);
        }

        fn check_edge_drop(&self) {
            let port = self.source_node.view.model().output_port_shape().unwrap();
            port.events.emit_mouse_down(PrimaryButton);
            port.events.emit_mouse_up(PrimaryButton);
            self.scene.mouse.frp.position.emit(self.mouse_position);
            assert!(
                self.graph_editor.has_detached_edge.value(),
                "No detached edge after clicking port"
            );
            let added_node = self.graph_editor.node_added.next_event();
            self.scene.mouse.click_on_background();
            enso_web::simulate_sleep((enso_shortcuts::DOUBLE_EVENT_TIME_MS + 10.0) as f64);
            self.check_searcher_opening_place(added_node);
        }
    }

    let test = IntegrationTestOnNewProject::setup().await;
    let scene = &test.ide.ensogl_app.display.default_scene;
    let graph_editor = test.graph_editor();
    let gap_x = graph_editor.default_x_gap_between_nodes.value();
    let gap_y = graph_editor.default_y_gap_between_nodes.value();
    let min_spacing = graph_editor.min_x_spacing_for_new_nodes.value();

    let InitialNodes { above: (_, above), below: (_, below) } =
        InitialNodes::obtain_from_graph_editor(&graph_editor);

    let create_case =
        |source_node: &Node, mouse_position: Vector2, expected_position: Vector2| Case {
            scene: scene.clone_ref(),
            graph_editor: graph_editor.clone_ref(),
            source_node: source_node.clone_ref(),
            mouse_position,
            expected_position,
        };

    let far_away = below.position().xy() + Vector2(500.0, 500.0);
    let far_away_expect = far_away;
    create_case(&below, far_away, far_away_expect).run();

    let under_below = below.position().xy() + Vector2(30.0, -25.0);
    let under_below_expect = below.position().xy() + Vector2(0.0, -gap_y - node_view::HEIGHT);
    create_case(&below, under_below, under_below_expect).run();

    let under_above = above.position().xy() + Vector2(30.0, -25.0);
    let under_above_expect = Vector2(
        below.position().x - gap_x - min_spacing,
        above.position().y - gap_y - node_view::HEIGHT,
    );
    create_case(&above, under_above, under_above_expect).run();
}



// ====================
// === InitialNodes ===
// ====================

struct InitialNodes {
    above: (NodeId, Node),
    below: (NodeId, Node),
}

impl InitialNodes {
    fn obtain_from_graph_editor(graph_editor: &GraphEditor) -> Self {
        let nodes = graph_editor.nodes().all.entries();
        let mut sorted =
            nodes.into_iter().sorted_by_key(|(_, node)| OrderedFloat(node.position().y));
        match (sorted.next(), sorted.next()) {
            (Some(below), Some(above)) => Self { above, below },
            _ => panic!("Expected two nodes in initial Graph Editor"),
        }
    }
}
