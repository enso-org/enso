//! Utilities for simulating user interaction, to support testing and profiling.

// This module is not #[cfg(test)] because it is needed for batch-mode profiling,
// which is not just in testing builds.

use super::*;

use enso_frp::future::EventOutputExt;



// =====================
// === Node creation ===
// =====================

// FIXME: This functionality overlaps with the [`tests::add_node_by`] functions above.
//  I[KW] cannot currently refactor it, because integration tests aren't working on my machine.
//  (See: https://www.pivotaltracker.com/story/show/182278293)

/// Set up a new node created by the given function.
pub async fn add_node(
    graph_editor: &GraphEditor,
    expression: &str,
    method: impl Fn(&GraphEditor),
) -> (NodeId, Option<NodeSource>, Node) {
    let node_added = graph_editor.node_added.next_event();
    method(graph_editor);
    let (node_id, source_node, _) = node_added.expect();
    let node = graph_editor.nodes().get_cloned_ref(&node_id).expect("Node was not added");
    node.set_expression(node::Expression::new_plain(expression));
    graph_editor.stop_editing();
    (node_id, source_node, node)
}

/// Create a new node directly.
pub async fn add_node_with_internal_api(
    graph_editor: &GraphEditor,
    expression: &str,
) -> (NodeId, Option<NodeSource>, Node) {
    let method = |editor: &GraphEditor| editor.add_node();
    add_node(graph_editor, expression, method).await
}

/// Create a new node as if by the keyboard shortcut.
pub async fn add_node_with_shortcut(
    graph_editor: &GraphEditor,
    expression: &str,
) -> (NodeId, Option<NodeSource>, Node) {
    let method = |editor: &GraphEditor| editor.start_node_creation();
    add_node(graph_editor, expression, method).await
}

/// Create a new node as if by pressing the button.
pub async fn add_node_with_add_node_button(
    graph_editor: &GraphEditor,
    expression: &str,
) -> (NodeId, Option<NodeSource>, Node) {
    let add_node_button = &graph_editor.model.add_node_button;
    let method = |_: &GraphEditor| add_node_button.click();
    add_node(graph_editor, expression, method).await
}



// ====================
// === InitialNodes ===
// ====================

/// The nodes present in the default project template.
#[derive(Debug)]
pub struct InitialNodes {
    /// The node rendered higher on the page.
    pub above: (NodeId, Node),
    /// The node rendered lower on the page.
    pub below: (NodeId, Node),
}

impl InitialNodes {
    /// Find the initial nodes expected in a default project. Panics if the project state is not
    /// as expected.
    pub fn obtain_from_graph_editor(graph_editor: &GraphEditor) -> Self {
        let mut nodes = graph_editor.nodes().all.entries();
        let y = |n: &Node| n.position().y;
        nodes.sort_unstable_by(|(_, a), (_, b)| y(a).total_cmp(&y(b)));
        let two_nodes = "Expected two nodes in initial Graph Editor.";
        let mut nodes = nodes.drain(..);
        let above = nodes.next().expect(two_nodes);
        let below = nodes.next().expect(two_nodes);
        Self { above, below }
    }
}
