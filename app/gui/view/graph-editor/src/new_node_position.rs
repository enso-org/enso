//! This module provides functions returning positions for newly created nodes.
//!
//! The returned positions are such that the new nodes will not overlap with existing ones.

pub mod free_place_finder;

use crate::prelude::*;

use crate::component::node;
use crate::new_node_position::free_place_finder::find_free_place;
use crate::new_node_position::free_place_finder::OccupiedArea;
use crate::GraphEditorModel;
use crate::NodeId;
use crate::WayOfCreatingNode;



/// ============================
/// === New Node Positioning ===
/// ============================

/// Return a position for a newly created node. The position is calculated by establishing a
/// reference position and then aligning it to existing nodes.
///
/// The reference position is chosen from among:
///  - the position of a source node of the dropped edge (if available),
///  - the bottom-most selected node (if available),
///  - the mouse position,
///  - the screen center.
/// The position is then aligned to either:
///  - the source node of the dropped edge (if available),
///  - the selected nodes (if available),
///  - not aligned.
/// The choice among the options described above is governed by the `way`.
///
/// To learn more about the align algorithm, see the docs of [`under`].
pub fn new_node_position(
    graph_editor: &GraphEditorModel,
    way: &WayOfCreatingNode,
    mouse_position: Vector2,
) -> Vector2 {
    use WayOfCreatingNode::*;
    let scene = graph_editor.scene();
    let origin = Vector2(0.0, 0.0);
    let screen_center = scene.screen_to_object_space(&graph_editor.display_object, origin);
    let some_nodes_selected = graph_editor.nodes.selected.len() > 0;
    match way {
        AddNodeEvent => default(),
        StartCreationEvent | ClickingButton if some_nodes_selected => under_selection(graph_editor),
        StartCreationEvent => mouse_position,
        ClickingButton => on_ray(graph_editor, screen_center, Vector2(0.0, -1.0)).unwrap(),
        DroppingEdge { .. } => mouse_position,
        StartCreationFromPortEvent { endpoint } => under(graph_editor, endpoint.node_id),
    }
}

pub fn under_selection(graph_editor: &GraphEditorModel) -> Vector2 {
    let selected_nodes = graph_editor.nodes.selected.raw.borrow();
    let mut selected_nodes_iter = selected_nodes.iter();
    // FIXME: try to avoid unwrap()
    let first_selected_node = selected_nodes_iter.next().unwrap();
    let first_selected_node_pos = graph_editor.node_position(first_selected_node);
    let first_selected_node_bbox = graph_editor.node_bounding_box(first_selected_node);
    let x = first_selected_node_pos.x;
    // TODO: .map().min()?
    let mut min_bbox_bottom = first_selected_node_bbox.bottom();
    for node_id in selected_nodes_iter {
        let node_bbox = graph_editor.node_bounding_box(node_id);
        min_bbox_bottom = min(min_bbox_bottom, node_bbox.bottom());
    }
    below_line_left_aligned(graph_editor, min_bbox_bottom, x)
}

/// Return a position for a newly created node. Returns a position closely below the `node_id` node
/// if the position is available, or a first available point on a ray extending to the left of that
/// position.
///
/// Availability of a position is defined in the docs of [`on_ray`].
pub fn under(graph_editor: &GraphEditorModel, node_id: NodeId) -> Vector2 {
    let above_node_pos = graph_editor.node_position(node_id);
    let above_node_bbox = graph_editor.node_bounding_box(node_id);
    below_line_left_aligned(graph_editor, above_node_bbox.bottom(), above_node_pos.x)
}

/// Return a position for a newly created node. Returns a position left-aligned to `align_x`
/// closely below a horizontal line at `line_y`, or a first available position to the left if the
/// initial position is not available.
///
/// "Closely below" means that a vertical gap is maintained between the line and the top border of
/// a node placed at the returned position. The vertical gap is equal to
/// [`theme::graph_editor::default_y_gap_between_nodes`].
/// Availability of a position is defined in the docs of [`on_ray`].
pub fn below_line_left_aligned(
    graph_editor: &GraphEditorModel,
    line_y: f32,
    align_x: f32,
) -> Vector2 {
    let y_gap = graph_editor.frp.default_y_gap_between_nodes.value();
    let y_offset = y_gap + node::HEIGHT / 2.0;
    let starting_point = Vector2(align_x, line_y - y_offset);
    let direction = Vector2(-1.0, 0.0);
    on_ray(graph_editor, starting_point, direction).unwrap()
}

/// Return a position for a newly created node. Return the first available position on a ray
/// extending from `starting_point` in the `direction`, or [`None`] if the magnitude of each
/// coordinate of `direction` is smaller than [`f32::EPSILON`].
///
/// ## Available position
///
/// An available position is a position such that a newly created [`Node`] with origin at this
/// position does not overlap existing nodes. A node is said to overlap another node if an "overlap
/// area" (represented by the dashed border in the picture below) around the former intersects with
/// the bounding box of the latter. A newly created node is assumed to have a fixed size
/// (represented by the solid border in the picture below). The captions in the picture below are
/// used as variables in the code.
/// ```text
/// ┌┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┐
/// ┆                 ▲                    ┆
/// ┆           y_gap │                    ┆
/// ┆                 ▼                    ┆
/// ┆       ┌──────────────────────┐       ┆
/// ┆       │                   ▲  │       ┆
/// ┆       │      node::HEIGHT │  │       ┆
/// ┆       │                   │  │       ┆
/// ┆       │                   │  │       ┆
/// ┆ x_gap │  min_spacing      │  │ x_gap ┆
/// ┆ ◀───▶ │◀──────────────────+─▶│ ◀───▶ ┆
/// ┆       │                   │  │       ┆
/// ┆       │                   ▼  │       ┆
/// ┆       └──────────────────────┘       ┆
/// ┆                 ▲                    ┆
/// ┆           y_gap │                    ┆
/// ┆                 ▼                    ┆
/// └┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┘
/// ```
pub fn on_ray(
    graph_editor: &GraphEditorModel,
    starting_point: Vector2,
    direction: Vector2,
) -> Option<Vector2> {
    let x_gap = graph_editor.frp.default_x_gap_between_nodes.value();
    let y_gap = graph_editor.frp.default_y_gap_between_nodes.value();
    // This is how much horizontal space we are looking for.
    let min_spacing = graph_editor.frp.min_x_spacing_for_new_nodes.value();
    let nodes = graph_editor.nodes.all.raw.borrow();
    // The "occupied area" for given node consists of:
    // - area taken by node view (obviously);
    // - the minimum gap between nodes in all directions, so the new node won't be "glued" to
    //   another;
    // - the new node size measured from origin point at each direction accordingly: because
    //   `find_free_place` looks for free place for the origin point, and we want to fit not only
    //   the point, but the whole node.
    let node_areas = nodes.values().map(|node| {
        let bounding_box = node.frp.bounding_box.value();
        let left = bounding_box.left() - x_gap - min_spacing;
        let right = bounding_box.right() + x_gap;
        let top = bounding_box.top() + node::HEIGHT / 2.0 + y_gap;
        let bottom = bounding_box.bottom() - node::HEIGHT / 2.0 - y_gap;
        OccupiedArea { x1: left, x2: right, y1: top, y2: bottom }
    });
    find_free_place(starting_point, direction, node_areas)
}
