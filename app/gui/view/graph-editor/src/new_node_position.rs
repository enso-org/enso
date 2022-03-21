//! This module provides functions returning positions for newly created nodes.
//!
//! The returned positions are such that the new nodes will not overlap with existing ones.

pub mod free_place_finder;

use crate::prelude::*;

use crate::component::node;
use crate::new_node_position::free_place_finder::find_free_place;
use crate::new_node_position::free_place_finder::OccupiedArea;
use crate::selection::BoundingBox;
use crate::EdgeId;
use crate::GraphEditorModel;
use crate::Node;
use crate::NodeId;
use crate::WayOfCreatingNode;

use ensogl_hardcoded_theme as theme;



/// ============================
/// === New Node Positioning ===
/// ============================

/// Return a position for a newly created node. The position is calculated by establishing a
/// reference position and then aligning it to existing nodes.
///
/// The reference position is chosen from among:
///  - the position of a source node of the dropped edge (if available),
///  - the mouse position,
///  - the screen center.
/// The position is then aligned to either:
///  - the source node of the dropped edge (if available),
///  - the `selection` (if available),
///  - the node closest to the reference position (if available),
///  - not aligned.
/// The choice among the options described above is governed by the `way`.
///
/// To learn more about the align algorithm, see the docs of [`aligned_if_close_to_node`].
pub fn new_node_position(
    graph_editor: &GraphEditorModel,
    way: &WayOfCreatingNode,
    selection: Option<NodeId>,
    mouse_position: Vector2,
) -> Vector2 {
    use WayOfCreatingNode::*;
    let scene = graph_editor.scene();
    let origin = Vector2(0.0, 0.0);
    let screen_center = scene.screen_to_object_space(&graph_editor.display_object, origin);
    assert!(!screen_center.x.is_nan());
    assert!(!screen_center.y.is_nan());
    match way {
        AddNodeEvent => default(),
        StartCreationEvent | ClickingButton if selection.is_some() =>
            under(graph_editor, selection.unwrap()),
        StartCreationEvent => at_mouse_aligned_to_close_nodes(graph_editor, mouse_position),
        ClickingButton => on_ray(graph_editor, screen_center, Vector2(0.0, -1.0)).unwrap(),
        DroppingEdge { edge_id } =>
            at_mouse_aligned_to_source_node(graph_editor, *edge_id, mouse_position),
        StartCreationFromPortEvent { endpoint } => under(graph_editor, endpoint.node_id),
    }
}

/// Return a position for a newly created node. The position is calculated by taking the mouse
/// position and aligning it to the closest existing node if the mouse position is close enough to
/// the node.
///
/// To learn more about the align algorithm, see the docs of [`aligned_if_close_to_node`].
pub fn at_mouse_aligned_to_close_nodes(
    graph_editor: &GraphEditorModel,
    mouse_position: Vector2,
) -> Vector2 {
    let nearest_node = node_nearest_to_point(graph_editor, mouse_position);
    aligned_if_close_to_node(graph_editor, mouse_position, nearest_node)
}

/// Return a position for a newly created node. The position is calculated by taking the mouse
/// position and aligning it to the source node (the node at the source of the [`edge_id`] edge) if
/// the source node is close to the mouse position.
///
/// To learn more about the align algorithm, see the docs of [`aligned_if_close_to_node`].
pub fn at_mouse_aligned_to_source_node(
    graph_editor: &GraphEditorModel,
    edge_id: EdgeId,
    mouse_position: Vector2,
) -> Vector2 {
    let source_node_id = graph_editor.edge_source_node_id(edge_id);
    let source_node = source_node_id.and_then(|id| graph_editor.nodes.get_cloned_ref(&id));
    aligned_if_close_to_node(graph_editor, mouse_position, source_node)
}

/// Return a position for a newly created node, aligning it to the `alignment_node` if the proposed
/// position is close enough to it.
///
/// A point is close enough to a node if it is located in an alignment area around a node,
/// defined in the current theme ([`theme::graph_editor::alignment_area_around_node`]).
/// The alignment algorithm is described in the docs of [`under`].
///
/// In the picture below, the solid border represents the node, while the dashed border
/// represents the alignment area. The captions are used as the variables in the code.
/// ```text
/// ┌┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┐
/// ┆               ▲               ┆
/// ┆           top │               ┆
/// ┆               ▼               ┆
/// ┆ left  ┌───────────────┐ right ┆
/// ┆ ◀───▶ └───────────────┘ ◀───▶ ┆
/// ┆               ▲               ┆
/// ┆        bottom │               ┆
/// ┆               ▼               ┆
/// └┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┘
/// ```
pub fn aligned_if_close_to_node(
    graph_editor: &GraphEditorModel,
    proposed_position: Vector2,
    alignment_node: Option<Node>,
) -> Vector2 {
    let alignment_node = alignment_node.filter(|node| {
        use theme::graph_editor::alignment_area_around_node as alignment_area_style;
        let node_bounding_box = node.frp.bounding_box.value();
        let styles = &graph_editor.styles_frp;
        let left = styles.get_number_or(alignment_area_style::to_the_left_of_node, 0.0);
        let alignment_area_min_x = node_bounding_box.left() - left.value();
        let right = styles.get_number_or(alignment_area_style::to_the_right_of_node, 0.0);
        let alignment_area_max_x = node_bounding_box.right() + right.value();
        let top = styles.get_number_or(alignment_area_style::above_node, 0.0);
        let alignment_area_max_y = node_bounding_box.top() + top.value();
        let bottom = styles.get_number_or(alignment_area_style::below_node, 0.0);
        let alignment_area_min_y = node_bounding_box.bottom() - bottom.value();
        let alignment_area = BoundingBox::from_corners(
            Vector2(alignment_area_min_x, alignment_area_min_y),
            Vector2(alignment_area_max_x, alignment_area_max_y),
        );
        alignment_area.contains(proposed_position)
    });
    match alignment_node {
        Some(node) => under(graph_editor, node.id()),
        None => proposed_position,
    }
}

/// Return a position for a newly created node. Returns a position closely below the `node_id` node
/// if the position is available, or a first available point on a ray extending to the left of that
/// position.
///
/// Availability of a position is defined in the docs of [`on_ray`].
pub fn under(graph_editor: &GraphEditorModel, node_above: NodeId) -> Vector2 {
    let above_pos = graph_editor.node_position(node_above);
    let y_gap = graph_editor.frp.default_y_gap_between_nodes.value();
    let y_offset = y_gap + node::HEIGHT;
    let starting_point = above_pos - Vector2(0.0, y_offset);
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



/// ================================
/// === Private Helper Functions ===
/// ================================

/// Return a node nearest to the specified point.
///
/// The distance between a point and a node is the distance between the point and the node's
/// bounding box.
fn node_nearest_to_point(graph_editor: &GraphEditorModel, point: Vector2) -> Option<Node> {
    let mut min_distance_squared = f32::MAX;
    let mut nearest_node = None;
    let nodes = graph_editor.nodes.all.raw.borrow();
    for node in nodes.values() {
        let node_bounding_box = node.frp.bounding_box.value();
        let distance_squared = node_bounding_box.squared_distance_to_point(point);
        if distance_squared < min_distance_squared {
            min_distance_squared = distance_squared;
            nearest_node = Some(node.clone_ref());
        }
    }
    nearest_node
}
