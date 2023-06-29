//! This module provides functions returning positions for newly created nodes.
//!
//! The returned positions are such that the new nodes will not overlap with existing ones.

use crate::prelude::*;

use crate::component::node;
use crate::new_node_position::free_place_finder::find_free_place;
use crate::new_node_position::free_place_finder::OccupiedArea;
use crate::selection::BoundingBox;
use crate::GraphEditorModel;
use crate::Node;
use crate::NodeId;
use crate::WayOfCreatingNode;

use ensogl_hardcoded_theme as theme;


// ==============
// === Export ===
// ==============

pub mod free_place_finder;



/// ============================
/// === New Node Positioning ===
/// ============================

/// Return a position for a newly created node. The position is calculated by establishing a
/// reference position and then aligning it to existing nodes.
///
/// **Note** The aligning nodes is currently disabled for nodes which were created under mouse
/// position (including dropping an edge), as it turned out to be confusing for users. It may be
/// brought back once the algorithm will be improved (the [`at_mouse_aligned_to_close_nodes`]
/// function)
///
/// The reference position is chosen from among:
///  - the position of a source node of the dropped edge (if available),
///  - the bottom-most selected node (if available),
///  - the mouse position,
///  - the screen center.
/// The position is then aligned to either:
///  - the source node of the dropped edge (if available),
///  - the selected nodes (if available),
///  - the node closest to the reference position (if available),
///  - not aligned.
/// The choice among the options described above is governed by the `way`.
///
/// The Magnet Alignment algorithm is used to calculate the final position in the following cases:
///  - When creating node with (+) button without nodes selected.
///  - When creating node at mouse cursor, but not under the source node.
///  - When the node is pushed left due to lack of space - only horizontally.
///
/// To learn more about the align algorithm, see the docs of [`aligned_if_close_to_node`].
pub fn new_node_position(
    graph_editor: &GraphEditorModel,
    way: &WayOfCreatingNode,
    mouse_position: Vector2,
) -> Vector2 {
    use WayOfCreatingNode::*;
    let scene = graph_editor.scene();
    let origin = Vector2(0.0, 0.0);
    let screen_center = scene.screen_to_object_space(&graph_editor.display_object, origin);
    let some_nodes_are_selected = !graph_editor.nodes.selected.is_empty();
    match way {
        AddNodeEvent => default(),
        StartCreationEvent | ClickingButton if some_nodes_are_selected =>
            under_selected_nodes(graph_editor),
        StartCreationEvent => mouse_position,
        ClickingButton => {
            let pos = on_ray(graph_editor, screen_center, Vector2(0.0, -1.0)).unwrap();
            magnet_alignment(graph_editor, pos, HorizontallyAndVertically)
        }
        DroppingEdge { .. } => mouse_position,
        StartCreationFromPortEvent { endpoint } => under(graph_editor, endpoint.node_id),
    }
}

/// Return a position for a newly created node closely below all selected nodes, or a zero vector
/// if no nodes are selected. The position is left-aligned to the first selected node, then moved
/// to the left to the first available position if the initial position is not available.
///
/// Availability of a position is defined in the docs of [`on_ray`].
pub fn under_selected_nodes(graph_editor: &GraphEditorModel) -> Vector2 {
    let first_selected_node = graph_editor.nodes.selected.first_cloned();
    let first_selected_node_x = match first_selected_node {
        None => return Vector2::zeros(),
        Some(node_id) => graph_editor.node_position(node_id).x,
    };
    let node_bbox_bottom = |node_id| graph_editor.node_bounding_box(node_id).bottom();
    let selected_nodes = graph_editor.nodes.selected.raw.borrow();
    let selection_bottom = selected_nodes.iter().map(node_bbox_bottom).reduce(min);
    let selection_bottom_or_zero = selection_bottom.unwrap_or_default();
    below_line_and_left_aligned(graph_editor, selection_bottom_or_zero, first_selected_node_x)
}

/// Return a position for a newly created node. Returns a position closely below the `node_id` node
/// if the position is available, or a first available point on a ray extending to the left of that
/// position.
///
/// Availability of a position is defined in the docs of [`on_ray`].
pub fn under(graph_editor: &GraphEditorModel, node_id: NodeId) -> Vector2 {
    let above_node_pos = graph_editor.node_position(node_id);
    let above_node_bbox = graph_editor.node_bounding_box(node_id);
    below_line_and_left_aligned(graph_editor, above_node_bbox.bottom(), above_node_pos.x)
}

/// Return a position for a newly created node. Returns a position closely below a horizontal line
/// at `line_y` and left-aligned to `align_x`, or a first available position to the left of it if
/// the initial position is not available.
///
/// "Closely below" means that a vertical gap is maintained between the line and the top border of
/// a node placed at the returned position. The vertical gap is equal to
/// [`theme::graph_editor::default_y_gap_between_nodes`].
/// Availability of a position is defined in the docs of [`on_ray`].
pub fn below_line_and_left_aligned(
    graph_editor: &GraphEditorModel,
    line_y: f32,
    align_x: f32,
) -> Vector2 {
    let y_gap = graph_editor.frp_public.output.default_y_gap_between_nodes.value();
    let y_offset = y_gap + node::HEIGHT / 2.0;
    let starting_point = Vector2(align_x, line_y - y_offset);
    let direction = Vector2(-1.0, 0.0);
    let pos = on_ray(graph_editor, starting_point, direction).unwrap();
    let was_pushed_left = pos.x != align_x;
    if was_pushed_left {
        magnet_alignment(graph_editor, pos, Horizontally)
    } else {
        pos
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
    node_id: NodeId,
    mouse_position: Vector2,
) -> Vector2 {
    let source_node = graph_editor.nodes.get_cloned_ref(&node_id);
    aligned_if_close_to_node(graph_editor, mouse_position, source_node)
}

/// Return a position for a newly created node, aligning it to the `alignment_node` if the proposed
/// position is close enough to it. Otherwise perform a Magnet Alignment.
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
        let node_bounding_box = node.bounding_box.value();
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
        None => magnet_alignment(graph_editor, proposed_position, HorizontallyAndVertically),
    }
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
    let x_gap = graph_editor.frp_public.output.default_x_gap_between_nodes.value();
    let y_gap = graph_editor.frp_public.output.default_y_gap_between_nodes.value();
    // This is how much horizontal space we are looking for.
    let min_spacing = graph_editor.frp_public.output.min_x_spacing_for_new_nodes.value();
    let nodes = graph_editor.nodes.all.raw.borrow();
    // The "occupied area" for given node consists of:
    // - area taken by node view (obviously);
    // - the minimum gap between nodes in all directions, so the new node won't be "glued" to
    //   another;
    // - the new node size measured from origin point at each direction accordingly: because
    //   `find_free_place` looks for free place for the origin point, and we want to fit not only
    //   the point, but the whole node.
    let node_areas = nodes.values().map(|node| {
        let bounding_box = node.bounding_box.value();
        let left = bounding_box.left() - x_gap - min_spacing;
        let right = bounding_box.right() + x_gap;
        let top = bounding_box.top() + node::HEIGHT / 2.0 + y_gap;
        let bottom = bounding_box.bottom() - node::HEIGHT / 2.0 - y_gap;
        OccupiedArea { x1: left, x2: right, y1: top, y2: bottom }
    });
    find_free_place(starting_point, direction, node_areas)
}



// ==================================
// === Magnet Alignment Algorithm ===
// ==================================

/// The maximum distance (in scene units) to the nearest grid line in Magnet Alignment algorithm.
///
/// This value defines the "sensitivity" of the Magnet Alignment for new nodes - greater values
/// lead to more "aggressive" snapping.
const MAGNET_ALIGNMENT_THRESHOLD: f32 = 150.0;

/// The direction in which the Magnet Alignment algorithm will be applied.
///
/// See [`magnet_alignment`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum MagnetAlignmentDirection {
    HorizontallyAndVertically,
    Horizontally,
}
use MagnetAlignmentDirection::*;

/// Perform a Magnet Alignment algorithm.
///
/// Magnet Alignment tries to place a node so that it is aligned to the positions of the other
/// nodes, creating a "grid" of nodes.
///
/// `direction` determines whether the Magnet Alignment is used for X-coordinate only or for both
/// X- and Y-coordinates.
fn magnet_alignment(
    graph_editor: &GraphEditorModel,
    position: Vector2,
    direction: MagnetAlignmentDirection,
) -> Vector2 {
    graph_editor.nodes.recompute_grid(default());
    let threshold = MAGNET_ALIGNMENT_THRESHOLD;
    let grid_alignment = graph_editor.nodes.check_grid_magnet_with_threshold(position, threshold);
    let x = grid_alignment.x.unwrap_or(position.x);
    let use_for_y = matches!(direction, HorizontallyAndVertically);
    let y = if use_for_y { grid_alignment.y.unwrap_or(position.y) } else { position.y };
    on_ray(graph_editor, Vector2(x, y), Vector2(0.0, -1.0)).unwrap()
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
        let node_bounding_box = node.bounding_box.value();
        let distance_squared = node_bounding_box.squared_distance_to_point(point);
        if distance_squared < min_distance_squared {
            min_distance_squared = distance_squared;
            nearest_node = Some(node.clone_ref());
        }
    }
    nearest_node
}
