//! Edge layout calculation.
//!
//! # Corners
//!
//! ```text
//!   ────╮
//! ```
//!
//! The fundamental unit of edge layout is the [`Corner`]. A corner is a line segment attached to a
//! 90° arc. The length of the straight segment, the radius of the arc, and the orientation of the
//! shape may vary. Any shape of edge is built from corners.
//!
//! The shape of a corner can be fully-specified by two points: The horizontal end, and the vertical
//! end.
//!
//! In special cases, a corner may be *trivial*: It may have a radius of zero, in which case either
//! the horizontal or vertical end will not be in the usual orientation. The layout algorithm only
//! produces trivial corners when the source is directly in line with the target, or in some cases
//! when subdividing a corner (see [Partial edges] below).
//!
//! # Junction points
//!
//! ```text
//!              3
//!   1         /
//!    \    ╭─────╮
//!     ────╯\     \
//!           2     4
//! ```
//!
//! The layout algorithm doesn't directly place corners. The layout algorithm places a sequence of
//! junction points--coordinates where two horizontal corner ends or two vertical corner ends meet
//! (or just one corner end, at an end of an edge). A series of junction points, always alternating
//! horizontal/vertical, has a one-to-one relationship with a sequence of corners.
//!
//! # Partial edges
//!
//! Corners are sufficient to draw any complete edge; however, in order to split an edge into a
//! focused portion and an unfocused portion at an arbitrary location based on the mouse position,
//! we need to subdivide one of the corners of the edge.
//!
//! ```text
//!                  |\
//!                  | 3
//!                 /
//!               .'
//!  ..........-'
//!  \    \
//!   1    2 (split)
//! ```
//!
//! When the split position is on the straight segment of a corner, the corner can simply be split
//! into a corner with a shorter segment (2-3), and a trivial corner consisting only of a straight
//! segment (1-2).
//!
//! ```text
//!                  |\
//!                  | 4
//!                 /
//!               .'
//!  ..........-'  \
//!  \         \    3 (split)
//!   1         2
//! ```
//!
//! The difficult case is when the split position is on the arc. In this case, it is not possible to
//! draw the split using the same [`Rectangle`] shader that is used for everything else; a
//! specialized shape is used which supports drawing arbitrary-angle arcs. A trivial corner will
//! draw the straight line up to the beginning of the arc (1-2); arc shapes will draw the split arc
//! (2-3) and (3-4).

use super::*;

use std::f32::consts::FRAC_PI_2;
use std::f32::consts::TAU;



// =================
// === Constants ===
// =================

/// Constants affecting all layouts.
mod shared {
    /// Minimum height above the target the edge must approach it from.
    pub(super) const MIN_APPROACH_HEIGHT: f32 = 32.25;
    pub(super) const NODE_HEIGHT: f32 = crate::component::node::HEIGHT;
    pub(super) const NODE_CORNER_RADIUS: f32 = crate::component::node::CORNER_RADIUS;
    /// The preferred arc radius.
    pub(super) const RADIUS_BASE: f32 = 20.0;
    /// The maximum size in pixels of overdraw between edge segments. Prevents visible gaps.
    pub(super) const SEGMENT_OVERLAP: f32 = 0.5;
}
use shared::*;

/// Constants configuring the 1-corner layout.
mod single_corner {
    /// The y-allocation for the radius will be the full available height minus this value.
    pub(super) const RADIUS_Y_ADJUSTMENT: f32 = 29.0;
    /// The base x-allocation for the radius.
    pub(super) const RADIUS_X_BASE: f32 = super::RADIUS_BASE;
    /// Proportion (0-1) of extra x-distance allocated to the radius.
    pub(super) const RADIUS_X_FACTOR: f32 = 0.6;
    /// Distance for the line to continue under the node, to ensure that there isn't a gap.
    pub(super) const SOURCE_NODE_OVERLAP: f32 = 4.0;
    /// Minimum arc radius at which we offset the source end to exit normal to the node's curve.
    pub(super) const MINIMUM_TANGENT_EXIT_RADIUS: f32 = 2.0;
}

/// Constants configuring the 3-corner layouts.
mod three_corner {
    /// The maximum arc radius.
    pub(super) const RADIUS_MAX: f32 = super::RADIUS_BASE;
    pub(super) const BACKWARD_EDGE_ARROW_THRESHOLD: f32 = 15.0;
    /// The maximum radius reduction (from [`RADIUS_BASE`]) to allow when choosing whether to use
    /// the three-corner layout that doesn't use a backward corner.
    pub(super) const MAX_SQUEEZE: f32 = 2.0;
}



// ==============
// === Layout ===
// ==============

/// Determine the positions and shapes of all the components of the edge.
pub(super) fn layout(
    target: Vector2,
    source_size: Vector2,
    target_size: Vector2,
    source_attached: bool,
    target_attached: bool,
) -> Layout {
    let (junction_points, max_radius, target_attachment) =
        junction_points(target, source_size, target_size, source_attached, target_attached);
    let corners = corners(&junction_points, max_radius).collect_vec();
    let arrow = arrow(target, &junction_points);
    Layout { corners, arrow, target_attachment, source_size }
}



// =======================
// === Junction points ===
// =======================

/// Calculate the start and end positions of each 1-corner section composing an edge to the
/// given offset. Return the points, the maximum radius that should be used to draw the corners
/// connecting them, and the length of the target attachment bit.
fn junction_points(
    target: Vector2,
    source_size: Vector2,
    target_size: Vector2,
    source_attached: bool,
    target_attached: bool,
) -> (Vec<Vector2>, f32, Option<TargetAttachment>) {
    let source_half_width = source_size.x() / 2.0;
    let source_half_height = source_size.y() / 2.0;
    // The maximum x-distance from the source (our local coordinate origin) for the point where the
    // edge will begin.
    let source_max_x_offset = (source_half_width - NODE_CORNER_RADIUS).max(0.0);
    // The maximum y-length of the target-attachment segment. If the layout allows, the
    // target-attachment segment will fully exit the node before the first corner begins.
    let target_max_attachment_height =
        target_attached.then_some((NODE_HEIGHT - target_size.y) / 2.0);
    let attachment = target_max_attachment_height.map(|length| TargetAttachment {
        target: target + Vector2(0.0, NODE_HEIGHT / 2.0),
        length,
    });

    let target_well_below_source =
        target.y() + target_max_attachment_height.unwrap_or_default() <= -MIN_APPROACH_HEIGHT;
    let target_below_source = target.y() < -NODE_HEIGHT / 2.0;
    let target_beyond_source = target.x().abs() > source_max_x_offset;
    let horizontal_room_for_3_corners = target_beyond_source
        && target.x().abs() - source_max_x_offset
            >= 3.0 * (RADIUS_BASE - three_corner::MAX_SQUEEZE);
    if target_well_below_source || (target_below_source && !horizontal_room_for_3_corners) {
        use single_corner::*;
        // The edge can originate anywhere along the length of the node.
        let source_x = target.x().clamp(-source_max_x_offset, source_max_x_offset);
        let distance_x = max(target.x().abs() - source_half_width, 0.0);
        let radius_x = RADIUS_X_BASE + distance_x * RADIUS_X_FACTOR;
        // The minimum length of straight line there should be at the target end of the edge. This
        // is a fixed value, except it is reduced when the target is horizontally very close to the
        // edge of the source, so that very short edges are less sharp.
        let y_adjustment = min(
            target.x().abs() - source_half_width + RADIUS_Y_ADJUSTMENT / 2.0,
            RADIUS_Y_ADJUSTMENT,
        );
        let radius_y = max(target.y().abs() - y_adjustment, 0.0);
        let max_radius = min(radius_x, radius_y);
        // The radius the edge would have, if the arc portion were as large as possible.
        let natural_radius = min((target.x() - source_x).abs(), target.y().abs());
        let source_y = if natural_radius > MINIMUM_TANGENT_EXIT_RADIUS {
            // Offset the beginning of the edge so that it is normal to the curve of the source node
            // at the point that it exits the node.
            let radius = min(natural_radius, max_radius);
            let arc_origin_x = target.x().abs() - radius;
            let source_arc_origin = source_half_width - NODE_CORNER_RADIUS;
            let circle_offset = arc_origin_x - source_arc_origin;
            let intersection = circle_intersection(circle_offset, NODE_CORNER_RADIUS, radius);
            -(radius - intersection).abs()
        } else if source_attached {
            SOURCE_NODE_OVERLAP - source_half_height
        } else {
            source_half_height
        };
        let source = Vector2(source_x, source_y);
        // The target attachment will extend as far toward the edge of the node as it can without
        // rising above the source.
        let attachment_height = target_max_attachment_height.map(|dy| min(dy, target.y().abs()));
        let attachment_y = target.y() + attachment_height.unwrap_or_default();
        let target_attachment = Vector2(target.x(), attachment_y);
        (vec![source, target_attachment], max_radius, attachment)
    } else {
        use three_corner::*;
        // The edge originates from either side of the node.
        let source_x = source_max_x_offset.copysign(target.x());
        let distance_x = (target.x() - source_x).abs();
        let (j0_x, j1_x, height_adjustment);
        if horizontal_room_for_3_corners {
            //               J1
            //              /
            //            ╭──────╮
            // ╭─────╮    │      ▢
            // ╰─────╯────╯\
            //             J0
            // Junctions (J0, J1) are in between source and target.
            let j0_dx = min(2.0 * RADIUS_MAX, distance_x / 2.0);
            let j1_dx = min(RADIUS_MAX, (distance_x - j0_dx) / 2.0);
            j0_x = source_x + j0_dx.copysign(target.x());
            j1_x = j0_x + j1_dx.copysign(target.x());
            height_adjustment = RADIUS_MAX - j1_dx;
        } else {
            //            J1
            //           /
            //     ╭──────╮ J0
            //     ▢      │/
            // ╭─────╮    │
            // ╰─────╯────╯
            // J0 > source; J0 > J1; J1 > target.
            j1_x = target.x() + RADIUS_MAX.copysign(target.x());
            let j0_beyond_target = target.x().abs() + RADIUS_MAX * 2.0;
            let j0_beyond_source = source_x.abs() + RADIUS_MAX;
            j0_x = j0_beyond_source.max(j0_beyond_target).copysign(target.x());
            height_adjustment = 0.0;
        }
        let attachment_height = target_max_attachment_height.unwrap_or_default();
        let top =
            max(target.y() + MIN_APPROACH_HEIGHT + attachment_height - height_adjustment, 0.0);
        let source = Vector2(source_x, 0.0);
        let j0 = Vector2(j0_x, top / 2.0);
        let j1 = Vector2(j1_x, top);
        // The corners meet the target attachment at the top of the node.
        let attachment_target = attachment.map_or(target, |a| a.target);
        (vec![source, j0, j1, attachment_target], RADIUS_MAX, attachment)
    }
}



// ==================
// === End points ===
// ==================

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(super) enum EndPoint {
    Source,
    Target,
}



// =======================
// === Splitting edges ===
// =======================

#[derive(Debug, Copy, Clone, PartialEq)]
pub(super) struct EdgeSplit {
    pub corner_index: usize,
    pub closer_end:   EndPoint,
    pub split_corner: SplitCorner,
}

/// Find a point along the edge. Return the index of the corner the point occurs in, and which end
/// is closer to the point, and information about how the corner under the point has been split.
///
/// Returns [`None`] if the point is not on the edge.
pub(super) fn find_position(
    position: ParentCoords,
    layout: &Layout,
    source_height: f32,
    input_width: f32,
) -> Option<EdgeSplit> {
    let position = *position;
    let corners = &layout.corners;
    let corner_index = corners
        .iter()
        .position(|&corner| corner.bounding_box(input_width).contains_inclusive(position))?;
    let split_corner = corners[corner_index].split(position, input_width)?;
    let (full_corners, following_corners) = corners.split_at(corner_index);
    let full_corners_distance: f32 =
        full_corners.iter().map(|&corner| corner.rectilinear_length()).sum();
    let following_distance: f32 =
        following_corners.iter().map(|&corner| corner.rectilinear_length()).sum();
    let target_attachment_distance =
        layout.target_attachment.map(|bit| bit.length).unwrap_or_default();
    // The source end of the edge is on a horizontal line through the center of the source node
    // (this gives nice behavior when the edge exits the end at an angle). To accurately determine
    // which end a point appears closer to, we must exclude the portion of the edge that is hidden
    // under the source node.
    let hidden_source_distance = source_height / 2.0;
    let total_distance = full_corners_distance + following_distance - hidden_source_distance
        + target_attachment_distance;
    let offset_from_partial_corner = position - corners[corner_index].source_end();
    let partial_corner_distance =
        offset_from_partial_corner.x().abs() + offset_from_partial_corner.y().abs();
    let distance_from_source =
        full_corners_distance + partial_corner_distance - hidden_source_distance;
    let closer_end = match distance_from_source * 2.0 < total_distance {
        true => EndPoint::Source,
        false => EndPoint::Target,
    };
    Some(EdgeSplit { corner_index, closer_end, split_corner })
}



// ======================================
// === Connecting points with corners ===
// ======================================

fn corners(points: &[Vector2], max_radius: f32) -> impl Iterator<Item = Oriented<Corner>> + '_ {
    let mut next_direction = CornerDirection::HorizontalToVertical;
    points.array_windows().map(move |&[p0, p1]| {
        let direction = next_direction;
        next_direction = next_direction.reverse();
        let corner = match direction {
            CornerDirection::HorizontalToVertical =>
                Corner { horizontal: p0, vertical: p1, max_radius },
            CornerDirection::VerticalToHorizontal =>
                Corner { horizontal: p1, vertical: p0, max_radius },
        };
        Oriented::new(corner, direction)
    })
}



// ==============
// === Corner ===
// ==============

#[derive(Debug, Copy, Clone, PartialEq)]
pub(super) struct Corner {
    horizontal: Vector2,
    vertical:   Vector2,
    max_radius: f32,
}

impl Corner {
    #[inline]
    pub fn clip(self) -> Vector2 {
        let Corner { horizontal, vertical, .. } = self;
        let (dx, dy) = (vertical.x() - horizontal.x(), horizontal.y() - vertical.y());
        let (x_clip, y_clip) = (0.5f32.copysign(dx), 0.5f32.copysign(dy));
        Vector2(x_clip, y_clip)
    }

    /// Calculate vertical and horizontal line overlap to avoid visible gaps between segments.
    #[inline]
    fn overlap_padding(self, line_width: f32) -> Vector2 {
        let Corner { horizontal, vertical, .. } = self;
        let offset = (horizontal - vertical).abs();
        Vector2(
            SEGMENT_OVERLAP.min(offset.x() - line_width * 0.5).max(0.0),
            SEGMENT_OVERLAP.min(offset.y() - line_width * 0.5).max(0.0),
        )
    }

    /// Calculate origin offset caused by overlap padding.
    #[inline]
    fn overlap_offset(self, line_width: f32) -> Vector2 {
        let Corner { horizontal, vertical, .. } = self;
        let offset = horizontal - vertical;
        let pad = self.overlap_padding(line_width);
        // Position the overlap according to clip direction. For straight lines, the overlap is
        // centered on the line.
        let x = match () {
            _ if offset.x() < 0.0 => -pad.x(),
            _ if offset.y() == 0.0 => -0.5 * pad.x(),
            _ => 0.0,
        };
        let y = match () {
            _ if offset.y() > 0.0 => -pad.y(),
            _ if offset.x() == 0.0 => -0.5 * pad.y(),
            _ => 0.0,
        };
        Vector2(x, y)
    }

    #[inline]
    pub fn origin(self, line_width: f32) -> Vector2 {
        let Corner { horizontal, vertical, .. } = self;
        let offset = horizontal - vertical;
        let pad_offset = self.overlap_offset(line_width);
        let half_line_width_w = offset.y().abs().min(line_width / 2.0);
        let half_line_width_h = offset.x().abs().min(line_width / 2.0);
        let x = pad_offset.x() + (horizontal.x()).min(vertical.x() - half_line_width_w);
        let y = pad_offset.y() + (vertical.y()).min(horizontal.y() - half_line_width_h);
        Vector2(x, y)
    }

    #[inline]
    pub fn size(self, line_width: f32) -> Vector2 {
        let Corner { horizontal, vertical, .. } = self;
        let offset = (horizontal - vertical).abs();
        let pad = self.overlap_padding(line_width);
        let half_line_width_w = offset.y().min(line_width / 2.0);
        let half_line_width_h = offset.x().min(line_width / 2.0);
        let width = pad.x() + (offset.x() + half_line_width_w).max(half_line_width_w * 2.0);
        let height = pad.y() + (offset.y() + half_line_width_h).max(half_line_width_h * 2.0);
        Vector2(width, height)
    }

    #[inline]
    pub fn radius(self, line_width: f32) -> f32 {
        let Corner { horizontal, vertical, .. } = self;
        let offset = (horizontal - vertical).abs();
        let smaller_offset = offset.x().min(offset.y());
        let piecewise_limit = (smaller_offset * 2.0)
            .min(line_width)
            .max(smaller_offset + smaller_offset.min(line_width / 2.0));
        (self.max_radius + line_width / 2.0).min(piecewise_limit)
    }

    fn bounding_box(self, line_width: f32) -> BoundingBox {
        let origin = self.origin(line_width);
        let size = self.size(line_width);
        BoundingBox::from_position_and_size_unchecked(origin, size)
    }

    #[allow(unused)]
    fn euclidean_length(self) -> f32 {
        let Corner { horizontal, vertical, max_radius } = self;
        let offset = horizontal - vertical;
        let (dx, dy) = (offset.x().abs(), offset.y().abs());
        let radius = min(dx, dy).min(max_radius);
        let linear_x = dx - radius;
        let linear_y = dy - radius;
        let arc = FRAC_PI_2 * radius;
        arc + linear_x + linear_y
    }

    fn rectilinear_length(self) -> f32 {
        let Corner { horizontal, vertical, .. } = self;
        let offset = horizontal - vertical;
        offset.x().abs() + offset.y().abs()
    }

    #[allow(unused)]
    fn transpose(self) -> Self {
        let Corner { horizontal, vertical, max_radius } = self;
        Corner { horizontal: vertical.yx(), vertical: horizontal.yx(), max_radius }
    }

    fn vertical_end_angle(self) -> f32 {
        match self.vertical.x() > self.horizontal.x() {
            true => 0.0,
            false => std::f32::consts::PI.copysign(self.horizontal.y() - self.vertical.y()),
        }
    }

    fn horizontal_end_angle(self) -> f32 {
        FRAC_PI_2.copysign(self.horizontal.y() - self.vertical.y())
    }
}


// === Parameters for drawing the arc portion of a corner in two parts ===

#[derive(Debug, Copy, Clone, Default, PartialEq)]
pub(super) struct SplitArc {
    pub origin:           Vector2,
    pub radius:           f32,
    pub source_end_angle: f32,
    pub split_angle:      f32,
    pub target_end_angle: f32,
}



// ========================
// === Oriented corners ===
// ========================

#[derive(Debug, Copy, Clone, Deref, PartialEq)]
pub(super) struct Oriented<T> {
    #[deref]
    value:     T,
    direction: CornerDirection,
}

impl<T> Oriented<T> {
    fn new(value: T, direction: CornerDirection) -> Self {
        Self { value, direction }
    }
}

impl Oriented<Corner> {
    /// Split the shape at the given point, if the point is within the tolerance specified by
    /// `snap_line_width` of the shape.
    fn split(self, split_point: Vector2, snap_line_width: f32) -> Option<SplitCorner> {
        let Corner { horizontal, vertical, max_radius } = self.value;
        let hv_offset = horizontal - vertical;
        let (dx, dy) = (hv_offset.x().abs(), hv_offset.y().abs());
        let radius = min(dx, dy).min(max_radius);

        // Calculate closeness to the straight segments.
        let (linear_x, linear_y) = (dx - radius, dy - radius);
        let snap_distance = snap_line_width / 2.0;
        let y_along_vertical = (self.vertical.y() - split_point.y()).abs() < linear_y;
        let x_along_horizontal = (self.horizontal.x() - split_point.x()).abs() < linear_x;
        let y_near_horizontal = (self.horizontal.y() - split_point.y()).abs() <= snap_distance;
        let x_near_vertical = (self.vertical.x() - split_point.x()).abs() <= snap_distance;

        // Calculate closeness to the arc.
        // 1. Find the origin of the circle the arc is part of.
        // The corner of our bounding box that is immediately outside the arc.
        let point_outside_arc = Vector2(self.vertical.x(), self.horizontal.y());
        // The opposite corner of our bounding box, far inside the arc.
        // Used to find the direction from outside the arc to the origin of the arc's circle.
        let point_inside_arc = Vector2(self.horizontal.x(), self.vertical.y());
        let outside_to_inside = point_inside_arc - point_outside_arc;
        let outside_to_origin =
            Vector2(radius.copysign(outside_to_inside.x()), radius.copysign(outside_to_inside.y()));
        let origin = point_outside_arc + outside_to_origin;
        // 2. Check if the point is on the arc.
        let input_to_origin = split_point - origin;
        let distance_squared_from_origin =
            input_to_origin.x().powi(2) + input_to_origin.y().powi(2);
        let min_radius = radius - snap_distance;
        let max_radius = radius + snap_distance;
        let too_close = distance_squared_from_origin < min_radius.powi(2);
        let too_far = distance_squared_from_origin > max_radius.powi(2);
        let on_arc = !(too_close || too_far);

        if y_near_horizontal && x_along_horizontal {
            // The point is along the horizontal line. Snap its y-value, and draw a corner to it.
            let snapped = Vector2(split_point.x(), self.horizontal.y());
            let source_end = self.with_target_end(snapped);
            let target_end = self.with_source_end(snapped);
            Some(SplitCorner { source_end, target_end, split_arc: None })
        } else if x_near_vertical && y_along_vertical {
            // The point is along the vertical line. Snap its x-value, and draw a corner to it.
            let snapped = Vector2(self.vertical.x(), split_point.y());
            let source_end = self.with_target_end(snapped);
            let target_end = self.with_source_end(snapped);
            Some(SplitCorner { source_end, target_end, split_arc: None })
        } else if on_arc {
            // Find the input point's angle along the arc.
            let offset_from_origin = split_point - origin;
            let split_angle = offset_from_origin.y().atan2(offset_from_origin.x());
            // Split the arc on the angle.
            let arc_horizontal_end = origin - Vector2(0.0, radius.copysign(outside_to_inside.y()));
            let arc_vertical_end = origin - Vector2(radius.copysign(outside_to_inside.x()), 0.0);
            let (arc_begin, arc_end) = match self.direction {
                CornerDirection::HorizontalToVertical => (arc_horizontal_end, arc_vertical_end),
                CornerDirection::VerticalToHorizontal => (arc_vertical_end, arc_horizontal_end),
            };
            let source_end = self.with_target_end(arc_begin);
            let target_end = self.with_source_end(arc_end);
            let source_end_angle = self.source_end_angle();
            let target_end_angle = self.target_end_angle();
            let split_angle = self.clamp_to_arc(split_angle);
            let split =
                SplitArc { origin, radius, source_end_angle, split_angle, target_end_angle };
            Some(SplitCorner { source_end, target_end, split_arc: Some(split) })
        } else {
            None
        }
    }

    fn clamp_to_arc(self, c: f32) -> f32 {
        let a = self.horizontal_end_angle();
        let b = self.vertical_end_angle();
        let a_to_c = (c.rem_euclid(TAU) - a.rem_euclid(TAU)).abs();
        let b_to_c = (c.rem_euclid(TAU) - b.rem_euclid(TAU)).abs();
        let ac = min(a_to_c, TAU - a_to_c);
        let bc = min(b_to_c, TAU - b_to_c);
        let close_to_a = ac < FRAC_PI_2;
        let close_to_b = bc < FRAC_PI_2;
        // The angle is on the minor arc if it is close to both limits; otherwise, clamp it to
        // whichever is closer.
        if close_to_a && close_to_b {
            c
        } else if ac < bc {
            a
        } else {
            b
        }
    }

    fn source_end(self) -> Vector2 {
        match self.direction {
            CornerDirection::VerticalToHorizontal => self.value.vertical,
            CornerDirection::HorizontalToVertical => self.value.horizontal,
        }
    }

    #[allow(unused)]
    fn target_end(self) -> Vector2 {
        match self.direction {
            CornerDirection::VerticalToHorizontal => self.value.horizontal,
            CornerDirection::HorizontalToVertical => self.value.vertical,
        }
    }

    fn with_target_end(mut self, value: Vector2) -> Self {
        *(match self.direction {
            CornerDirection::VerticalToHorizontal => &mut self.value.horizontal,
            CornerDirection::HorizontalToVertical => &mut self.value.vertical,
        }) = value;
        self
    }

    fn with_source_end(mut self, value: Vector2) -> Self {
        *(match self.direction {
            CornerDirection::VerticalToHorizontal => &mut self.value.vertical,
            CornerDirection::HorizontalToVertical => &mut self.value.horizontal,
        }) = value;
        self
    }

    fn source_end_angle(self) -> f32 {
        match self.direction {
            CornerDirection::HorizontalToVertical => self.horizontal_end_angle(),
            CornerDirection::VerticalToHorizontal => self.vertical_end_angle(),
        }
    }

    fn target_end_angle(self) -> f32 {
        self.reverse().source_end_angle()
    }

    fn reverse(self) -> Self {
        let Self { value, direction } = self;
        let direction = direction.reverse();
        Self { value, direction }
    }
}


// === Corner direction ===

#[derive(Debug, Copy, Clone, PartialEq)]
pub(super) enum CornerDirection {
    HorizontalToVertical,
    VerticalToHorizontal,
}

impl CornerDirection {
    pub(super) fn reverse(self) -> Self {
        match self {
            CornerDirection::HorizontalToVertical => CornerDirection::VerticalToHorizontal,
            CornerDirection::VerticalToHorizontal => CornerDirection::HorizontalToVertical,
        }
    }
}


// === Split (oriented) corners ====

#[derive(Debug, Copy, Clone, PartialEq)]
pub(super) struct SplitCorner {
    pub source_end: Oriented<Corner>,
    pub target_end: Oriented<Corner>,
    pub split_arc:  Option<SplitArc>,
}



// ===========================
// === Backward-edge arrow ===
// ===========================

fn arrow(target_offset: Vector2, junction_points: &[Vector2]) -> Option<Vector2> {
    let three_corner_layout = junction_points.len() > 2;
    let long_backward_edge = target_offset.y() >= three_corner::BACKWARD_EDGE_ARROW_THRESHOLD;
    // The points are ordered from source end to destination, and are alternately horizontal
    // and vertical junctions. The arrow must be in a vertical part of the edge. Place it at
    // the first vertical junction.
    let arrow_origin = junction_points[1];
    (three_corner_layout && long_backward_edge).then_some(arrow_origin)
}



// =============================
// === Target-attachment bit ===
// =============================

/// The target-end of the edge, drawn on top of a node.
#[derive(Debug, Copy, Clone, PartialEq)]
pub(super) struct TargetAttachment {
    /// The target end.
    pub target: Vector2,
    /// How far to extend from the target.
    pub length: f32,
}



// ==================
// === Math Utils ===
// ==================

/// For the given radius of the first circle (`r1`), radius of the second circle (`r2`), and the
/// x-axis position of the second circle (`x`), computes the y-axis position of the second circle in
/// such a way, that the borders of the circle cross at the right angle. It also computes the angle
/// of the intersection. Please note, that the center of the first circle is in the origin.
///
/// ```text
///       r1
///      ◄───►                (1) x^2 + y^2 = r1^2 + r2^2
///    _____                  (1) => y = sqrt((r1^2 + r2^2)/x^2)
///  .'     `.
/// /   _.-"""B-._     ▲
/// | .'0┼    |   `.   │      angle1 = A-XY-0
/// \/   │    /     \  │ r2   angle2 = 0-XY-B
/// |`._ │__.'       | │      alpha  = B-XY-X_AXIS
/// |   A└───┼─      | ▼
/// |      (x,y)     |        tg(angle1) = y  / x
///  \              /         tg(angle2) = r1 / r2
///   `._        _.'          alpha      = PI - angle1 - angle2
///      `-....-'
/// ```
fn circle_intersection(x: f32, r1: f32, r2: f32) -> f32 {
    let x_norm = x.clamp(-r2, r1);
    (r1 * r1 + r2 * r2 - x_norm * x_norm).sqrt()
}
