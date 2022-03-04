//! Module that contains an implementation of a simple axis aligned bounding box.
use ensogl::prelude::*;

use nalgebra::clamp;



// ===================
// === BoundingBox ===
// ===================

/// Describes a 2D bounding box of an UI component.
#[derive(Clone, Copy, Default, Debug)]
pub struct BoundingBox {
    top:    f32,
    bottom: f32,
    left:   f32,
    right:  f32,
}

impl BoundingBox {
    /// Return a bounding box that spans the two points.
    pub fn from_corners(p1: Vector2, p2: Vector2) -> Self {
        let top = p1.y.max(p2.y);
        let bottom = p1.y.min(p2.y);
        let left = p1.x.min(p2.x);
        let right = p1.x.max(p2.x);
        BoundingBox { top, bottom, left, right }
    }

    /// Return a bounding box given by the position and size. The position interpreted as the
    /// top-right corner and size as the extend along the positive x and y-axis. Negative sizes
    /// are valid.
    pub fn from_position_and_size(position: Vector2, size: Vector2) -> Self {
        Self::from_corners(position, position + size)
    }

    /// Check whether the given `pos` lies within the bounding box.
    pub fn contains(&self, pos: Vector2) -> bool {
        self.contains_x(pos.x) && self.contains_y(pos.y)
    }

    fn contains_x(&self, x: f32) -> bool {
        x > self.left && x < self.right
    }

    fn contains_y(&self, y: f32) -> bool {
        y > self.bottom && y < self.top
    }

    /// Return the width of the bounding box.
    pub fn width(&self) -> f32 {
        self.right - self.left
    }

    /// Return the height of the bounding box.
    pub fn height(&self) -> f32 {
        self.top - self.bottom
    }

    /// Return whether the two bounding boxes have some area of overlap.
    pub fn intersects(&self, other: &BoundingBox) -> bool {
        let not_contained = (self.right < other.left)
            || (other.right < self.left)
            || (self.bottom > other.top)
            || (other.bottom > self.top);
        !not_contained
    }

    pub fn grow_x(&mut self, size: f32) {
        self.left -= size / 2.0;
        self.right += size / 2.0;
    }

    pub fn grow_y(&mut self, size: f32) {
        self.bottom -= size / 2.0;
        self.top += size / 2.0;
    }

    /// Return the x position of the left boundary.
    pub fn left(&self) -> f32 {
        self.left
    }

    /// Return the x position of the right boundary.
    pub fn right(&self) -> f32 {
        self.right
    }

    /// Return the y position of the top boundary.
    pub fn top(&self) -> f32 {
        self.top
    }

    /// Return the y position of the bottom boundary.
    pub fn bottom(&self) -> f32 {
        self.bottom
    }

    /// Calculates the squared norm of a vector between the point passed as argument, and a point
    /// in the bounding box that is nearest to the point passed as an argument.
    pub fn squared_distance_to_point(&self, point: Vector2) -> f32 {
        let x_of_nearest_point_in_bounding_box = clamp(point.x, self.left, self.right);
        let y_of_nearest_point_in_bounding_box = clamp(point.y, self.bottom, self.top);
        let nearest_point_in_bounding_box =
            Vector2(x_of_nearest_point_in_bounding_box, y_of_nearest_point_in_bounding_box);
        (nearest_point_in_bounding_box - point).norm_squared()
    }
}

impl PartialSemigroup<BoundingBox> for BoundingBox {
    /// Expand the boundaries to make them contain all points belonging to the bounding box passed
    /// as an argument.
    fn concat_mut(&mut self, other: BoundingBox) {
        self.left = min(self.left, other.left);
        self.right = max(self.right, other.right);
        self.bottom = min(self.bottom, other.bottom);
        self.top = max(self.top, other.top);
    }
}


// === Bounding Box Tests ===

#[cfg(test)]
mod tests {
    use super::*;

    use assert_approx_eq::assert_approx_eq;


    #[test]
    fn test_intersection() {
        assert_intersect(((0.5, 0.5), (1.0, 1.0)), ((0.0, 0.0), (2.0, 2.0)), true);
        assert_intersect(((3.0, 3.0), (4.0, 4.0)), ((0.0, 0.0), (2.0, 2.0)), false);
        assert_intersect(((0.0, 0.0), (4.0, 4.0)), ((0.0, 0.0), (-2.0, -2.0)), true);
        assert_intersect(((0.0, 0.0), (4.0, 4.0)), ((2.0, 2.0), (200.0, 200.0)), true);
        assert_intersect(((-50.0, -50.0), (25.0, 25.0)), ((5.00, 50.0), (100.0, 100.0)), false);
    }

    fn assert_intersect(bb1: RectangleCorners, bb2: RectangleCorners, expected_to_intersect: bool) {
        let bb1 = bounding_box_from_corners(bb1.0, bb1.1);
        let bb2 = bounding_box_from_corners(bb2.0, bb2.1);
        assert_eq!(bb1.intersects(&bb2), expected_to_intersect);
        assert_eq!(bb2.intersects(&bb1), expected_to_intersect);
    }

    #[test]
    fn test_concat() {
        assert_concat(((0.0, 0.0), (2.0, 3.0)), ((2.0, 3.0), (4.0, 5.0)), ((0.0, 0.0), (4.0, 5.0)));
        assert_concat(
            ((0.0, 0.0), (1.0, 1.0)),
            ((-1.0, -1.0), (0.5, 0.5)),
            ((-1.0, -1.0), (1.0, 1.0)),
        );
        assert_concat(((0.0, 0.0), (1.0, 1.0)), ((0.3, 0.3), (0.6, 0.6)), ((0.0, 0.0), (1.0, 1.0)));
    }

    fn bounding_box_from_corners(bottom_left: (f32, f32), top_right: (f32, f32)) -> BoundingBox {
        BoundingBox::from_corners(
            Vector2(bottom_left.0, bottom_left.1),
            Vector2(top_right.0, top_right.1),
        )
    }

    type RectangleCorners = ((f32, f32), (f32, f32));

    fn assert_concat(
        bb1: RectangleCorners,
        bb2: RectangleCorners,
        expected_result: RectangleCorners,
    ) {
        let bb1 = bounding_box_from_corners(bb1.0, bb1.1);
        let bb2 = bounding_box_from_corners(bb2.0, bb2.1);
        let result = bb1.concat(bb2);
        assert_eq!(((result.left, result.bottom), (result.right, result.top)), expected_result);
    }

    fn assert_squared_distance_to_point(
        bb: RectangleCorners,
        point: (f32, f32),
        expected_squared_distance: f32,
    ) {
        let bb = bounding_box_from_corners(bb.0, bb.1);
        let point = Vector2(point.0, point.1);
        assert_approx_eq!(bb.squared_distance_to_point(point), expected_squared_distance);
    }

    #[test]
    fn test_squared_distance_to_point() {
        assert_squared_distance_to_point(((-1.0, -1.0), (0.0, 0.0)), (3.0, 4.0), 5.0.pow(2.0));
        // Distance between a bounding box and a point inside it should be 0.
        assert_squared_distance_to_point(((0.0, 0.0), (1.0, 1.0)), (0.5, 0.5), 0.0);
        assert_squared_distance_to_point(((0.0, 0.0), (1.0, 1.0)), (3.0, 0.0), 2.0.pow(2.0));
        assert_squared_distance_to_point(((0.0, 0.0), (1.0, 1.0)), (-2.0, 0.0), 2.0.pow(2.0));
        assert_squared_distance_to_point(((0.0, 0.0), (1.0, 1.0)), (0.0, -2.0), 2.0.pow(2.0));
        assert_squared_distance_to_point(((0.0, 0.0), (1.0, 1.0)), (0.0, 3.0), 2.0.pow(2.0));
    }
}
