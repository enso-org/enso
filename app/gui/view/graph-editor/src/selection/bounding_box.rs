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

    /// Expand the boundaries to make them contain all points belonging to the bounding box passed
    /// as an argument.
    pub fn grow_to_include(&mut self, other: &BoundingBox) {
        self.left = self.left.min(other.left);
        self.right = self.right.max(other.right);
        self.bottom = self.bottom.min(other.bottom);
        self.top = self.top.max(other.top);
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


// === Bounding Box Tests ===

#[cfg(test)]
mod tests {
    use super::*;

    use assert_approx_eq::assert_approx_eq;


    macro_rules! bounding_box_from_corners {
        (($x1:expr, $y1:expr), ($x2:expr, $y2:expr)) => {
            BoundingBox::from_corners(Vector2($x1, $y1), Vector2($x2, $y2))
        };
    }

    #[test]
    fn test_intersection() {
        let bb1 = bounding_box_from_corners!((0.5, 0.5), (1.0, 1.0));
        let bb2 = bounding_box_from_corners!((0.0, 0.0), (2.0, 2.0));
        assert!(bb1.intersects(&bb2));
        assert!(bb2.intersects(&bb1));

        let bb1 = bounding_box_from_corners!((3.0, 3.0), (4.0, 4.0));
        let bb2 = bounding_box_from_corners!((0.0, 0.0), (2.0, 2.0));
        assert!(!bb1.intersects(&bb2));
        assert!(!bb2.intersects(&bb1));

        let bb1 = bounding_box_from_corners!((0.0, 0.0), (4.0, 4.0));
        let bb2 = bounding_box_from_corners!((0.0, 0.0), (-2.0, -2.0));
        assert!(bb1.intersects(&bb2));
        assert!(bb2.intersects(&bb1));

        let bb1 = bounding_box_from_corners!((0.0, 0.0), (4.0, 4.0));
        let bb2 = bounding_box_from_corners!((2.0, 2.0), (200.0, 200.0));
        assert!(bb1.intersects(&bb2));
        assert!(bb2.intersects(&bb1));

        let bb1 = bounding_box_from_corners!((-50.0, -50.0), (25.0, 25.0));
        let bb2 = bounding_box_from_corners!((5.00, 50.0), (100.0, 100.0));
        assert!(!bb1.intersects(&bb2));
        assert!(!bb2.intersects(&bb1));
    }

    #[test]
    fn test_grow_to_include() {
        let mut bb1 = bounding_box_from_corners!((0.0, 0.0), (2.0, 3.0));
        let bb2 = bounding_box_from_corners!((2.0, 3.0), (4.0, 5.0));
        bb1.grow_to_include(&bb2);
        assert_eq!((bb1.left, bb1.bottom, bb1.right, bb1.top), (0.0, 0.0, 4.0, 5.0));

        let mut bb1 = bounding_box_from_corners!((0.0, 0.0), (1.0, 1.0));
        let bb2 = bounding_box_from_corners!((-1.0, -1.0), (0.5, 0.5));
        bb1.grow_to_include(&bb2);
        assert_eq!((bb1.left, bb1.bottom, bb1.right, bb1.top), (-1.0, -1.0, 1.0, 1.0));

        let mut bb1 = bounding_box_from_corners!((0.0, 0.0), (1.0, 1.0));
        let bb2 = bounding_box_from_corners!((0.3, 0.3), (0.6, 0.6));
        bb1.grow_to_include(&bb2);
        assert_eq!((bb1.left, bb1.bottom, bb1.right, bb1.top), (0.0, 0.0, 1.0, 1.0));
    }

    #[test]
    fn test_squared_distance_to_point() {
        let bb = bounding_box_from_corners!((-1.0, -1.0), (0.0, 0.0));
        let point = Vector2(3.0, 4.0);
        assert_approx_eq!(bb.squared_distance_to_point(point), 5.0.pow(2.0));

        // Distance between a bounding box and a point inside it should be 0.
        let bb = bounding_box_from_corners!((0.0, 0.0), (1.0, 1.0));
        let point = Vector2(0.5, 0.5);
        assert_approx_eq!(bb.squared_distance_to_point(point), 0.0);

        let bb = bounding_box_from_corners!((0.0, 0.0), (1.0, 1.0));
        let point = Vector2(3.0, 0.0);
        assert_approx_eq!(bb.squared_distance_to_point(point), 2.0.pow(2.0));

        let bb = bounding_box_from_corners!((0.0, 0.0), (1.0, 1.0));
        let point = Vector2(-2.0, 0.0);
        assert_approx_eq!(bb.squared_distance_to_point(point), 2.0.pow(2.0));

        let bb = bounding_box_from_corners!((0.0, 0.0), (1.0, 1.0));
        let point = Vector2(0.0, -2.0);
        assert_approx_eq!(bb.squared_distance_to_point(point), 2.0.pow(2.0));

        let bb = bounding_box_from_corners!((0.0, 0.0), (1.0, 1.0));
        let point = Vector2(0.0, 3.0);
        assert_approx_eq!(bb.squared_distance_to_point(point), 2.0.pow(2.0));
    }
}
