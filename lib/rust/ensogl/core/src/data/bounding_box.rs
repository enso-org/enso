//! Module that contains an implementation of a simple axis aligned bounding box.

use crate::prelude::*;

use nalgebra::clamp;



// ===================
// === BoundingBox ===
// ===================

/// Describes a 2D bounding box of an UI component.
///
/// As pictured below, the left edge of a bounding box has a smaller or equal x coordinate compared
/// to the right edge, and the bottom edge has a smaller or equal y coordinate compared to the top
/// edge.
/// ```text
///  ▲
///  ┆ y
///  ┆          top
///  ┆       ┌────────┐
///  ┆ left  │        │ right
///  ┆       └────────┘
///  ┆         bottom
///  ┆                        x
/// ┄+┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄▶
///  ┆
/// ```
#[derive(Clone, Copy, Default, Debug, PartialEq)]
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

    /// Constructor.
    pub fn from_position_and_size(position: Vector2, size: Vector2) -> Self {
        Self::from_corners(position, position + size)
    }

    /// Constructor.
    pub fn from_center_and_size(position: Vector2, size: Vector2) -> Self {
        Self::from_corners(position - size / 2.0, position + size / 2.0)
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

    /// Return whether the interiors of two bounding boxes (the bounded area without boundaries)
    /// have some area of overlap.
    pub fn interior_intersects(&self, other: &BoundingBox) -> bool {
        let not_contained = (self.right <= other.left)
            || (other.right <= self.left)
            || (self.bottom >= other.top)
            || (other.bottom >= self.top);
        !not_contained
    }

    /// Expand the bounding box in the x direction by the given amount.
    pub fn grow_x(&mut self, size: f32) {
        self.left -= size / 2.0;
        self.right += size / 2.0;
    }

    /// Expand the bounding box in the y direction by the given amount.
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

    /// Return the position of the point at this bounding box center.
    pub fn center(&self) -> Vector2 {
        Vector2((self.left + self.right) / 2.0, (self.top + self.bottom) / 2.0)
    }

    /// Calculates the squared norm of a vector between the point passed as an argument, and a
    /// point in the bounding box that is nearest to the point passed as an argument.
    ///
    /// If the point passed as an argument is inside the bounding box, returns 0.
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


    impl From<((f32, f32), (f32, f32))> for BoundingBox {
        fn from(corners: ((f32, f32), (f32, f32))) -> BoundingBox {
            let corner0 = corners.0;
            let corner1 = corners.1;
            BoundingBox::from_corners(Vector2(corner0.0, corner0.1), Vector2(corner1.0, corner1.1))
        }
    }

    macro_rules! assert_intersect {
        ( $bbox1:tt *? $bbox2:tt == $expected_result:literal ) => {
            let bbox1: BoundingBox = $bbox1.into();
            let bbox2: BoundingBox = $bbox2.into();
            assert_eq!(bbox1.intersects(&bbox2), $expected_result);
            assert_eq!(bbox2.intersects(&bbox1), $expected_result);
        };
    }

    #[test]
    fn test_intersection() {
        assert_intersect! { ((0.5, 0.5), (1.0, 1.0))  *?  ((0.0, 0.0), (  2.0,   2.0))  ==  true };
        assert_intersect! { ((3.0, 3.0), (4.0, 4.0))  *?  ((0.0, 0.0), (  2.0,   2.0))  ==  false };
        assert_intersect! { ((0.0, 0.0), (4.0, 4.0))  *?  ((0.0, 0.0), ( -2.0,  -2.0))  ==  true };
        assert_intersect! { ((0.0, 0.0), (4.0, 4.0))  *?  ((2.0, 2.0), (200.0, 200.0))  ==  true };
        assert_intersect! {
            ((-50.0, -50.0), (25.0, 25.0))  *?  ((5.00, 50.0), (100.0, 100.0))  ==  false
        };
    }

    macro_rules! assert_concat {
        ( $( $bbox1:tt + $bbox2:tt == $bbox3:tt ; )+ ) => {
            $( assert_concat!{$bbox1 + $bbox2 == $bbox3}; )+
        };

        ( $bbox1:tt + $bbox2:tt == $bbox3:tt ) => {
            let bbox1: BoundingBox = $bbox1.into();
            let bbox2: BoundingBox = $bbox2.into();
            let bbox3: BoundingBox = $bbox3.into();
            let result = bbox1.concat(bbox2);
            let assert_msg = format!(
                "Concat result was expected to be: {bbox3:?}, but got: {result:?} instead.");
            assert_eq!(result.left, bbox3.left, "{}", assert_msg);
            assert_eq!(result.right, bbox3.right, "{}", assert_msg);
            assert_eq!(result.top, bbox3.top, "{}", assert_msg);
            assert_eq!(result.bottom, bbox3.bottom, "{}", assert_msg);
        };
    }

    #[test]
    fn test_concat() {
        assert_concat! {
            ((0.0, 0.0), (2.0, 3.0)) + (( 2.0,  3.0), (4.0, 5.0)) == ((0.0,   0.0), (4.0, 5.0));
            ((0.0, 0.0), (1.0, 1.0)) + ((-1.0, -1.0), (0.5, 0.5)) == ((-1.0, -1.0), (1.0, 1.0));
            ((0.0, 0.0), (1.0, 1.0)) + (( 0.3,  0.3), (0.6, 0.6)) == ((0.0,   0.0), (1.0, 1.0));
        };
    }

    const SQUARED_DISTANCE_COMPARISON_PRECISION: f32 = 0.001;

    macro_rules! assert_squared_distance_to_point {
        ( $( $bbox:tt <-sq-> $point:tt =~ $expected_sq_distance:expr ; )+ ) => {
            $(
                assert_squared_distance_to_point!{ $bbox <-sq-> $point =~ $expected_sq_distance };
            )+
        };

        ($bbox:tt <-sq-> $point:tt =~ $expected_sq_distance:expr) => {
            let bbox: BoundingBox = $bbox.into();
            let point = Vector2($point.0, $point.1);
            let result = bbox.squared_distance_to_point(point);
            let result_deviation = (result - $expected_sq_distance).abs();
            let result_ok = result_deviation < SQUARED_DISTANCE_COMPARISON_PRECISION;
            let assert_msg = format!(
                "Squared distance between {bbox:?} and {point:?} \
                expected to approximately equal {}, \
                but got {result} instead.", $expected_sq_distance);
            assert!(result_ok, "{}", assert_msg);
        };
    }

    #[test]
    fn test_squared_distance_to_point() {
        assert_squared_distance_to_point! {
            ((-1.0, -1.0), (0.0, 0.0))  <-sq->  ( 3.0,  4.0)  =~  5.0.pow(2.0);
            // Distance between a bounding box and a point inside it should be 0.
            (( 0.0,  0.0), (1.0, 1.0))  <-sq->  ( 0.5,  0.5)  =~  0.0;
            (( 0.0,  0.0), (1.0, 1.0))  <-sq->  ( 3.0,  0.0)  =~  2.0.pow(2.0);
            (( 0.0,  0.0), (1.0, 1.0))  <-sq->  (-2.0,  0.0)  =~  2.0.pow(2.0);
            (( 0.0,  0.0), (1.0, 1.0))  <-sq->  ( 0.0, -2.0)  =~  2.0.pow(2.0);
            (( 0.0,  0.0), (1.0, 1.0))  <-sq->  ( 0.0,  3.0)  =~  2.0.pow(2.0);
        };
    }
}
