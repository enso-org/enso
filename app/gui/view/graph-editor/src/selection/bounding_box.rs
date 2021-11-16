//! Module that contains an implementation of a simple axis aligned bounding box.
use ensogl::prelude::*;



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
}


// === Bounding Box Tests ===

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_intersection() {
        let bb1 = BoundingBox::from_corners(Vector2::new(0.5, 0.5), Vector2::new(1.0, 1.0));
        let bb2 = BoundingBox::from_corners(Vector2::new(0.0, 0.0), Vector2::new(2.0, 2.0));
        assert!(bb1.intersects(&bb2));
        assert!(bb2.intersects(&bb1));

        let bb1 = BoundingBox::from_corners(Vector2::new(3.0, 3.0), Vector2::new(4.0, 4.0));
        let bb2 = BoundingBox::from_corners(Vector2::new(0.0, 0.0), Vector2::new(2.0, 2.0));
        assert!(!bb1.intersects(&bb2));
        assert!(!bb2.intersects(&bb1));

        let bb1 = BoundingBox::from_corners(Vector2::new(0.0, 0.0), Vector2::new(4.0, 4.0));
        let bb2 = BoundingBox::from_corners(Vector2::new(0.0, 0.0), Vector2::new(-2.0, -2.0));
        assert!(bb1.intersects(&bb2));
        assert!(bb2.intersects(&bb1));

        let bb1 = BoundingBox::from_corners(Vector2::new(0.0, 0.0), Vector2::new(4.0, 4.0));
        let bb2 = BoundingBox::from_corners(Vector2::new(2.0, 2.0), Vector2::new(200.0, 200.0));
        assert!(bb1.intersects(&bb2));
        assert!(bb2.intersects(&bb1));

        let bb1 = BoundingBox::from_corners(Vector2::new(-50.0, -50.0), Vector2::new(25.0, 25.0));
        let bb2 = BoundingBox::from_corners(Vector2::new(5.00, 50.0), Vector2::new(100.0, 100.0));
        assert!(!bb1.intersects(&bb2));
        assert!(!bb2.intersects(&bb1));
    }
}
