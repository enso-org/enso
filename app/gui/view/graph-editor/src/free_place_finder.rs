//! The module containing an algorithm for searching free space for some point going at the specific
//! direction.
//!
//! This is used in Graph Editor to find unoccupied place for newly created node.

use crate::prelude::*;

use ordered_float::OrderedFloat;



// ====================
// === OccupiedArea ===
// ====================

/// The structure describing an occupied area.
///
/// All such areas are rectangles described by x and y ranges. The (x1, x2) and (y1, y2) pairs are
/// not sorted - if you want to get the lesser/greater one, use one of [`left`], [`right`], [`top`],
/// or [`bottom`] methods.
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct OccupiedArea {
    pub x1: f32,
    pub y1: f32,
    pub x2: f32,
    pub y2: f32,
}

impl OccupiedArea {
    /// Get x position of the left boundary.
    pub fn left(&self) -> f32 {
        min(self.x1, self.x2)
    }

    /// Get x position of the right boundary.
    pub fn right(&self) -> f32 {
        max(self.x1, self.x2)
    }

    /// Get y position of the top boundary.
    pub fn top(&self) -> f32 {
        max(self.y1, self.y2)
    }

    /// Get y position of the bottom boundary.
    pub fn bottom(&self) -> f32 {
        min(self.y1, self.y2)
    }

    /// Check if the rectangle contains given point.
    ///
    /// The boundaries are open - they are not considered occupied.
    pub fn contains(&self, point: Vector2) -> bool {
        (self.x1 - point.x) * (self.x2 - point.x) < 0.0
            && (self.y1 - point.y) * (self.y2 - point.y) < 0.0
    }

    /// Return the x position of the left or right boundary, depending on whether the direction
    /// points leftwards of rightwards respectively. Returns [`None`] if direction does not point
    /// leftwards nor rightwards.
    pub fn x_bound_following_direction(&self, direction: Vector2) -> Option<f32> {
        if direction.x > f32::EPSILON {
            Some(self.right())
        } else if direction.x < -f32::EPSILON {
            Some(self.left())
        } else {
            None
        }
    }

    /// Return the y position of the top or bottom boundary, depending on whether the direction
    /// points toward the top or bottom respectively. Returns [`None`] if direction does not point
    /// toward top nor bottom.
    pub fn y_bound_following_direction(&self, direction: Vector2) -> Option<f32> {
        if direction.y > f32::EPSILON {
            Some(self.top())
        } else if direction.y < -f32::EPSILON {
            Some(self.bottom())
        } else {
            None
        }
    }

    /// Return the point where the ray going from starting point along the `direction` vector will
    /// intersect with area boundaries, if there is exactly one such point, otherwise the result is
    /// unspecified.
    pub fn boundary_intersection(&self, starting_point: Vector2, direction: Vector2) -> Vector2 {
        let x_bound = self.x_bound_following_direction(direction);
        let dir_x_factor = x_bound.map(|x| (x - starting_point.x) / direction.x);
        let y_bound = self.y_bound_following_direction(direction);
        let dir_y_factor = y_bound.map(|y| (y - starting_point.y) / direction.y);
        let dir_factor = match (dir_x_factor, dir_y_factor) {
            (Some(x), Some(y)) => min(x, y),
            (Some(x), None) => x,
            (None, Some(y)) => y,
            _ => default(),
        };
        starting_point + direction * dir_factor
    }
}



// =======================
// === find_free_place ===
// =======================

/// With the list of occupied areas, return the first unoccupied point when going along the ray
/// starting from `starting_point` and parallel to `direction` vector.
///
/// Returns [`None`] if the `direction` does not go clearly at any direction (both `direction.x` and
/// `direction.y` are smaller than [`f32::EPSILON`]).
pub fn find_free_place(
    starting_point: Vector2,
    direction: Vector2,
    occupied: impl IntoIterator<Item = OccupiedArea>,
) -> Option<Vector2> {
    let valid_dir = direction.x.abs() > f32::EPSILON || direction.y.abs() > f32::EPSILON;
    valid_dir.as_some_from(move || {
        let sorted_areas = occupied.into_iter().sorted_by_key(|area| {
            let x = area.x_bound_following_direction(-direction).unwrap_or(0.0);
            let y = area.y_bound_following_direction(-direction).unwrap_or(0.0);
            OrderedFloat(x * direction.x + y * direction.y)
        });
        let mut current_point = starting_point;
        for area in sorted_areas {
            if area.contains(current_point) {
                current_point = area.boundary_intersection(current_point, direction)
            }
        }
        current_point
    })
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn zero_direction_is_not_allowed() {
        assert!(find_free_place(default(), Vector2(0.0, 0.0), vec![]).is_none());
    }

    #[derive(Clone, Debug)]
    struct Case {
        starting_point:  Vector2,
        direction:       Vector2,
        occupied:        Vec<OccupiedArea>,
        expected_result: Vector2,
    }

    impl Case {
        fn run(&self) {
            let occupied = self.occupied.iter().cloned();
            let result = find_free_place(self.starting_point, self.direction, occupied).unwrap();
            assert_eq!(result, self.expected_result, "Case {:?} gave wrong result.", self);
        }

        fn flip(&self, on_x: bool, on_y: bool) -> Self {
            let x_factor = if on_x { -1.0 } else { 1.0 };
            let y_factor = if on_y { -1.0 } else { 1.0 };
            let factor = Vector2(x_factor, y_factor);
            Case {
                starting_point:  self.starting_point.component_mul(&factor),
                direction:       self.starting_point.component_mul(&factor),
                occupied:        self
                    .occupied
                    .iter()
                    .map(|area| OccupiedArea {
                        x1: area.x1 * factor.x,
                        x2: area.x2 * factor.x,
                        y1: area.y1 * factor.y,
                        y2: area.y2 * factor.y,
                    })
                    .collect(),
                expected_result: self.expected_result.component_mul(&factor),
            }
        }

        fn run_each_flip(&self) {
            self.run();
            self.flip(true, false).run();
            self.flip(false, true).run();
            self.flip(true, true).run();
        }
    }

    #[test]
    fn already_in_unoccupied() {
        // s> - starting point moving right.
        // X  - occupied areas.
        //   +---+
        //   |XXX|
        // +-+---+-+
        // |X|   |X|
        // |X| s>|X|
        // |X|   |X|
        // +-+---+-+
        //   |XXX|
        //   +---+
        let case = Case {
            starting_point:  Vector2(1.0, -1.0),
            direction:       Vector2(1.0, 0.0),
            occupied:        vec![
                OccupiedArea { x1: 2.0, x2: 3.0, y1: 0.0, y2: -2.0 },
                OccupiedArea { x1: -1.0, x2: 0.0, y1: 0.0, y2: -2.0 },
                OccupiedArea { x1: 0.0, x2: 2.0, y1: 1.0, y2: 0.0 },
                OccupiedArea { x1: 0.0, x2: 2.0, y1: -2.0, y2: -3.0 },
            ],
            expected_result: Vector2(1.0, -1.0),
        };
        case.run_each_flip()
    }

    #[test]
    fn single_area() {
        let orthogonal = Case {
            starting_point:  Vector2(10.0, 10.0),
            direction:       Vector2(1.0, 0.0),
            occupied:        vec![OccupiedArea { x1: 0.0, x2: 20.0, y1: 15.0, y2: 5.0 }],
            expected_result: Vector2(20.0, 10.0),
        };
        let non_orthogonal = Case {
            direction: Vector2(18.0, 15.0),
            expected_result: Vector2(18.0, 15.0),
            ..orthogonal.clone()
        };
        orthogonal.run_each_flip();
        non_orthogonal.run_each_flip();
    }

    #[test]
    fn overlapping_areas() {
        let case = Case {
            starting_point:  Vector2(11.0, 0.0),
            direction:       Vector2(1.0, 0.0),
            occupied:        vec![
                OccupiedArea { x1: 10.0, x2: 20.0, y1: -1.0, y2: 1.0 },
                OccupiedArea { x1: 15.0, x2: 25.0, y1: -1.0, y2: 1.0 },
                OccupiedArea { x1: 20.0, x2: 30.0, y1: -1.0, y2: 0.0 },
                OccupiedArea { x1: 25.0, x2: 30.0, y1: -1.0, y2: 1.0 },
            ],
            expected_result: Vector2(25.0, 0.0),
        };
        let reversed_areas_list =
            Case { occupied: case.occupied.iter().rev().cloned().collect(), ..case.clone() };
        case.run_each_flip();
        reversed_areas_list.run_each_flip();
    }
}
