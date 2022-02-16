use crate::prelude::*;
use ordered_float::OrderedFloat;

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct OccupiedArea {
    pub x1: f32,
    pub y1: f32,
    pub x2: f32,
    pub y2: f32,
}

impl OccupiedArea {
    fn left(&self) -> f32 {
        min(self.x1, self.x2)
    }
    fn right(&self) -> f32 {
        max(self.x1, self.x2)
    }
    fn top(&self) -> f32 {
        max(self.y1, self.y2)
    }
    fn bottom(&self) -> f32 {
        min(self.y1, self.y2)
    }

    pub fn contains(&self, point: Vector2) -> bool {
        (self.x1 - point.x) * (self.x2 - point.x) < 0.0
            && (self.y1 - point.y) * (self.y2 - point.y) < 0.0
    }

    pub fn x_bound_following_direction(&self, direction: Vector2) -> Option<f32> {
        if direction.x > f32::EPSILON {
            Some(self.right())
        } else if direction.x < -f32::EPSILON {
            Some(self.left())
        } else {
            None
        }
    }

    pub fn y_bound_following_direction(&self, direction: Vector2) -> Option<f32> {
        if direction.y > f32::EPSILON {
            Some(self.top())
        } else if direction.y < -f32::EPSILON {
            Some(self.bottom())
        } else {
            None
        }
    }

    pub fn boundary_intersection(&self, starting_point: Vector2, direction: Vector2) -> Vector2 {
        let x_bound = self.x_bound_following_direction(direction);
        let dir_x_factor = x_bound.map(|x| (x - starting_point.x) / direction.x);
        let y_bound = self.y_bound_following_direction(direction);
        let dir_y_factor = y_bound.map(|y| (y - starting_point.y) / direction.y);
        let dir_factor = match (dir_x_factor, dir_y_factor) {
            (Some(x), Some(y)) => min(x, y),
            (Some(x), None) => x,
            (None, Some(y)) => y,
            _ => panic!("Direction is too close to (0.0, 0.0)."),
        };
        starting_point + direction * dir_factor
    }
}

#[derive(Clone, Debug)]
pub struct FreePlaceFinder {
    starting_point: Vector2,
    direction:      Vector2,
    occupied:       Vec<OccupiedArea>,
}

impl FreePlaceFinder {
    pub fn new(
        starting_point: Vector2,
        direction: Vector2,
        occupied: impl IntoIterator<Item = OccupiedArea>,
    ) -> Option<Self> {
        let valid_dir = direction.x.abs() > f32::EPSILON || direction.y.abs() > f32::EPSILON;
        valid_dir.as_some_from(move || {
            let occupied = occupied
                .into_iter()
                .sorted_by_key(|area| {
                    let x = area.x_bound_following_direction(-direction).unwrap_or(0.0);
                    let y = area.y_bound_following_direction(-direction).unwrap_or(0.0);
                    OrderedFloat(x * direction.x + y * direction.y)
                })
                .collect();
            Self { starting_point, direction, occupied }
        })
    }


    pub fn find(&self) -> Vector2 {
        DEBUG!("OCCUPIED: {self.occupied:?}");
        let mut current_point = self.starting_point;
        for area in &self.occupied {
            if area.contains(current_point) {
                current_point = area.boundary_intersection(current_point, self.direction)
            }
        }
        current_point
    }
}


// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn zero_direction_is_not_allowed() {
        assert!(FreePlaceFinder::new(default(), Vector2(0.0, 0.0), vec![]).is_none());
    }

    fn scale(case: &FreePlaceFinder, factor: Vector2) -> FreePlaceFinder {
        let starting_point = case.starting_point.component_mul(&factor);
        let direction = case.direction.component_mul(&factor);
        let occupied = case.occupied.iter().map(|area| OccupiedArea {
            x1: area.x1 * factor.x,
            x2: area.x2 * factor.x,
            y1: area.y1 * factor.y,
            y2: area.y2 * factor.y,
        });
        FreePlaceFinder::new(starting_point, direction, occupied).unwrap()
    }

    fn check_each_flip(base_case: FreePlaceFinder, base_case_expected: Vector2) {
        let flips = [(-1.0, 1.0), (1.0, -1.0), (-1.0, 1.0)].into_iter();
        let flip_cases = flips
            .map(|(x, y)| {
                let factor = Vector2(x, y);
                let flip_case = scale(&base_case, Vector2(x, y));
                let expected = base_case_expected.component_mul(&factor);
                (flip_case, expected)
            })
            .collect_vec();
        let all_cases = std::iter::once((base_case, base_case_expected)).chain(flip_cases);
        for (case, expected) in all_cases {
            let result = case.find();
            assert_eq!(result, expected, "Case {:?} gave wrong result.", case);
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
        let starting_point = Vector2(1.0, -1.0);
        let direction = Vector2(1.0, 0.0);
        let occupied = [
            OccupiedArea { x1: 2.0, x2: 3.0, y1: 0.0, y2: -2.0 },
            OccupiedArea { x1: -1.0, x2: 0.0, y1: 0.0, y2: -2.0 },
            OccupiedArea { x1: 0.0, x2: 2.0, y1: 1.0, y2: 0.0 },
            OccupiedArea { x1: 0.0, x2: 2.0, y1: -2.0, y2: -3.0 },
        ];
        let finder = FreePlaceFinder::new(starting_point, direction, occupied).unwrap();
        let expected = Vector2(1.0, -1.0);
        check_each_flip(finder, expected)
    }

    #[test]
    fn single_area() {
        let starting_point = Vector2(10.0, 10.0);
        let occupied = [OccupiedArea { x1: 0.0, x2: 20.0, y1: 15.0, y2: 5.0 }];
        let orthogonal =
            FreePlaceFinder::new(starting_point, Vector2(1.0, 0.0), occupied.clone()).unwrap();
        let non_orthogonal =
            FreePlaceFinder::new(starting_point, Vector2(1.0, 0.625), occupied).unwrap();
        check_each_flip(orthogonal, Vector2(20.0, 10.0));
        check_each_flip(non_orthogonal, Vector2(18.0, 15.0));
    }

    #[test]
    fn overlapping_areas() {
        let starting_point = Vector2(11.0, 0.0);
        let direction = Vector2(1.0, 0.0);
        let occupied = [
            OccupiedArea { x1: 10.0, x2: 20.0, y1: -1.0, y2: 1.0 },
            OccupiedArea { x1: 15.0, x2: 25.0, y1: -1.0, y2: 1.0 },
            OccupiedArea { x1: 20.0, x2: 30.0, y1: -1.0, y2: 0.0 },
            OccupiedArea { x1: 25.0, x2: 30.0, y1: -1.0, y2: 1.0 },
        ];
        let occupied_rev = occupied.iter().rev().cloned().collect_vec();
        let base = FreePlaceFinder::new(starting_point, direction, occupied).unwrap();
        let reversed = FreePlaceFinder::new(starting_point, direction, occupied_rev).unwrap();
        let expected = Vector2(25.0, 0.0);
        check_each_flip(base, expected);
        check_each_flip(reversed, expected);
    }
}
