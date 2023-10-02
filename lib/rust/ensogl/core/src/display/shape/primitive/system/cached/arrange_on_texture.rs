//! A module containing the cached shapes pack algorithm.
//!
//! The main entry point of the algorithm is [`arrange_shapes_on_texture`] function.

use crate::prelude::*;

use crate::data::bounding_box::BoundingBox;

use itertools::iproduct;
use ordered_float::OrderedFloat;



// =================
// === Constants ===
// =================

/// A parameter of [`arrange_shapes_on_texture`] algorithm: the factor how much the texture size is
/// increased when the current size is insufficient.
const TEXTURE_SIZE_MULTIPLIER: i32 = 2;



// =========================
// === Helper Structures ===
// =========================

// === ShapeWithSize ===

/// A shape paired with information about its size in the texture.
#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Default)]
pub struct ShapeWithSize<Shape> {
    pub shape: Shape,
    pub size:  Vector2,
}


// === ShapeWithPosition ===

/// A shape paired with information about its position in the texture.
#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Default)]
pub struct ShapeWithPosition<Shape> {
    pub shape:    Shape,
    pub position: Vector2,
}

impl<Shape> ShapeWithPosition<Shape> {
    /// Change the shapes' position
    pub fn map_position(self, f: impl FnOnce(Vector2) -> Vector2) -> Self {
        Self { shape: self.shape, position: f(self.position) }
    }
}


// === ArrangedShapes ===

/// The results of [`arrange_shapes_on_texture`] function.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct ArrangedShapes<Shape> {
    pub texture_size:    Vector2<i32>,
    pub shape_positions: Vec<ShapeWithPosition<Shape>>,
}



// =================
// === The Logic ===
// =================

/// Arrange Cached Shapes on texture.
///
/// The function takes the definition of cached shapes and returns the created
/// [shape views](AnyShapeView) with positions and sizes already set to proper place in the texture.
///
/// # The Texture Pack Algorithm
///
/// The algorithm is a simple heuristic: we sort shapes from the biggest to the smallest, and take
/// the shapes one by one by that order and scans possible left-bottom corner positions, starting
/// from left-bottom corner of the texture and going row by row, from left to right and from bottom
/// to top, picking the first position where the current shape does not collide with any already
/// placed one. According to
/// https://gamedev.stackexchange.com/questions/2829/texture-packing-algorithm this is actually one
/// of the best packing algorithm in terms of texture space.
///
/// As the above algorithm assumes some texture size, we starts with predefined square texture with
/// [`INITIAL_TEXTURE_SIZE`] and - if the shapes does not fit it - we repeatedly increase the size
/// and try again.
///
/// ## Example
///
/// Assuming initial texture size 5x5 and having the following shapes:
///
/// ```text
/// ┌───┐ ┌───┐ ┌───────┐ ┌───┐ ┌───────────┐
/// │1x1│ │1x1│ │2x2    │ │1x2│ │3x3        │
/// └───┘ └───┘ │       │ │   │ │           │
///             │       │ │   │ │           │
///             └───────┘ └───┘ │           │
///                             │           │
///                             └───────────┘
/// ```
///
/// this function will arrange it this way:
/// ```text
/// ┌───────────────────┐
/// │                   │
/// ├───┐       ┌───┐   │
/// │1x1│       │1x2│   │
/// ├───┴───────┤   ├───┤
/// │3x3        │   │1x1│
/// │           ├───┴───┤
/// │           │2x2    │
/// │           │       │
/// └───────────┴───────┘
/// ```
/// And return the texture size 5x4.
///
/// # Implementation
///
/// As the new shape is always "glued" to
/// - right boundary of another shape or left boundary of the texture
/// - top boundary of another shape or bottom boundary of the texture
/// We do not need to iterate over every row and column, only over those containing the
/// boundaries.
///
/// Therefore, this simple implementation has the time complexity of O(n^3) where n is number of
/// placed shapes.
pub fn arrange_shapes_on_texture<Shape>(
    shapes: impl IntoIterator<Item = ShapeWithSize<Shape>>,
    initial_texture_size: i32,
) -> ArrangedShapes<Shape>
where
    Shape: Clone,
{
    let sorted_shapes = shapes
        .into_iter()
        .sorted_by_key(|shape| OrderedFloat(shape.size.x * shape.size.y))
        .rev()
        .collect_vec();
    let mut tex_size = initial_texture_size;
    loop {
        if let Some(arranged) = try_to_fit_shapes_on_texture(&sorted_shapes, tex_size) {
            break arranged;
        }
        tex_size *= 2;
    }
}


// === try_to_fit_shapes_on_texture ===

/// Try to fit shapes on texture assuming maximum texture size. Returns [`None`] if was unable to
/// do it.
fn try_to_fit_shapes_on_texture<Shape>(
    sorted_shapes: &[ShapeWithSize<Shape>],
    max_texture_size: i32,
) -> Option<ArrangedShapes<Shape>>
where
    Shape: Clone,
{
    let mut placed_so_far: Vec<BoundingBox> = vec![];
    // First, we compute the shapes position with origin placed in left-bottom corner. This way
    // we don't need to know the eventual texture size upfront.
    let shapes_with_pos_left_bottom_origin_iter =
        sorted_shapes.iter().map(|ShapeWithSize { shape, size }| {
            find_free_place(*size, max_texture_size, &placed_so_far).map(|bbox| {
                placed_so_far.push(bbox);
                ShapeWithPosition { shape: shape.clone(), position: bbox.center() }
            })
        });
    // We collect all positions, so `placed_so_far` will contain all shapes and we can compute
    // texture size.
    let shapes_with_pos_left_bottom_origin: Option<Vec<_>> =
        shapes_with_pos_left_bottom_origin_iter.collect();
    let texture_width =
        placed_so_far.iter().map(BoundingBox::right).reduce(f32::max).unwrap_or_default();
    let texture_height =
        placed_so_far.iter().map(BoundingBox::top).reduce(f32::max).unwrap_or_default();
    let texture_origin = Vector2(texture_width, texture_height) / 2.0;
    shapes_with_pos_left_bottom_origin.map(|shapes_with_pos| ArrangedShapes {
        shape_positions: shapes_with_pos
            .into_iter()
            .map(|shape| shape.map_position(|pos| pos - texture_origin))
            .collect(),
        texture_size:    Vector2(texture_width as i32, texture_height as i32),
    })
}


// === find_free_place ===

/// For shape of given size, find the unoccupied space in the square texture.
///
/// A step of the [`arrange_shapes_on_texture`] algorithm. See its docs for details.
fn find_free_place(
    shape_size: Vector2,
    tex_size: i32,
    placed_so_far: &[BoundingBox],
) -> Option<BoundingBox> {
    let tex_size = tex_size as f32;
    // There is no need of iterating over all columns and rows, as the new shape is always "glued"
    // to right/top boundary of another shape, or left/bottom boundary of the texture.
    let right_bounds = placed_so_far.iter().map(|bbox| bbox.right().ceil());
    let allowed_right_bounds = right_bounds.filter(|col| col + shape_size.x <= tex_size);
    let top_bounds = placed_so_far.iter().map(|bbox| bbox.top().ceil());
    let allowed_top_bounds = top_bounds.filter(|row| row + shape_size.y <= tex_size);
    let candidate_rows =
        iter::once(0.0).chain(allowed_top_bounds).sorted_by_key(|&x| OrderedFloat(x));
    let candidate_cols =
        iter::once(0.0).chain(allowed_right_bounds).sorted_by_key(|&x| OrderedFloat(x));
    let candidate_positions = iproduct!(candidate_rows, candidate_cols);
    let mut candidate_bboxes = candidate_positions
        .map(|(y, x)| BoundingBox::from_bottom_left_position_and_size(Vector2(x, y), shape_size));
    candidate_bboxes.find(|bbox| {
        let is_collision = placed_so_far.iter().any(|placed| placed.interior_intersects(bbox));
        !is_collision
    })
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use crate::display::shape::system::cached::INITIAL_TEXTURE_SIZE;

    type MockShape = usize;

    fn shape_entries_from_sizes(
        sizes: impl IntoIterator<Item = (f32, f32)>,
    ) -> Vec<ShapeWithSize<MockShape>> {
        sizes
            .into_iter()
            .enumerate()
            .map(|(index, (width, height))| ShapeWithSize {
                size:  Vector2(width, height),
                shape: index,
            })
            .collect_vec()
    }


    #[test]
    fn texture_size() {
        fn run_case(shape_sizes: impl IntoIterator<Item = (f32, f32)>, expected_size: (i32, i32)) {
            let shape_entries = shape_entries_from_sizes(shape_sizes);
            let result = arrange_shapes_on_texture(shape_entries.into_iter(), INITIAL_TEXTURE_SIZE);
            assert_eq!(result.texture_size, IntoVector2::into_vector(expected_size));
        }

        run_case([(32.0, 32.0), (16.0, 16.0)], (48, 32));
        run_case([(16.0, 2.0), (2.0, 20.0)], (18, 20));
        run_case([(256.0, 2.0), (257.0, 2.0)], (257, 4));
        run_case(iter::repeat((32.0, 32.0)).take(2), (64, 32));
        run_case(iter::repeat((32.0, 32.0)).take(16), (512, 32));
        run_case(iter::repeat((32.0, 32.0)).take(17), (512, 64));
        run_case(iter::repeat((64.0, 64.0)).take(64), (512, 512));
        // Shapes does not fit initial texture size: the texture is extended.
        run_case(iter::repeat((64.0, 64.0)).take(65), (1024, 320));
        // This will extend the texture several times.
        run_case(iter::repeat((512.0, 512.0)).take(17), (4096, 1536));
    }

    #[test]
    fn fitting_shapes_on_texture() {
        fn run_case<const N: usize>(
            tex_size: i32,
            sizes: [(f32, f32); N],
            expected_position: Option<[(f32, f32); N]>,
        ) {
            let shape_entries = shape_entries_from_sizes(sizes);
            let result = try_to_fit_shapes_on_texture(&shape_entries, tex_size);
            let positions = result.map(|shapes| {
                shapes
                    .shape_positions
                    .iter()
                    .map(|shape| (shape.position.x, shape.position.y))
                    .collect_vec()
            });
            assert_eq!(positions.as_deref(), expected_position.as_ref().map(|arr| arr.as_slice()));
        }

        run_case(64, [(32.0, 32.0), (32.0, 32.0)], Some([(-16.0, 0.0), (16.0, 0.0)]));
        run_case(63, [(32.0, 32.0), (32.0, 32.0)], None);
        run_case(
            64,
            [(32.0, 32.0), (32.0, 32.0), (32.0, 32.0), (32.0, 32.0)],
            Some([(-16.0, -16.0), (16.0, -16.0), (-16.0, 16.0), (16.0, 16.0)]),
        );
        run_case(64, [(32.0, 32.0), (32.0, 32.0), (32.0, 32.0), (2.0, 33.0)], None);
        run_case(
            64,
            [(32.0, 32.0), (16.0, 16.0), (16.0, 16.0), (16.0, 16.0)],
            Some([(-16.0, 0.0), (8.0, -8.0), (24.0, -8.0), (8.0, 8.0)]),
        );
        run_case(
            32,
            [(16.0, 8.0), (2.0, 32.0), (16.0, 2.0), (12.0, 2.0)],
            Some([(-7.0, -12.0), (2.0, 0.0), (-7.0, -7.0), (9.0, -15.0)]),
        );
    }
}
