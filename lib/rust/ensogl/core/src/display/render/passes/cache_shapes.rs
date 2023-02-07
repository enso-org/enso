//! Pass where we check what shapes created with [`cached_shape!`](crate::cached_shape) macro are
//! ready to being rendered to the texture.

use crate::prelude::*;

use crate::data::bounding_box::BoundingBox;
use crate::display;
use crate::display::render::pass;
use crate::display::render::pass::Instance;
use crate::display::scene::Layer;
use crate::display::scene::UpdateStatus;
use crate::display::world::with_context;
use crate::display::world::CachedShapeDefinition;
use crate::display::Scene;
use crate::gui::component::AnyShapeView;

use itertools::iproduct;



// =================
// === Constants ===
// =================

/// A parameter of [`arrange_shapes_on_texture`] algorithm: the initial assumed size for the texture
/// with cached shapes. If it turn out to be too small, we extend this size and try again.
const INITIAL_TEXTURE_SIZE: i32 = 512;

/// A parameter of [`arrange_shapes_on_texture`] algorithm: the factor how much the texture size is
/// increased when the current size is insufficient.
const TEXTURE_SIZE_MULTIPLIER: i32 = 2;



// =======================
// === CacheShapesPass ===
// =======================

/// Definition of pass rendering cached shapes to texture.
///
/// On each run it checks what not-yet-rendered shapes has compiled shaders and render them to
/// the texture, which is stored in `pass_cached_shapes` uniform.
///
/// # Implementation
///
/// For each shape to render we create a single [view](crate::gui::component::ShapeView), which is
/// then added to a special, internal [`Layer`] which is not rendered in the default symbol pass.
/// Once given shape system is ready to render (has shader compiled), we call "render" only on its
/// symbol. There is no need to render previous shapes again, because we don't clear texture at any
/// point.
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct CacheShapesPass {
    scene:            Scene,
    framebuffer:      Option<pass::Framebuffer>,
    #[derivative(Debug = "ignore")]
    shapes_to_render: Vec<Rc<dyn AnyShapeView>>,
    texture_width:    i32,
    texture_height:   i32,
    layer:            Layer,
}

impl CacheShapesPass {
    /// Constructor.
    pub fn new(scene: &Scene) -> Self {
        Self {
            framebuffer:      default(),
            shapes_to_render: default(),
            layer:            Layer::new("Cached Shapes"),
            scene:            scene.clone_ref(),
            texture_width:    default(),
            texture_height:   default(),
        }
    }
}


// === [`pass::Definition`] Implementation ===

impl pass::Definition for CacheShapesPass {
    fn initialize(&mut self, instance: &Instance) {
        let ArrangedShapes { texture_width, texture_height, shapes } =
            display::world::CACHED_SHAPES_DEFINITIONS
                .with_borrow(|shapes| arrange_shapes_on_texture(shapes));
        self.shapes_to_render = shapes.into_iter().map(Box::into).collect();
        self.texture_width = texture_width;
        self.texture_height = texture_height;

        for shape in &self.shapes_to_render {
            self.scene.add_child(&**shape);
            self.layer.add(&**shape);
        }
        self.layer.camera().set_screen(texture_width as f32, texture_height as f32);
        // We must call update of layer and display object hierarchy at this point, because:
        // 1. the [`self.layer`] is not in the Layer hierarchy, so it's not updated during routine
        //    layers update.
        // 2. The pass can be re-initialized after the display object hierarchy update, but before
        //    rendering, so the herarchy could be outdated during rendering.
        self.layer.camera().update(&self.scene);
        self.scene.display_object.update(&self.scene);
        self.layer.update();

        let output = pass::OutputDefinition::new_rgba("cached_shapes");
        let texture = instance.new_texture(&output, texture_width, texture_height);
        self.framebuffer = Some(instance.new_framebuffer(&[&texture]));
    }

    fn run(&mut self, instance: &Instance, _update_status: UpdateStatus) {
        let is_shader_compiled =
            |shape: &mut Rc<dyn AnyShapeView>| shape.sprite().symbol.shader().program().is_some();
        let mut ready_to_render = self.shapes_to_render.drain_filter(is_shader_compiled).peekable();
        if ready_to_render.peek().is_some() {
            if let Some(framebuffer) = self.framebuffer.as_ref() {
                framebuffer.with_bound(|| {
                    instance.with_viewport(self.texture_width, self.texture_height, || {
                        with_context(|ctx| ctx.set_camera(&self.layer.camera()));
                        for shape in ready_to_render {
                            shape.sprite().symbol.render();
                        }
                    });
                });
            } else {
                reportable_error!("Impossible happened: The CacheShapesPass was run without initialized framebuffer.");
            }
        }
    }
}



// ===================================
// === Arranging Shapes on Texture ===
// ===================================

/// For shape of given size, find the unoccupied space in the square texture.
///
/// A step of the [`arrange_shapes_on_texture`] algorithm. See its docs for details.
fn find_free_place(
    shape_size: Vector2<i32>,
    tex_size: i32,
    placed_so_far: &[BoundingBox],
) -> Option<BoundingBox> {
    // There is no need of iterating over all columns and rows, as the new shape is always "glued"
    // to right/top boundary of another shape, or left/bottom boundary of the texture.
    let right_bounds = placed_so_far.iter().map(|bbox| bbox.right().ceil() as i32);
    let allowed_right_bounds = right_bounds.filter(|col| col + shape_size.x <= tex_size);
    let top_bounds = placed_so_far.iter().map(|bbox| bbox.top().ceil() as i32);
    let allowed_top_bounds = top_bounds.filter(|row| row + shape_size.y <= tex_size);
    let candidate_rows = iter::once(0).chain(allowed_top_bounds).sorted();
    let candidate_cols = iter::once(0).chain(allowed_right_bounds).sorted();
    let candidate_positions = iproduct!(candidate_rows, candidate_cols);
    let mut candidate_bboxes = candidate_positions.map(|(y, x)| {
        BoundingBox::from_position_and_size(
            Vector2(x as f32, y as f32),
            shape_size.map(|i| i as f32),
        )
    });
    candidate_bboxes.find(|bbox| {
        let is_collision = placed_so_far.iter().any(|placed| placed.interior_intersects(bbox));
        !is_collision
    })
}

/// The results of [`arrange_shapes_on_texture`] function.
#[derive(Derivative)]
#[derivative(Debug)]
struct ArrangedShapes {
    texture_width:  i32,
    texture_height: i32,
    #[derivative(Debug = "ignore")]
    shapes:         Vec<Box<dyn AnyShapeView>>,
}

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
fn arrange_shapes_on_texture<'a>(
    shapes: impl IntoIterator<Item = &'a CachedShapeDefinition>,
) -> ArrangedShapes {
    let sorted_shapes =
        shapes.into_iter().sorted_by_key(|shape| shape.size.x * shape.size.y).rev().collect_vec();
    let mut tex_size = INITIAL_TEXTURE_SIZE;
    loop {
        if let Some(arranged) = try_to_fit_shapes_on_texture(&sorted_shapes, tex_size) {
            break arranged;
        }
        tex_size *= 2;
    }
}

/// Try to fit shapes on texture assuming maximum texture size. Returns [`None`] if was unable to
/// do it.
fn try_to_fit_shapes_on_texture(
    sorted_shapes: &[impl std::borrow::Borrow<CachedShapeDefinition>],
    max_texture_size: i32,
) -> Option<ArrangedShapes> {
    let mut placed_so_far: Vec<BoundingBox> = vec![];
    let sorted_shapes_ref = sorted_shapes.iter().map(|shape| shape.borrow());
    let shapes_with_positions_iter = sorted_shapes_ref.map(|shape_def| {
        find_free_place(shape_def.size, max_texture_size, &placed_so_far).map(|bbox| {
            placed_so_far.push(bbox);
            (shape_def, bbox.center())
        })
    });
    // We collect all positions, so `placed_so_far` will contain all shapes and we can compute
    // texture width.
    let shapes_with_positions: Option<Vec<_>> = shapes_with_positions_iter.collect();
    let texture_width =
        placed_so_far.iter().map(BoundingBox::right).reduce(f32::max).unwrap_or_default();
    let texture_height =
        placed_so_far.iter().map(BoundingBox::top).reduce(f32::max).unwrap_or_default();
    let texture_origin = Vector2(texture_width, texture_height) / 2.0;
    shapes_with_positions.map(|shapes_with_positions| {
        let shapes = shapes_with_positions.into_iter().map(|(shape_def, position)| {
            let shape = (shape_def.cons)();
            shape.set_size(shape_def.size);
            shape.set_xy(position - texture_origin);
            shape
        });
        ArrangedShapes {
            shapes:         shapes.collect(),
            texture_width:  texture_width as i32,
            texture_height: texture_height as i32,
        }
    })
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use crate::display::shape::*;
    use crate::display::world::World;

    mod mock_shape {
        use super::*;

        crate::shape! {
            (style: Style) {
                Plane().into()
            }
        }
    }

    fn shape_entries_from_sizes(
        sizes: impl IntoIterator<Item = (i32, i32)>,
    ) -> Vec<CachedShapeDefinition> {
        sizes
            .into_iter()
            .map(|(width, height)| CachedShapeDefinition {
                size: Vector2(width, height),
                cons: Box::new(|| Box::new(mock_shape::View::new())),
            })
            .collect_vec()
    }


    #[test]
    fn texture_size() {
        fn run_case(shape_sizes: impl IntoIterator<Item = (i32, i32)>, expected_size: (i32, i32)) {
            let shape_entries = shape_entries_from_sizes(shape_sizes);
            let result = arrange_shapes_on_texture(shape_entries.iter());
            assert_eq!((result.texture_width, result.texture_height), expected_size);
        }

        let _world = World::new();

        run_case([(32, 32), (16, 16)], (48, 32));
        run_case([(16, 2), (2, 20)], (18, 20));
        run_case([(256, 2), (257, 2)], (257, 4));
        run_case(iter::repeat((32, 32)).take(2), (64, 32));
        run_case(iter::repeat((32, 32)).take(16), (512, 32));
        run_case(iter::repeat((32, 32)).take(17), (512, 64));
        run_case(iter::repeat((64, 64)).take(64), (512, 512));
        // Shapes does not fit initial texture size: the texture is extended.
        run_case(iter::repeat((64, 64)).take(65), (1024, 320));
        // This will extend the texture several times.
        run_case(iter::repeat((512, 512)).take(17), (4096, 1536));
    }

    #[test]
    fn fitting_shapes_on_texture() {
        fn run_case<const N: usize>(
            tex_size: i32,
            sizes: [(i32, i32); N],
            expected_position: Option<[(f32, f32); N]>,
        ) {
            let shape_entries = shape_entries_from_sizes(sizes);
            let result = try_to_fit_shapes_on_texture(&shape_entries, tex_size);
            let positions = result.map(|shapes| {
                shapes.shapes.iter().map(|shape| (shape.x(), shape.y())).collect_vec()
            });
            assert_eq!(positions.as_deref(), expected_position.as_ref().map(|arr| arr.as_slice()));
        }

        let _world = World::new();
        // run_case(64, [(32, 32), (32, 32)], Some([(-16.0, 0.0), (16.0, 0.0)]));
        // run_case(63, [(32, 32), (32, 32)], None);
        // run_case(
        //     64,
        //     [(32, 32), (32, 32), (32, 32), (32, 32)],
        //     Some([(-16.0, -16.0), (16.0, -16.0), (-16.0, 16.0), (16.0, 16.0)]),
        // );
        // run_case(64, [(32, 32), (32, 32), (32, 32), (2, 33)], None);
        run_case(
            64,
            [(32, 32), (16, 16), (16, 16), (16, 16)],
            Some([(-16.0, 0.0), (8.0, -8.0), (24.0, -8.0), (8.0, 8.0)]),
        );
        // run_case(
        //     32,
        //     [(16, 8), (2, 32), (16, 2), (12, 2)],
        //     Some([(-8.0, -12.0), (1.0, 0.0), (-8.0, -7.0), (8.0, -15.0)]),
        // );
    }
}
