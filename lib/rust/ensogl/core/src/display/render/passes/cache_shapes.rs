//! Pass where we checks what shapes created with [`cached_shape!`](crate::cached_shape) macro are
//! ready to being rendered on the texture.

use crate::prelude::*;

use crate::data::bounding_box::BoundingBox;
use crate::display;
use crate::display::render::pass;
use crate::display::render::pass::Instance;
use crate::display::scene::Layer;
use crate::display::scene::UpdateStatus;
use crate::display::world::with_context;
use crate::display::world::CachedShapeDefinition;
use crate::display::Context;
use crate::display::Scene;
use crate::gui::component::AnyShapeView;

use itertools::iproduct;



// =================
// === Constants ===
// =================

const INITIAL_TEXTURE_SIZE: i32 = 512;



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
/// Once given shape system has shader compiled, we call "render" only on its symbol. There is no
/// need to render previous shapes again, because we don't clear texture at any point.
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct CacheShapesPass {
    scene:          Scene,
    framebuffer:    Option<pass::Framebuffer>,
    #[derivative(Debug = "ignore")]
    shapes:         Vec<Rc<dyn AnyShapeView>>,
    texture_width:  i32,
    texture_height: i32,
    layer:          Layer,
}

impl CacheShapesPass {
    /// Constructor.
    pub fn new(scene: &Scene) -> Self {
        Self {
            framebuffer:    default(),
            shapes:         default(),
            layer:          Layer::new("Cached Shapes"),
            scene:          scene.clone_ref(),
            texture_width:  default(),
            texture_height: default(),
        }
    }
}


// === [`pass::Definition`] Implementation ===

impl pass::Definition for CacheShapesPass {
    fn initialize(&mut self, instance: &Instance) {
        let ArrangedShapes { texture_width, texture_height, shapes } =
            display::world::CACHED_SHAPES_DEFINITIONS
                .with_borrow(|shapes| arrange_shapes_on_texture(shapes));
        self.shapes = shapes.into_iter().map(Box::into).collect();
        self.texture_width = texture_width;
        self.texture_height = texture_height;

        for shape in &self.shapes {
            self.scene.add_child(&**shape);
            self.layer.add(&**shape);
        }
        self.layer.camera().set_screen(texture_width as f32, texture_height as f32);
        // We must call update of layer and display object hierarchy at this point, because:
        // 1. the [`self.layer`] is not in the Layer hierarchy, so it's not updated during routine
        //    layers update.
        // 2. There is possibility that the shapes will be rendered in the same frame, before we
        //    reach next "update display objects" stage.
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
        let mut ready_shapes = self.shapes.drain_filter(is_shader_compiled).peekable();
        if ready_shapes.peek().is_some() {
            if let Some(framebuffer) = self.framebuffer.as_ref() {
                framebuffer.bind();
                instance.context.viewport(0, 0, self.texture_width, self.texture_height);
                for shape in ready_shapes {
                    with_context(|t| {
                        t.set_camera(&self.layer.camera());
                        t.update();
                    });
                    shape.sprite().symbol.render();
                }
                // Restore screen viewport.
                instance.context.viewport(0, 0, instance.width, instance.height);
                instance.context.bind_framebuffer(*Context::FRAMEBUFFER, None);
            } else {
                error!("Impossible happened: The CacheShapesPass was run without initialized framebuffer.")
            }
        }
    }
}



// ===================================
// === Arranging Shapes on Texture ===
// ===================================

/// For shape of given size, find the unoccupied space in the square texture.
fn find_free_place(
    shape_size: Vector2<i32>,
    tex_size: i32,
    placed_so_far: &[BoundingBox],
) -> Option<BoundingBox> {
    let right_bounds = placed_so_far.iter().map(|bbox| bbox.right().ceil() as i32);
    let allowed_right_bounds = right_bounds.filter(|col| col + shape_size.x <= tex_size);
    let top_bounds = placed_so_far.iter().map(|bbox| bbox.top().ceil() as i32);
    let allowed_top_bounds = top_bounds.filter(|row| row + shape_size.y <= tex_size);
    let candidate_rows = iter::once(0).chain(allowed_top_bounds);
    let candidate_cols = iter::once(0).chain(allowed_right_bounds);
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
/// Algorithm of placing each shape on the texture is a simple heuristic, where we sort shapes from
/// the biggest to the smallest, and take the shapes one by one by that order and try to fit them
/// by scanning possible left-bottom corner positions, starting from left-bottom corner of the
/// texture and going line-by-line. According to
/// https://gamedev.stackexchange.com/questions/2829/texture-packing-algorithm this is actually one
/// of the best packing algorithm in terms of texture space.
///
/// This is a simple implementation with time complexity O(n m s) where n and m is the number of
/// rows and columns in the resulting texture, and s is number of placed shapes.
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::display::shape::*;

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
            assert_eq!(
                positions.as_ref().map(|vec| vec.as_slice()),
                expected_position.as_ref().map(|arr| arr.as_slice())
            );
        }


        run_case(64, [(32, 32), (32, 32)], Some([(16.0, 16.0), (48.0, 16.0)]));
        run_case(63, [(32, 32), (32, 32)], None);
        run_case(
            64,
            [(32, 32), (32, 32), (32, 32), (32, 32)],
            Some([(16.0, 16.0), (48.0, 16.0), (16.0, 48.0), (48.0, 48.0)]),
        );
        run_case(64, [(32, 32), (32, 32), (32, 32), (2, 33)], None);
        run_case(
            64,
            [(32, 32), (16, 16), (16, 16), (16, 16)],
            Some([(16.0, 16.0), (40.0, -8.0), (56.0, -8.0), (40.0, 24.0)]),
        );
        run_case(
            32,
            [(16, 8), (2, 32), (16, 2), (12, 2)],
            Some([(8.0, 4.0), (17.0, 16.0), (8.0, 9.0), (24.0, 1.0)]),
        );
    }

    #[test]
    fn texture_size() {
        fn run_case(shape_sizes: impl IntoIterator<Item = (i32, i32)>, expected_size: (i32, i32)) {
            let shape_entries = shape_entries_from_sizes(shape_sizes);
            let result = arrange_shapes_on_texture(shape_entries.iter());
            assert_eq!((result.texture_width, result.texture_height), expected_size);
        }

        run_case([(32, 32), (16, 16)], (48, 32));
        run_case([(16, 2), (2, 20)], (18, 20));
        run_case([(256, 2), (257, 2)], (257, 4));
        run_case(iter::repeat((32, 32)).take(2), (64, 32));
        run_case(iter::repeat((32, 32)).take(16), (512, 32));
        run_case(iter::repeat((32, 32)).take(17), (512, 64));
        run_case(iter::repeat((64, 64)).take(64), (512, 512));
        // Shapes does not fit initial texture size: the texture is extended.
        run_case(iter::repeat((64, 64)).take(65), (1024, 320));
        // Several steps of extending the texture
        run_case(iter::repeat((512, 512)).take(17), (4096, 1536));
    }
}
