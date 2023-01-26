//! Pass where we checks what shapes created with [`cached_shape!`](crate::cached_shape) macro are
//! ready to being rendered on the texture.

use crate::prelude::*;

use crate::data::bounding_box::BoundingBox;
use crate::display;
use crate::display::render::pass;
use crate::display::render::pass::Instance;
use crate::display::scene::Layer;
use crate::display::scene::UpdateStatus;
use crate::display::uniform;
use crate::display::world::with_context;
use crate::display::world::CachedShapeDefinition;
use crate::display::Context;
use crate::display::Scene;
use crate::gui::component::AnyShapeView;

use itertools::iproduct;

const INITIAL_TEXTURE_SIZE: i32 = 256;

#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct CacheShapesPass {
    scene:        Scene,
    framebuffer:  Option<pass::Framebuffer>,
    #[derivative(Debug = "ignore")]
    shapes:       Vec<Rc<dyn AnyShapeView>>,
    texture_size: Vector2<i32>,
    layer:        Layer,
}

impl pass::Definition for CacheShapesPass {
    fn initialize(&mut self, instance: &Instance) {
        let ArrangedShapes { texture_size, shapes } = display::world::CACHED_SHAPES_DEFINITIONS
            .with_borrow(|cached_shapes| arrange_shapes_on_texture(cached_shapes));
        self.shapes = shapes.into_iter().map(Box::into).collect();
        self.texture_size = texture_size;

        for shape in &self.shapes {
            warn!("The position on initialization is {:?}", shape.xy());
            self.scene.add_child(&**shape);
            self.layer.add(&**shape);
        }
        self.layer.camera().set_screen(texture_size.x as f32, texture_size.y as f32);
        self.layer.camera().update(&self.scene);
        self.scene.display_object.update(&self.scene);
        self.layer.update();

        let output = pass::OutputDefinition::new_rgba("color");

        // FIXME[ao]: duplicated with new_screen_texture?
        let context = &instance.context;
        let variables = &instance.variables;
        let name = "pass_cached_shapes";
        let format = output.internal_format;
        let item_type = output.item_type;
        let args = (texture_size.x, texture_size.y);
        let params = Some(output.texture_parameters);
        let texture = uniform::get_or_add_gpu_texture_dyn(
            context, variables, name, format, item_type, args, params,
        );
        self.framebuffer = Some(instance.new_framebuffer(&[&texture]));
    }

    fn run(&mut self, instance: &Instance, _update_status: UpdateStatus) {
        let mut ready_shapes = self
            .shapes
            .drain_filter(|shape| shape.sprite().symbol.shader().program().is_some())
            .peekable();
        if ready_shapes.peek().is_some() {
            if let Some(framebuffer) = self.framebuffer.as_ref() {
                framebuffer.bind();
                instance.context.viewport(0, 0, self.texture_size.x, self.texture_size.y);
                for shape in ready_shapes {
                    warn!("Rendering shape {}", shape.sprite().symbol.id);
                    // warn!("After update it is {}", shape.sprite().symbol.id);
                    warn!("The position on run is {:?}", shape.xy());
                    warn!("The buffer is {:#?}", shape.sprite().transform);
                    // warn!("The size is {:#?}", shape.sprite().size);
                    // warn!("The matrix is {:?}", shape.display_object().transformation_matrix());
                    with_context(|t| {
                        warn!("CAMERA: {:#?}", self.layer.camera());
                        t.set_camera(&self.layer.camera());
                        t.update();
                        // t.render_symbols(&self.layer.symbols());
                    });
                    warn!(
                        "Printing symbol with VARIABLES: {:#?}",
                        shape.sprite().symbol.variables()
                    );
                    warn!("Printing symbol with BINDINGS: {:#?}", shape.sprite().symbol.bindings);
                    shape.sprite().symbol.render();
                    self.layer.remove(&*shape);
                }
                instance.context.viewport(0, 0, instance.width, instance.height);
                instance.context.bind_framebuffer(*Context::FRAMEBUFFER, None);
            }
        }
    }
}

impl CacheShapesPass {
    pub fn new(scene: &Scene) -> Self {
        Self {
            framebuffer:  default(),
            shapes:       default(),
            layer:        Layer::new("Cached Shapes"),
            scene:        scene.clone_ref(),
            texture_size: default(),
        }
    }
}


fn find_free_place(
    shape_size: Vector2<i32>,
    tex_size: i32,
    placed_so_far: &[BoundingBox],
) -> Option<BoundingBox> {
    let row_or_col_to_coord = |index| (index as f32) - (tex_size as f32) / 2.0;
    let candidate_rows = (0..tex_size - shape_size.y + 1).map(row_or_col_to_coord);
    let candidate_cols = (0..tex_size - shape_size.x + 1).map(row_or_col_to_coord);
    let candidate_positions = iproduct!(candidate_rows, candidate_cols);
    let mut candidate_bboxes = candidate_positions.map(|(y, x)| {
        BoundingBox::from_position_and_size(Vector2(x, y), shape_size.map(|i| i as f32))
    });
    candidate_bboxes.find(|bbox| {
        let is_collision = placed_so_far.iter().any(|placed| placed.interior_intersects(bbox));
        !is_collision
    })
}

struct ArrangedShapes {
    texture_size: Vector2<i32>,
    shapes:       Vec<Box<dyn AnyShapeView>>,
}



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

fn try_to_fit_shapes_on_texture(
    sorted_shapes: &[impl std::borrow::Borrow<CachedShapeDefinition>],
    max_texture_size: i32,
) -> Option<ArrangedShapes> {
    let mut placed_so_far: Vec<BoundingBox> = vec![];
    let mut texture_size: Vector2<i32> = default();
    let sorted_shapes_ref = sorted_shapes.iter().map(|shape| shape.borrow());
    let shape_view_iter = sorted_shapes_ref.map(move |shape_def| {
        find_free_place(shape_def.size, max_texture_size, &placed_so_far).map(|bbox| {
            texture_size.x = max(texture_size.x, bbox.right() as i32);
            texture_size.y = max(texture_size.y, bbox.top() as i32);
            placed_so_far.push(bbox);
            let position = bbox.center();
            let shape = (shape_def.cons)();
            shape.set_size(shape_def.size);
            shape.set_xy(position);
            shape
        })
    });
    let shape_views: Option<Vec<_>> = shape_view_iter.collect();
    shape_views.map(|shapes| ArrangedShapes { shapes, texture_size })
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


        run_case(64, [(32, 32), (32, 32)], Some([(-16.0, -16.0), (16.0, -16.0)]));
        run_case(63, [(32, 32), (32, 32)], None);
        run_case(
            64,
            [(32, 32), (32, 32), (32, 32), (32, 32)],
            Some([(-16.0, -16.0), (16.0, -16.0), (-16.0, 16.0), (16.0, 16.0)]),
        );
        run_case(64, [(32, 32), (32, 32), (32, 32), (2, 33)], None);
        run_case(
            64,
            [(32, 32), (16, 16), (16, 16), (16, 16)],
            Some([(-16.0, -16.0), (8.0, -24.0), (24.0, -24.0), (8.0, -8.0)]),
        );
        run_case(
            32,
            [(16, 8), (2, 32), (16, 2), (12, 2)],
            Some([(-8.0, -12.0), (1.0, 0.0), (-8.0, -7.0), (8.0, -15.0)]),
        );
    }

    #[test]
    fn texture_size() {
        fn run_case(shape_sizes: impl IntoIterator<Item = (i32, i32)>, expected_size: (i32, i32)) {
            let shape_entries = shape_entries_from_sizes(shape_sizes);
            let result = arrange_shapes_on_texture(shape_entries.iter());
            assert_eq!(result.texture_size, IntoVector2::into_vector(expected_size));
        }

        run_case([(32, 32), (16, 16)], (48, 32));
        run_case([(16, 2), (2, 20)], (22, 16));
        run_case(iter::repeat((32, 32)).take(2), (64, 32));
        run_case(iter::repeat((32, 32)).take(16), (512, 32));
        run_case(iter::repeat((32, 32)).take(256), (512, 512));
        // Shapes does not fit initial texture size: the texture is extended.
        run_case(iter::repeat((32, 32)).take(257), (1024, 544));
        // Several steps of extending the texture
        run_case(iter::repeat((512, 512)).take(17), (4096, 1536));
    }
}
