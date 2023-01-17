use crate::prelude::*;

use crate::data::bounding_box::BoundingBox;
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

use crate::display;
use itertools::iproduct;

const TEX_SIZE: i32 = 1024;

#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct CacheShapesPass {
    scene:       Scene,
    framebuffer: Option<pass::Framebuffer>,
    #[derivative(Debug = "ignore")]
    shapes:      Vec<Rc<dyn AnyShapeView>>,
    layer:       Layer,
}

impl pass::Definition for CacheShapesPass {
    fn initialize(&mut self, instance: &Instance) {
        let output = pass::OutputDefinition::new_rgba("color");

        // FIXME[ao]: duplicated with new_screen_texture?
        let context = &instance.context;
        let variables = &instance.variables;
        let name = "pass_cached_shapes";
        let args = (TEX_SIZE, TEX_SIZE);
        let format = output.internal_format;
        let item_type = output.item_type;
        let params = Some(output.texture_parameters);
        let texture = uniform::get_or_add_gpu_texture_dyn(
            context, variables, name, format, item_type, args, params,
        );
        self.framebuffer = Some(instance.new_framebuffer(&[&texture]));
        self.shapes = display::world::CACHED_SHAPES_DEFINITIONS.with_borrow(|cached_shapes| {
            arrange_shapes_on_texture(cached_shapes, TEX_SIZE).map(Box::into).collect()
        });
        for shape in &self.shapes {
            warn!("The position on initialization is {:?}", shape.xy());
            self.scene.add_child(&**shape);
            self.layer.add(&**shape);
        }
        self.layer.camera().set_screen(TEX_SIZE as f32, TEX_SIZE as f32);
        self.layer.camera().update(&self.scene);
        self.scene.display_object.update(&self.scene);
        self.layer.update();
    }

    fn run(&mut self, instance: &Instance, _update_status: UpdateStatus) {
        let mut ready_shapes = self
            .shapes
            .drain_filter(|shape| shape.sprite().symbol.shader().program().is_some())
            .peekable();
        if ready_shapes.peek().is_some() {
            if let Some(framebuffer) = self.framebuffer.as_ref() {
                framebuffer.bind();
                instance.context.viewport(0, 0, TEX_SIZE, TEX_SIZE);
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
            framebuffer: default(),
            shapes:      default(),
            layer:       Layer::new("Cached Shapes"),
            scene:       scene.clone_ref(),
        }
    }
}

struct ShapeBeingPlacedOnTexture<'a> {
    definition:     &'a CachedShapeDefinition,
    position_found: Option<Vector2>,
}

impl<'a> ShapeBeingPlacedOnTexture<'a> {
    fn new(definition: &'a CachedShapeDefinition) -> Self {
        Self { definition, position_found: None }
    }

    fn put_on_free_place(
        &mut self,
        tex_size: i32,
        placed_so_far: &[BoundingBox],
    ) -> Option<BoundingBox> {
        let free_place = self.find_free_place(tex_size, placed_so_far);
        self.position_found = free_place.as_ref().map(|bbox| bbox.center());
        if self.position_found.is_none() {
            error!("Cannot insert shape! The texture size {tex_size} is too small!")
        }
        free_place
    }

    fn find_free_place(&self, tex_size: i32, placed_so_far: &[BoundingBox]) -> Option<BoundingBox> {
        let shape_size = self.definition.size;
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

    fn create_shape(self) -> Option<Box<dyn AnyShapeView>> {
        self.position_found.map(|pos| {
            let shape = (self.definition.cons)();
            shape.set_xy(pos);
            shape.set_size(self.definition.size.map(|i| i as f32));
            shape
        })
    }
}

fn arrange_shapes_on_texture<'a>(
    shapes: impl IntoIterator<Item = &'a CachedShapeDefinition>,
    tex_size: i32,
) -> impl Iterator<Item = Box<dyn AnyShapeView>> + 'a {
    let shapes = shapes.into_iter().map(ShapeBeingPlacedOnTexture::new);
    let sorted_shapes =
        shapes.sorted_by_key(|shape| shape.definition.size.x * shape.definition.size.y).rev();
    let mut placed_so_far: Vec<BoundingBox> = vec![];
    sorted_shapes.filter_map(move |mut shape| {
        if let Some(took_bbox) = shape.put_on_free_place(tex_size, &placed_so_far) {
            placed_so_far.push(took_bbox);
        }
        shape.create_shape()
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::display::shape::*;
    use wasm_bindgen::prelude::*;

    mod mock_shape {
        use super::*;

        crate::shape2! {
            (style: Style) {
                Plane().into()
            }
        }
    }

    #[test]
    fn arrange_shapes_in_texture() {
        fn run_case<const N: usize>(
            tex_size: i32,
            sizes: [(i32, i32); N],
            expected_position: [(f32, f32); N],
        ) {
            let shape_entries = sizes
                .into_iter()
                .map(|(width, height)| CachedShapeDefinition {
                    size: Vector2(width, height),
                    cons: Box::new(|| Box::new(mock_shape::View::new())),
                })
                .collect_vec();
            let shapes = arrange_shapes_on_texture(&shape_entries, tex_size);
            let positions = shapes.map(|shape| (shape.x(), shape.y())).collect_vec();
            assert_eq!(&positions, &expected_position);
        }


        run_case(64, [(32, 32), (32, 32)], ([(-16.0, -16.0), (16.0, -16.0)]));
        run_case(
            64,
            [(32, 32), (32, 32), (32, 32), (32, 32)],
            ([(-16.0, -16.0), (16.0, -16.0), (-16.0, 16.0), (16.0, 16.0)]),
        );
        run_case(
            64,
            [(32, 32), (16, 16), (16, 16), (16, 16)],
            ([(-16.0, -16.0), (8.0, -24.0), (24.0, -24.0), (8.0, -8.0)]),
        );
        run_case(
            32,
            [(16, 8), (2, 32), (16, 2), (12, 2)],
            ([(-8.0, -12.0), (1.0, 0.0), (-8.0, -7.0), (8.0, -15.0)]),
        );
    }
}
