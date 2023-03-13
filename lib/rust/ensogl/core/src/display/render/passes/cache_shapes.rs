//! Pass where we check what shapes created with [`cached_shape!`](crate::cached_shape) macro are
//! ready to being rendered to the texture.

use crate::prelude::*;

use crate::display;
use crate::display::render::pass;
use crate::display::render::pass::Instance;
use crate::display::scene::Layer;
use crate::display::scene::UpdateStatus;
use crate::display::shape::glsl::codes::DisplayModes;
use crate::display::world::with_context;
use crate::display::Scene;
use crate::gui::component::AnyShapeView;



// =======================
// === CacheShapesPass ===
// =======================

/// Definition of pass rendering cached shapes to texture.
///
/// On each run it checks what not-yet-rendered shapes has compiled shaders and render their color
/// and SDF information to the texture, which is stored in `pass_cached_shapes` uniform. See also
/// the [full documentation of cached shapes](display::shape::primitive::system::cached).
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
    scene:               Scene,
    framebuffer:         Option<pass::Framebuffer>,
    #[derivative(Debug = "ignore")]
    shapes_to_render:    Vec<Rc<dyn AnyShapeView>>,
    /// Texture size in device pixels.
    texture_size_device: Vector2<i32>,
    layer:               Layer,
}

impl CacheShapesPass {
    /// Constructor.
    pub fn new(scene: &Scene) -> Self {
        Self {
            framebuffer:         default(),
            shapes_to_render:    default(),
            layer:               Layer::new("Cached Shapes"),
            scene:               scene.clone_ref(),
            texture_size_device: default(),
        }
    }
}


// === [`pass::Definition`] Implementation ===

impl pass::Definition for CacheShapesPass {
    fn initialize(&mut self, instance: &Instance) {
        display::world::CACHED_SHAPES_DEFINITIONS.with_borrow(|shapes| {
            self.shapes_to_render =
                shapes.iter().map(|def| (def.for_texture_constructor)().into()).collect()
        });
        let texture_size = display::shape::primitive::system::cached::texture_size();
        self.texture_size_device =
            texture_size.map(|i| ((i as f32) * instance.pixel_ratio).ceil() as i32);

        for shape in &self.shapes_to_render {
            self.scene.add_child(&**shape);
            self.layer.add(&**shape);
        }
        self.layer.camera().set_screen(texture_size.x as f32, texture_size.y as f32);
        // We must call update of layer and display object hierarchy at this point, because:
        // 1. the [`self.layer`] is not in the Layer hierarchy, so it's not updated during routine
        //    layers update.
        // 2. The pass can be re-initialized after the display object hierarchy update, but before
        //    rendering, so the herarchy could be outdated during rendering.
        self.layer.camera().update(&self.scene);
        self.scene.display_object.update(&self.scene);
        self.layer.update();

        let output = pass::OutputDefinition::new_rgba("cached_shapes");
        let texture =
            instance.new_texture(&output, self.texture_size_device.x, self.texture_size_device.y);
        self.framebuffer = Some(instance.new_framebuffer(&[&texture]));
    }

    fn run(&mut self, instance: &Instance, _update_status: UpdateStatus) {
        let is_shader_compiled =
            |shape: &mut Rc<dyn AnyShapeView>| shape.sprite().symbol.shader().program().is_some();
        let mut ready_to_render = self.shapes_to_render.drain_filter(is_shader_compiled).peekable();
        if ready_to_render.peek().is_some() {
            if let Some(framebuffer) = self.framebuffer.as_ref() {
                framebuffer.with_bound(|| {
                    instance.with_viewport(
                        self.texture_size_device.x,
                        self.texture_size_device.y,
                        || {
                            with_display_mode(DisplayModes::CachedShapesTexture, || {
                                with_context(|ctx| ctx.set_camera(&self.layer.camera()));
                                for shape in ready_to_render {
                                    shape.sprite().symbol.render();
                                }
                            })
                        },
                    );
                });
            } else {
                reportable_error!("Impossible happened: The CacheShapesPass was run without initialized framebuffer.");
            }
        }
    }
}

fn with_display_mode<R>(mode: DisplayModes, f: impl FnOnce() -> R) -> R {
    with_context(move |ctx| {
        let mode_before = ctx.display_mode.get();
        let code: u32 = mode.into();
        ctx.display_mode.set(code as i32);
        let result = f();
        ctx.display_mode.set(mode_before);
        result
    })
}
