//! Pass where we check what shapes created with [`cached_shape!`](crate::cached_shape) macro are
//! ready to being rendered to the texture.

use crate::prelude::*;

use crate::display;
use crate::display::render::pass;
use crate::display::scene::Layer;
use crate::display::scene::UpdateStatus;
use crate::display::shape::glsl::codes::DisplayModes;
use crate::display::world::with_context;
use crate::gui::component::AnyShapeView;
use crate::system::gpu::context::ContextLost;


// ==========================
// === CacheShapesPassDef ===
// ==========================

/// Definition of pass rendering cached shapes to texture. See [`CacheShapesPass`] for information
/// about pass operation.
#[derive(Debug)]
pub struct CacheShapesPassDef {
    layer: Layer,
}

impl Default for CacheShapesPassDef {
    fn default() -> Self {
        Self { layer: Layer::new("Cached Shapes") }
    }
}

impl CacheShapesPassDef {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }
}

impl pass::Definition for CacheShapesPassDef {
    fn instantiate(
        &self,
        instance: pass::InstanceInfo,
    ) -> Result<Box<dyn pass::Instance>, ContextLost> {
        Ok(Box::new(CacheShapesPass::new(self.layer.clone(), instance)?))
    }
}



// =======================
// === CacheShapesPass ===
// =======================

/// Instance of pass rendering cached shapes to texture.
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
#[derive(Derivative)]
#[derivative(Debug)]
pub struct CacheShapesPass {
    framebuffer: pass::Framebuffer,
    texture: crate::system::gpu::data::uniform::AnyTextureUniform,
    #[derivative(Debug = "ignore")]
    shapes_to_render: Vec<Box<dyn AnyShapeView>>,
    /// Texture size in device pixels.
    texture_size_device: Vector2<i32>,
    layer: Layer,
    camera_ready: Rc<Cell<bool>>,
    #[derivative(Debug = "ignore")]
    display_object_update_handler: Option<Rc<enso_callback::Handle>>,
    instance: pass::InstanceInfo,
}

impl CacheShapesPass {
    /// Constructor.
    pub fn new(layer: Layer, instance: pass::InstanceInfo) -> Result<Self, ContextLost> {
        let scene = scene();
        let shapes_to_render: Vec<Box<dyn AnyShapeView>> =
            display::world::CACHED_SHAPES_DEFINITIONS.with_borrow(|shapes| {
                shapes.iter().map(|def| (def.for_texture_constructor)()).collect()
            });
        let texture_size = display::shape::primitive::system::cached::texture_size();
        let texture_size_device =
            texture_size.map(|i| ((i as f32) * instance.pixel_ratio).ceil() as i32);
        for shape in &shapes_to_render {
            scene.add_child(&**shape);
            layer.add(&**shape);
        }
        layer.camera().set_screen(texture_size.x as f32, texture_size.y as f32);
        // We must call update of layer and display object hierarchy at this point, because:
        // 1. the [`self.layer`] is not in the Layer hierarchy, so it's not updated during routine
        //    layers update.
        // 2. The pass can be re-initialized after the display object hierarchy update, but before
        //    rendering, so the hierarchy could be outdated during rendering.
        layer.camera().update(&scene);
        scene.display_object.update(&scene);
        layer.update();
        let camera_ready = Rc::new(Cell::new(false));
        // The asynchronous update of the scene's display object initiated above will eventually
        // set our layer's camera's transformation. Handle the camera update when all
        // previously-initiated FRP events finish being processed.
        let display_object_update_handler = frp::microtasks::next_microtask({
            let camera = layer.camera();
            let camera_ready = Rc::clone(&camera_ready);
            move || {
                // Be careful to not capture variable `scene` here! The scene keeps all passes,
                // so we would create an Rc loop.
                camera.update(&display::world::scene());
                camera_ready.set(true);
            }
        });
        let output = pass::OutputDefinition::new_rgba("cached_shapes");
        let texture = instance.new_texture(&output, texture_size_device.x, texture_size_device.y);
        let framebuffer = instance.new_framebuffer(&[&texture])?;
        Ok(Self {
            framebuffer,
            texture,
            shapes_to_render,
            layer,
            texture_size_device,
            camera_ready,
            display_object_update_handler: Some(Rc::new(display_object_update_handler)),
            instance,
        })
    }
}

impl pass::Instance for CacheShapesPass {
    fn run(&mut self, _update_status: UpdateStatus) {
        if self.camera_ready.get() {
            let is_shader_compiled = |shape: &mut Box<dyn AnyShapeView>| {
                shape.sprite().symbol.shader.borrow().program().is_some()
            };
            let mut ready_to_render =
                self.shapes_to_render.drain_filter(is_shader_compiled).peekable();
            if ready_to_render.peek().is_some() {
                self.framebuffer.with_bound(|| {
                    self.instance.with_viewport(
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
            }
        }
    }

    fn resize(&mut self, width: i32, height: i32, pixel_ratio: f32) {
        self.instance.width = width;
        self.instance.height = height;
        self.instance.pixel_ratio = pixel_ratio;
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
