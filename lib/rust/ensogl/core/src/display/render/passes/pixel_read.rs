//! Pass reading pixels from a previously created framebuffer.

use crate::prelude::*;
use crate::system::gpu::*;
use crate::system::js::*;

use crate::display::render::pass;
use crate::display::scene::UpdateStatus;
use crate::system::gpu::context::ContextLost;

use web_sys::WebGlBuffer;
use web_sys::WebGlFramebuffer;
use web_sys::WebGlSync;



// ========================
// === PixelReadPassDef ===
// ========================

/// Definition of a pass that reads the color of a pixel.
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct PixelReadPassDef<T> {
    position:      Uniform<Vector2<i32>>,
    threshold:     Rc<Cell<usize>>,
    #[derivative(Debug = "ignore")]
    callback:      Rc<dyn Fn(Vec<T>)>,
    #[derivative(Debug = "ignore")]
    sync_callback: Rc<dyn Fn()>,
}

impl<T: JsTypedArrayItem> PixelReadPassDef<T> {
    /// Create a new pixel-read-pass factory.
    ///
    /// `callback`: Will be evaluated after a successful pixel read action.
    /// `sync_callback`: Will be evaluated at the beginning of a pixel read action.
    ///
    /// Note: The callbacks will not be evaluated on each run of the pass, as the read is
    /// asynchronous and can take longer than a single frame.
    pub fn new(
        position: Uniform<Vector2<i32>>,
        callback: impl 'static + Fn(Vec<T>),
        sync_callback: impl 'static + Fn(),
    ) -> Self {
        let threshold = Rc::new(Cell::new(0));
        let callback = Rc::new(callback);
        let sync_callback = Rc::new(sync_callback);
        Self { position, threshold, callback, sync_callback }
    }

    /// Returns a reference that can be used to set the threshold of how often the pass should be
    /// run. Threshold of 0 means that it will be run every time. Threshold of N means that it will
    /// be only run every N-th call to the `run` function.
    pub fn get_threshold(&self) -> Rc<Cell<usize>> {
        self.threshold.clone()
    }
}

impl<T: JsTypedArrayItem> pass::Definition for PixelReadPassDef<T> {
    fn instantiate(
        &self,
        instance: pass::InstanceInfo,
    ) -> Result<Box<dyn pass::Instance>, ContextLost> {
        Ok(Box::new(PixelReadPass::new(self.clone(), instance)?))
    }
}



// =====================
// === PixelReadPass ===
// =====================

/// Reads the pixel color and stores it in the 'pass_pixel_color' variable.
#[derive(Derivative, Clone, Deref)]
#[derivative(Debug)]
pub struct PixelReadPass<T: JsTypedArrayItem> {
    buffer:          WebGlBuffer,
    framebuffer:     WebGlFramebuffer,
    format:          texture::AnyFormat,
    item_type:       texture::AnyItemType,
    js_array:        JsTypedArray<T>,
    sync:            Option<WebGlSync>,
    since_last_read: usize,
    instance:        pass::InstanceInfo,
    #[deref]
    definition:      PixelReadPassDef<T>,
}

impl<T: JsTypedArrayItem> PixelReadPass<T> {
    /// Constructor.
    pub fn new(
        definition: PixelReadPassDef<T>,
        instance: pass::InstanceInfo,
    ) -> Result<Self, ContextLost> {
        let context = &instance.context;
        let buffer = context.create_buffer()?;
        let js_array = JsTypedArray::<T>::new_with_length(4);
        let target = Context::PIXEL_PACK_BUFFER;
        let usage = Context::DYNAMIC_READ;
        context.bind_buffer(*target, Some(&buffer));
        context.buffer_data_with_opt_array_buffer(*target, Some(&js_array.buffer()), *usage);
        context.bind_buffer(*target, None);
        let texture = match instance.variables.borrow().get("pass_id").unwrap() {
            AnyUniform::Texture(t) => t,
            _ => panic!("Pass internal error. Unmatched types."),
        };
        let texture = texture.texture().ok_or(ContextLost)?;
        let format = texture.get_format();
        let item_type = texture.get_item_type();
        let gl_texture = Some(texture.as_gl_texture());
        let framebuffer = context.create_framebuffer()?;
        let target = Context::FRAMEBUFFER;
        let texture_target = Context::TEXTURE_2D;
        let attachment_point = Context::COLOR_ATTACHMENT0;
        let level = 0;
        context.bind_framebuffer(*target, Some(&framebuffer));
        context.framebuffer_texture_2d(
            *target,
            *attachment_point,
            *texture_target,
            gl_texture,
            level,
        );
        context.bind_framebuffer(*target, None);
        let framebuffer_status = context.check_framebuffer_status(*Context::FRAMEBUFFER);
        if framebuffer_status != *Context::FRAMEBUFFER_COMPLETE {
            warn!("Framebuffer incomplete (status: {framebuffer_status}).")
        }
        Ok(Self {
            buffer,
            framebuffer,
            format,
            item_type,
            js_array,
            sync: default(),
            since_last_read: default(),
            instance,
            definition,
        })
    }

    #[profile(Detail)]
    fn run_not_synced(&mut self) {
        let context = &self.instance.context;
        let position = self.position.get();
        let width = 1;
        let height = 1;
        let format = self.format.to::<GlEnum>().into();
        let typ = self.item_type.to::<GlEnum>().into();
        let offset = 0;
        context.bind_framebuffer(*Context::FRAMEBUFFER, Some(&self.framebuffer));
        context.bind_buffer(*Context::PIXEL_PACK_BUFFER, Some(&self.buffer));
        context
            .read_pixels_with_i32(position.x, position.y, width, height, format, typ, offset)
            .unwrap();
        context.bind_buffer(*Context::PIXEL_PACK_BUFFER, None);
        let condition = Context::SYNC_GPU_COMMANDS_COMPLETE;
        let flags = 0;
        let sync = context.fence_sync(*condition, flags).unwrap();
        self.sync = Some(sync);
    }

    #[profile(Detail)]
    fn check_and_handle_sync(&mut self, sync: &WebGlSync) {
        let context = &self.instance.context;
        let status = context.get_sync_parameter(sync, *Context::SYNC_STATUS);
        if status == *Context::SIGNALED {
            context.delete_sync(Some(sync));
            self.sync = None;
            let target = Context::PIXEL_PACK_BUFFER;
            let offset = 0;
            let buffer_view = self.js_array.to_object();
            context.bind_buffer(*target, Some(&self.buffer));
            context.get_buffer_sub_data_with_i32_and_array_buffer_view(
                *target,
                offset,
                buffer_view,
            );
            context.bind_buffer(*Context::PIXEL_PACK_BUFFER, None);
            (self.callback)(self.js_array.to_vec());
        }
    }
}

impl<T: JsTypedArrayItem> pass::Instance for PixelReadPass<T> {
    fn run(&mut self, update_status: UpdateStatus) {
        if self.since_last_read < self.threshold.get() {
            self.since_last_read += 1;
        } else {
            self.since_last_read = 0;
            if let Some(sync) = self.sync.clone() {
                self.check_and_handle_sync(&sync);
            }
            let need_sync = update_status.scene_was_dirty || update_status.pointer_position_changed;
            if need_sync && self.sync.is_none() {
                self.run_not_synced();
                (self.sync_callback)();
            }
        }
    }

    fn resize(&mut self, width: i32, height: i32, pixel_ratio: f32) {
        let mut instance = self.instance.clone();
        instance.width = width;
        instance.height = height;
        instance.pixel_ratio = pixel_ratio;
        match Self::new(self.definition.clone(), instance) {
            Ok(new) => {
                *self = new;
            }
            Err(ContextLost) => (),
        }
    }
}
