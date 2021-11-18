//! Pass reading pixels from a previously created framebuffer.

use crate::prelude::*;

use crate::display::render::pass;
use crate::system::gpu::data::texture::class::TextureOps;
use crate::system::gpu::*;
use crate::system::js::*;

use web_sys::WebGlBuffer;
use web_sys::WebGlFramebuffer;
use web_sys::WebGlSync;



// =========================
// === PixelReadPassData ===
// =========================

/// Internal state for the `PixelReadPass`.
#[derive(Clone, Debug)]
pub struct PixelReadPassData<T: JsTypedArrayItem> {
    buffer:      WebGlBuffer,
    framebuffer: WebGlFramebuffer,
    format:      texture::AnyFormat,
    item_type:   texture::AnyItemType,
    js_array:    JsTypedArray<T>,
}

impl<T: JsTypedArrayItem> PixelReadPassData<T> {
    /// Constructor.
    pub fn new(
        buffer: WebGlBuffer,
        framebuffer: WebGlFramebuffer,
        format: texture::AnyFormat,
        item_type: texture::AnyItemType,
        js_array: JsTypedArray<T>,
    ) -> Self {
        Self { buffer, framebuffer, format, item_type, js_array }
    }
}



// =====================
// === PixelReadPass ===
// =====================

/// Reads the pixel color and stores it in the 'pass_pixel_color' variable.
#[derive(Derivative, Clone)]
#[derivative(Debug)]
pub struct PixelReadPass<T: JsTypedArrayItem> {
    data:         Option<PixelReadPassData<T>>,
    sync:         Option<WebGlSync>,
    position:     Uniform<Vector2<i32>>,
    threshold:    usize,
    to_next_read: usize,
    #[derivative(Debug = "ignore")]
    callback:     Option<Rc<dyn Fn(Vec<T>)>>,
}

impl<T: JsTypedArrayItem> PixelReadPass<T> {
    /// Constructor.
    pub fn new(position: &Uniform<Vector2<i32>>) -> Self {
        let data = default();
        let sync = default();
        let position = position.clone_ref();
        let callback = default();
        let threshold = 0;
        let to_next_read = 0;
        Self { data, sync, position, threshold, to_next_read, callback }
    }

    /// Sets a callback which will be evaluated after a successful pixel read action. Please note
    /// that it will not be evaluated after each run of this pass, as the read is performed in an
    /// asynchronous fashion and can take longer than a single frame.
    pub fn set_callback<F: Fn(Vec<T>) + 'static>(&mut self, f: F) {
        self.callback = Some(Rc::new(f));
    }

    /// Sets a threshold of how often the pass should be run. Threshold of 0 means that it will be
    /// run every time. Threshold of N means that it will be only run every N-th call to the `run`
    /// function.
    pub fn set_threshold(&mut self, threshold: usize) {
        self.threshold = threshold;
    }

    fn init_if_fresh(&mut self, context: &Context, variables: &UniformScope) {
        if self.data.is_none() {
            let buffer = context.create_buffer().unwrap();
            let js_array = JsTypedArray::<T>::new_with_length(4);
            let target = Context::PIXEL_PACK_BUFFER;
            let usage = Context::DYNAMIC_READ;
            context.bind_buffer(target, Some(&buffer));
            context.buffer_data_with_opt_array_buffer(target, Some(&js_array.buffer()), usage);

            let texture = match variables.get("pass_id").unwrap() {
                uniform::AnyUniform::Texture(t) => t,
                _ => panic!("Pass internal error. Unmatched types."),
            };
            let format = texture.get_format();
            let item_type = texture.get_item_type();
            let gl_texture = texture.gl_texture();
            let framebuffer = context.create_framebuffer().unwrap();
            let target = Context::FRAMEBUFFER;
            let texture_target = Context::TEXTURE_2D;
            let attachment_point = Context::COLOR_ATTACHMENT0;
            let gl_texture = Some(&gl_texture);
            let level = 0;
            context.bind_framebuffer(target, Some(&framebuffer));
            context.framebuffer_texture_2d(
                target,
                attachment_point,
                texture_target,
                gl_texture,
                level,
            );

            let data = PixelReadPassData::new(buffer, framebuffer, format, item_type, js_array);
            self.data = Some(data);
        }
    }

    fn run_not_synced(&mut self, context: &Context) {
        let data = self.data.as_ref().unwrap();
        let position = self.position.get();
        let width = 1;
        let height = 1;
        let format = data.format.to::<GlEnum>().into();
        let typ = data.item_type.to::<GlEnum>().into();
        let offset = 0;
        context.bind_framebuffer(Context::FRAMEBUFFER, Some(&data.framebuffer));
        context.bind_buffer(Context::PIXEL_PACK_BUFFER, Some(&data.buffer));
        context
            .read_pixels_with_i32(position.x, position.y, width, height, format, typ, offset)
            .unwrap();
        let condition = Context::SYNC_GPU_COMMANDS_COMPLETE;
        let flags = 0;
        let sync = context.fence_sync(condition, flags).unwrap();
        self.sync = Some(sync);
        context.flush();
    }

    fn check_and_handle_sync(&mut self, context: &Context, sync: &WebGlSync) {
        let data = self.data.as_ref().unwrap();
        let status = context.get_sync_parameter(sync, Context::SYNC_STATUS);
        if status == Context::SIGNALED {
            context.delete_sync(Some(sync));
            self.sync = None;
            let target = Context::PIXEL_PACK_BUFFER;
            let offset = 0;
            let buffer_view = data.js_array.to_object();
            context.bind_buffer(target, Some(&data.buffer));
            context.get_buffer_sub_data_with_i32_and_array_buffer_view(target, offset, buffer_view);
            if let Some(f) = &self.callback {
                f(data.js_array.to_vec());
            }
        }
    }
}

impl<T: JsTypedArrayItem> pass::Definition for PixelReadPass<T> {
    fn run(&mut self, instance: &pass::Instance) {
        if self.to_next_read > 0 {
            self.to_next_read -= 1;
        } else {
            self.to_next_read = self.threshold;
            self.init_if_fresh(&instance.context, &instance.variables);
            if let Some(sync) = self.sync.clone() {
                self.check_and_handle_sync(&instance.context, &sync);
            }
            if self.sync.is_none() {
                self.run_not_synced(&instance.context);
            }
        }
    }
}
