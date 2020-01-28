//! Pass allowing rendering a chosen display object.

use crate::prelude::*;

use crate::display::object::DisplayObjectData;
use crate::display::render::pipeline::*;
use crate::system::gpu::*;



// ===============================
// === DisplayObjectRenderPass ===
// ===============================

/// Pass allowing rendering a chosen display object. The results are stored in a `'color'` variable.
#[derive(Clone,Debug)]
pub struct DisplayObjectRenderPass {
    target: DisplayObjectData
}

impl DisplayObjectRenderPass {
    /// Constructor.
    pub fn new(target:&DisplayObjectData) -> Self {
        let target = target.clone_ref();
        Self {target}
    }
}

impl RenderPass for DisplayObjectRenderPass {
    fn outputs(&self) -> Vec<RenderPassOutput> {
        vec![ RenderPassOutput::new("color",texture::Rgba,texture::item_type::u8)
            , RenderPassOutput::new("id",texture::Rgba32ui,texture::item_type::u32)
            ]
    }

    fn run(&mut self, context:&Context, _:&UniformScope) {
        let arr = vec![0.0,0.0,0.0,0.0];
        let arr2 = vec![0,0,0,0];
        context.clear_bufferfv_with_f32_array(Context::COLOR,0,&arr);
        context.clear_bufferuiv_with_u32_array(Context::COLOR,1,&arr2);
        self.target.render();
    }
}
