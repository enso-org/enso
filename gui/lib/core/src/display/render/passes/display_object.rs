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
        vec![RenderPassOutput::new("color",texture::AnyInternalFormat::Rgba)]
    }

    fn run(&mut self, context:&Context, _:&UniformScope) {
        context.clear_color(0.0, 0.0, 0.0, 1.0);
        context.clear(Context::COLOR_BUFFER_BIT);
        self.target.render();
    }
}
