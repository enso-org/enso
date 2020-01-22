//! Pass rendering directly to the screen.

use crate::prelude::*;

use crate::display::render::pipeline::*;
use crate::display::symbol::Screen;
use crate::system::gpu::*;



// ========================
// === ScreenRenderPass ===
// ========================

/// Renders the last `'color'` variable to the screen.
#[derive(Clone,Debug,Default)]
pub struct ScreenRenderPass {
    screen: Screen,
}

impl ScreenRenderPass {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }
}

impl RenderPass for ScreenRenderPass {
    fn run(&mut self, _:&Context, _:&UniformScope) {
        self.screen.render();
    }
}
