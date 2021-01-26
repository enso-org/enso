//! Pass rendering directly to the screen.

use crate::prelude::*;

use crate::display::render::pipeline::*;
use crate::display::symbol::Screen;
use crate::display::world::World;
use crate::system::gpu::*;



// ========================
// === ScreenRenderPass ===
// ========================

/// Renders the last `'color'` variable to the screen.
#[derive(Clone,Debug)]
pub struct ScreenRenderPass {
    screen: Screen,
}

impl ScreenRenderPass {
    /// Constructor.
    pub fn new(world:&World) -> Self {
        let screen = Screen::new(world);
        screen.hide();
        Self {screen}
    }
}

impl RenderPass for ScreenRenderPass {
    /// Please note that we show the screen only for the moment of it's rendering. This allows us to
    /// be sure that other passes will not render it. Otherwise this could cause serious WebGL
    /// errors, as it may cause a situation when other pass is trying to render to a texture all
    /// symbols (including this one), while this symbol would need this texture to render itself.
    fn run(&mut self, _:&Context, _:&UniformScope) {
        self.screen.show();
        self.screen.render();
        self.screen.hide();
    }
}
