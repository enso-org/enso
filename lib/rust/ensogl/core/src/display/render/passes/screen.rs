//! Pass rendering directly to the screen.

use crate::prelude::*;

use crate::display::render::pass;
use crate::display::scene::Scene;
use crate::display::symbol::Screen;



// ========================
// === ScreenRenderPass ===
// ========================

/// Renders the last `'color'` variable to the screen.
#[derive(Clone, Debug)]
pub struct ScreenRenderPass {
    screen: Screen,
}

impl ScreenRenderPass {
    /// Constructor.
    pub fn new(scene: &Scene) -> Self {
        let screen = Screen::new_identity_painter(scene, "pass_color");
        Self { screen }
    }
}

impl pass::Definition for ScreenRenderPass {
    fn run(&mut self, _: &pass::Instance) {
        self.screen.render();
    }
}
