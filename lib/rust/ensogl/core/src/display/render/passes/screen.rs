//! Pass rendering directly to the screen.

use crate::prelude::*;

use crate::display::render::pass;
use crate::display::scene::UpdateStatus;
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
    pub fn new() -> Self {
        let screen = Screen::new_identity_painter("pass_color");
        Self { screen }
    }
}

impl Default for ScreenRenderPass {
    fn default() -> Self {
        Self::new()
    }
}

impl pass::Definition for ScreenRenderPass {
    fn run(&mut self, _: &pass::Instance, update_status: UpdateStatus) {
        if update_status.scene_was_dirty {
            self.screen.render();
        }
    }
}
