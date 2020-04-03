//! Pass for rendering all symbols.

use crate::prelude::*;

use crate::display::render::pipeline::*;
use crate::system::gpu::*;
use crate::display::symbol::registry::SymbolRegistry;
use crate::display::scene;



// =========================
// === SymbolsRenderPass ===
// =========================

/// Pass for rendering all symbols. The results are stored in a `'color'` variable.
#[derive(Clone,Debug)]
pub struct SymbolsRenderPass {
    target : SymbolRegistry,
    views  : scene::Views,
}

impl SymbolsRenderPass {
    /// Constructor.
    pub fn new(target:&SymbolRegistry, views:&scene::Views) -> Self {
        let target = target.clone_ref();
        let views  = views.clone_ref();
        Self {target,views}
    }
}

impl RenderPass for SymbolsRenderPass {
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
        self.target.set_camera(&self.views.main.camera);
        self.target.render_by_ids(&self.views.main.symbols());
    }
}



// ==========================
// === SymbolsRenderPass2 ===
// ==========================

// FIXME
// This is a hack. We are using `SymbolsRenderPass` to render the first scene view, and
// `SymbolsRenderPass2` to render the rest of views. It is not used for the needs of first rendering
// scene symbols, then reading pixel under cursor, and then rendering cursor. It should be unified
// and refactored to a better solution instead.

/// Pass for rendering all symbols. The results are stored in a `'color'` variable.
#[derive(Clone,Debug)]
pub struct SymbolsRenderPass2 {
    target : SymbolRegistry,
    views  : scene::Views,
}

impl SymbolsRenderPass2 {
    /// Constructor.
    pub fn new(target:&SymbolRegistry, views:&scene::Views) -> Self {
        let target = target.clone_ref();
        let views  = views.clone_ref();
        Self {target,views}
    }
}

impl RenderPass for SymbolsRenderPass2 {
    fn run(&mut self, _:&Context, _:&UniformScope) {
        let views = &self.views.all()[1..];
        for view in views {
            view.upgrade().for_each(|v| {
                self.target.set_camera(&v.camera);
                self.target.render_by_ids(&v.symbols());
            })
        }
    }
}
