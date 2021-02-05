//! Pass for rendering all symbols.

use crate::prelude::*;

use crate::display::render::pipeline::*;
use crate::system::gpu::*;
use crate::display::symbol::registry::SymbolRegistry;
use crate::display::scene;



// =========================
// === SymbolsRenderPass ===
// =========================

/// Pass for rendering all symbols. The results are stored in the 'color' and 'id' outputs.
#[derive(Clone,Debug)]
pub struct SymbolsRenderPass {
    target : SymbolRegistry,
    layers : scene::HardcodedLayers,
}

impl SymbolsRenderPass {
    /// Constructor.
    pub fn new(target:&SymbolRegistry, layers:&scene::HardcodedLayers) -> Self {
        let target = target.clone_ref();
        let layers = layers.clone_ref();
        Self {target,layers}
    }
}

impl RenderPass for SymbolsRenderPass {
    fn outputs(&self) -> Vec<RenderPassOutput> {
        let color_parameters = texture::Parameters::default();
        let id_parameters    = texture::Parameters {
            min_filter : texture::MinFilter::Nearest,
            mag_filter : texture::MagFilter::Nearest,
            ..default()
        };
        vec![ RenderPassOutput::new("color" , texture::Rgba,texture::item_type::u8,color_parameters)
            , RenderPassOutput::new("id"    , texture::Rgba,texture::item_type::u8,id_parameters)
            ]
    }

    fn run(&mut self, context:&Context, _:&UniformScope) {
        let arr = vec![0.0,0.0,0.0,0.0];
        context.clear_bufferfv_with_f32_array(Context::COLOR,0,&arr);
        context.clear_bufferfv_with_f32_array(Context::COLOR,1,&arr);

        for layer in self.layers.all().iter() {
            self.target.set_camera(&layer.camera());
            let symbols = layer.symbols();
            self.target.render_by_ids(&symbols);
        }
    }
}
