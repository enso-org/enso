//! Pass for rendering all symbols.

use crate::prelude::*;
use crate::system::gpu::*;

use crate::display::render::pass;
use crate::display::scene;
use crate::display::scene::layer;
use crate::display::scene::UpdateStatus;
use crate::display::symbol::MaskComposer;
use crate::display::symbol::OverlayComposer;
use crate::display::world;
use crate::system::gpu::context::ContextLost;



// ============================
// === SymbolsRenderPassDef ===
// ============================

/// Defines a pass for rendering symbols. See [`SymbolRenderPass`] for pass implementation.
#[derive(Debug, Clone)]
pub struct SymbolsRenderPassDef {
    layers:           scene::HardcodedLayers,
    mask_composer:    MaskComposer,
    overlay_composer: OverlayComposer,
}

impl SymbolsRenderPassDef {
    /// Constructor.
    pub fn new(layers: &scene::HardcodedLayers) -> Self {
        let layers = layers.clone_ref();
        let mask_composer =
            MaskComposer::new("pass_mask_color", "pass_layer_color", "pass_layer_id");
        let overlay_composer = OverlayComposer::new("pass_layer_color", "pass_layer_id");
        Self { layers, mask_composer, overlay_composer }
    }
}

impl pass::Definition for SymbolsRenderPassDef {
    fn instantiate(
        &self,
        instance: pass::InstanceInfo,
    ) -> Result<Box<dyn pass::Instance>, ContextLost> {
        Ok(Box::new(SymbolsRenderPass {
            framebuffers: Framebuffers::new(&instance)?,
            def: self.clone(),
            context: instance.context.clone(),
            instance,
        }))
    }
}



// =========================
// === SymbolsRenderPass ===
// =========================

/// Pass for rendering all symbols. The results are stored in the 'color' and 'id' outputs.
#[derive(Debug, Deref)]
pub struct SymbolsRenderPass {
    framebuffers: Framebuffers,
    #[deref]
    def:          SymbolsRenderPassDef,
    context:      Context,
    instance:     pass::InstanceInfo,
}

impl pass::Instance for SymbolsRenderPass {
    fn run(&mut self, update_status: UpdateStatus) {
        if update_status.scene_was_dirty {
            self.framebuffers.composed.bind();

            let arr = vec![0.0, 0.0, 0.0, 0.0];
            self.context.clear_bufferfv_with_f32_array(*Context::COLOR, 0, &arr);
            self.context.clear_bufferfv_with_f32_array(*Context::COLOR, 1, &arr);

            let mut scissor_stack = default();
            self.render_layer(&self.layers.root.clone(), &mut scissor_stack, false, None);
            if !scissor_stack.is_empty() {
                warn!(
                    "The scissor stack was not cleaned properly. \
                This is an internal bug that may lead to visual artifacts. Please report it."
                );
            }
            self.context.bind_framebuffer(*Context::FRAMEBUFFER, None);
        }
    }

    fn resize(&mut self, width: i32, height: i32, pixel_ratio: f32) {
        let mut instance = self.instance.clone();
        instance.width = width;
        instance.height = height;
        instance.pixel_ratio = pixel_ratio;
        match Framebuffers::new(&instance) {
            Ok(framebuffers) => {
                *self = SymbolsRenderPass {
                    framebuffers,
                    def: self.def.clone(),
                    context: self.context.clone(),
                    instance: self.instance.clone(),
                };
            }
            Err(ContextLost) => (),
        }
    }
}

impl SymbolsRenderPass {
    fn enable_scissor_test(&self) {
        self.context.enable(*Context::SCISSOR_TEST);
    }

    fn disable_scissor_test(&self) {
        self.context.disable(*Context::SCISSOR_TEST);
    }

    fn render_layer(
        &mut self,
        layer: &layer::Layer,
        scissor_stack: &mut Vec<layer::ScissorBox>,
        parent_masked: bool,
        override_blend: Option<layer::BlendMode>,
    ) {
        self.try_render_layer(layer, scissor_stack, parent_masked, override_blend);
    }

    // Render a layer if we are currently able to render. This should not fail unless the context
    // has been lost.
    fn try_render_layer(
        &mut self,
        layer: &layer::Layer,
        scissor_stack: &mut Vec<layer::ScissorBox>,
        parent_masked: bool,
        override_blend: Option<layer::BlendMode>,
    ) -> Option<()> {
        let has_symbols = layer.has_symbols();
        if !has_symbols && !layer.has_main_pass_sublayers() {
            return Some(());
        }

        let parent_scissor_box = scissor_stack.first().copied();
        let layer_scissor_box = layer.scissor_box();
        let scissor_box = parent_scissor_box.concat(layer_scissor_box);
        let scissor_box_changed = layer_scissor_box.is_some();
        let first_scissor_usage = scissor_box_changed && parent_scissor_box.is_none();
        if let Some(scissor_box) = scissor_box {
            if scissor_box_changed {
                if first_scissor_usage {
                    self.enable_scissor_test()
                }
                scissor_stack.push(scissor_box);
                let position = scissor_box.position();
                let size = scissor_box.size();
                self.context.scissor(position.x, position.y, size.x, size.y);
            }
        }

        let layer_mask = layer.mask();
        let is_masked = layer_mask.is_some();
        let was_ever_masked = is_masked || parent_masked;
        let nested_masking = is_masked && parent_masked;

        let layer_mask = layer_mask.filter(|_| !nested_masking);
        if nested_masking {
            warn!("Nested layer masking is not supported yet. Skipping nested masks.");
        }

        if let Some(layer::Mask { layer, inverted }) = layer_mask.as_ref() {
            let zero = [0.0, 0.0, 0.0, 0.0];

            if !inverted {
                self.framebuffers.mask.bind();
                self.context.clear_bufferfv_with_f32_array(*Context::COLOR, 0, &zero);
                self.context.clear_bufferfv_with_f32_array(*Context::COLOR, 1, &zero);
                self.render_layer(layer, scissor_stack, was_ever_masked, override_blend);
            }

            self.framebuffers.layer.bind();
            self.context.clear_bufferfv_with_f32_array(*Context::COLOR, 0, &zero);
            self.context.clear_bufferfv_with_f32_array(*Context::COLOR, 1, &zero);
        }

        if has_symbols {
            world::with_context(|t| {
                t.set_camera(&layer.camera());
                let blend_mode = override_blend.unwrap_or_else(|| layer.blend_mode());
                blend_mode.apply_to_context(&self.context);
                t.render_symbols(&layer.symbols());
            });
        }

        layer.for_each_sublayer(|layer| {
            if layer.flags.contains(layer::LayerFlags::MAIN_PASS_VISIBLE) {
                self.render_layer(&layer, scissor_stack, was_ever_masked, override_blend);
            }
        });

        if let Some(layer::Mask { layer: mask_layer, inverted: true }) = layer_mask.as_ref() {
            let blend = layer::BlendMode::ALPHA_CUTOUT;
            self.render_layer(mask_layer, scissor_stack, was_ever_masked, Some(blend));
            self.framebuffers.composed.bind();
            layer::BlendMode::PREMULTIPLIED_ALPHA_OVER.apply_to_context(&self.context);
            self.overlay_composer.render();
        } else if is_masked {
            self.framebuffers.composed.bind();
            layer::BlendMode::PREMULTIPLIED_ALPHA_OVER.apply_to_context(&self.context);
            self.mask_composer.render();
        }

        if scissor_box_changed {
            scissor_stack.pop();
            if first_scissor_usage {
                self.disable_scissor_test()
            }
        }
        Some(())
    }
}


// === Framebuffers ===

#[derive(Debug)]
struct Framebuffers {
    composed: pass::Framebuffer,
    mask:     pass::Framebuffer,
    layer:    pass::Framebuffer,
}

impl Framebuffers {
    fn new(instance: &pass::InstanceInfo) -> Result<Self, ContextLost> {
        let rgba = texture::Rgba8;
        let tex_type = texture::item_type::u8;
        let id_params = texture::Parameters {
            min_filter: texture::MinFilter::NEAREST,
            mag_filter: texture::MagFilter::NEAREST,
            ..default()
        };
        let framebuffer = |color, id| {
            let out_color = pass::OutputDefinition::new_rgba(color);
            let out_id = pass::OutputDefinition::new(id, rgba, tex_type, id_params);
            let tex_color = instance.new_screen_texture(&out_color);
            let tex_id = instance.new_screen_texture(&out_id);
            instance.new_framebuffer(&[&tex_color, &tex_id])
        };
        let composed = framebuffer("color", "id")?;
        let mask = framebuffer("mask_color", "mask_id")?;
        let layer = framebuffer("layer_color", "layer_id")?;
        Ok(Self { composed, mask, layer })
    }
}
