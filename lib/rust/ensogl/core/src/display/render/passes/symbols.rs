//! Pass for rendering all symbols.

use crate::prelude::*;
use crate::system::gpu::*;

use crate::display::render::pass;
use crate::display::scene;
use crate::display::scene::layer;
use crate::display::scene::Scene;
use crate::display::scene::UpdateStatus;
use crate::display::symbol::registry::SymbolRegistry;
use crate::display::symbol::MaskComposer;



// =========================
// === SymbolsRenderPass ===
// =========================

#[derive(Clone, Debug)]
struct Framebuffers {
    composed: pass::Framebuffer,
    mask:     pass::Framebuffer,
    layer:    pass::Framebuffer,
}

impl Framebuffers {
    fn new(composed: pass::Framebuffer, mask: pass::Framebuffer, layer: pass::Framebuffer) -> Self {
        Self { composed, mask, layer }
    }
}

/// Pass for rendering all symbols. The results are stored in the 'color' and 'id' outputs.
#[derive(Clone, Debug)]
pub struct SymbolsRenderPass {
    logger:          Logger,
    symbol_registry: SymbolRegistry,
    layers:          scene::HardcodedLayers,
    framebuffers:    Option<Framebuffers>,
    scene:           Scene,
    mask_composer:   MaskComposer,
}

impl SymbolsRenderPass {
    /// Constructor.
    pub fn new(
        logger: impl AnyLogger,
        scene: &Scene,
        symbol_registry: &SymbolRegistry,
        layers: &scene::HardcodedLayers,
    ) -> Self {
        let logger = Logger::new_sub(logger, "SymbolsRenderPass");
        let symbol_registry = symbol_registry.clone_ref();
        let layers = layers.clone_ref();
        let framebuffers = default();
        let scene = scene.clone_ref();
        let mask_composer =
            MaskComposer::new(&scene, "pass_mask_color", "pass_layer_color", "pass_layer_id");
        Self { logger, symbol_registry, layers, framebuffers, scene, mask_composer }
    }
}

impl pass::Definition for SymbolsRenderPass {
    fn initialize(&mut self, instance: &pass::Instance) {
        let rgba = texture::Rgba;
        let tex_type = texture::item_type::u8;
        let id_params = texture::Parameters {
            min_filter: texture::MinFilter::NEAREST,
            mag_filter: texture::MagFilter::NEAREST,
            ..default()
        };

        let out_color = pass::OutputDefinition::new_rgba("color");
        let out_id = pass::OutputDefinition::new("id", rgba, tex_type, id_params);
        let tex_color = instance.new_screen_texture(&out_color);
        let tex_id = instance.new_screen_texture(&out_id);
        let composed_fb = instance.new_framebuffer(&[&tex_color, &tex_id]);

        let out_mask_color = pass::OutputDefinition::new_rgba("mask_color");
        let out_mask_id = pass::OutputDefinition::new("mask_id", rgba, tex_type, id_params);
        let tex_mask_color = instance.new_screen_texture(&out_mask_color);
        let tex_mask_id = instance.new_screen_texture(&out_mask_id);
        let mask_fb = instance.new_framebuffer(&[&tex_mask_color, &tex_mask_id]);

        let out_layer_color = pass::OutputDefinition::new_rgba("layer_color");
        let out_layer_id = pass::OutputDefinition::new("layer_id", rgba, tex_type, id_params);
        let tex_layer_color = instance.new_screen_texture(&out_layer_color);
        let tex_layer_id = instance.new_screen_texture(&out_layer_id);
        let layer_fb = instance.new_framebuffer(&[&tex_layer_color, &tex_layer_id]);

        self.framebuffers = Some(Framebuffers::new(composed_fb, mask_fb, layer_fb));
    }

    fn run(&mut self, instance: &pass::Instance, update_status: UpdateStatus) {
        if update_status.scene_was_dirty {
            let framebuffers = self.framebuffers.as_ref().unwrap();

            framebuffers.composed.bind();

            let arr = vec![0.0, 0.0, 0.0, 0.0];
            instance.context.clear_bufferfv_with_f32_array(*Context::COLOR, 0, &arr);
            instance.context.clear_bufferfv_with_f32_array(*Context::COLOR, 1, &arr);

            let mut scissor_stack = default();
            self.render_layer(instance, &self.layers.root.clone(), &mut scissor_stack, false);
            if !scissor_stack.is_empty() {
                warning!(
                    &self.logger,
                    "The scissor stack was not cleaned properly. \
                This is an internal bug that may lead to visual artifacts. Please report it."
                );
            }
            instance.context.bind_framebuffer(*Context::FRAMEBUFFER, None);
        }
    }
}

impl SymbolsRenderPass {
    fn enable_scissor_test(&self, instance: &pass::Instance) {
        instance.context.enable(*Context::SCISSOR_TEST);
    }

    fn disable_scissor_test(&self, instance: &pass::Instance) {
        instance.context.disable(*Context::SCISSOR_TEST);
    }

    fn render_layer(
        &mut self,
        instance: &pass::Instance,
        layer: &layer::Layer,
        scissor_stack: &mut Vec<layer::ScissorBox>,
        parent_masked: bool,
    ) {
        let framebuffers = self.framebuffers.as_ref().unwrap();
        let parent_scissor_box = scissor_stack.first().copied();
        let layer_scissor_box = layer.scissor_box();
        let scissor_box = parent_scissor_box.concat(layer_scissor_box);
        let scissor_box_changed = layer_scissor_box.is_some();
        let first_scissor_usage = scissor_box_changed && parent_scissor_box.is_none();
        if let Some(scissor_box) = scissor_box {
            if scissor_box_changed {
                if first_scissor_usage {
                    self.enable_scissor_test(instance)
                }
                scissor_stack.push(scissor_box);
                let position = scissor_box.position();
                let size = scissor_box.size();
                instance.context.scissor(position.x, position.y, size.x, size.y);
            }
        }

        let layer_mask = layer.mask();
        let is_masked = layer_mask.is_some();
        let was_ever_masked = is_masked || parent_masked;
        let nested_masking = is_masked && parent_masked;

        if nested_masking {
            warning!(
                &self.logger,
                "Nested layer masking is not supported yet. Skipping nested masks."
            );
        } else if let Some(mask) = layer_mask {
            framebuffers.mask.bind();
            let arr = vec![0.0, 0.0, 0.0, 0.0];
            instance.context.clear_bufferfv_with_f32_array(*Context::COLOR, 0, &arr);
            instance.context.clear_bufferfv_with_f32_array(*Context::COLOR, 1, &arr);
            self.render_layer(instance, &mask, scissor_stack, was_ever_masked);

            let framebuffers = self.framebuffers.as_ref().unwrap();
            framebuffers.layer.bind();
            let arr = vec![0.0, 0.0, 0.0, 0.0];
            instance.context.clear_bufferfv_with_f32_array(*Context::COLOR, 0, &arr);
            instance.context.clear_bufferfv_with_f32_array(*Context::COLOR, 1, &arr);
        }

        self.symbol_registry.set_camera(&layer.camera());
        self.symbol_registry.render_by_ids(&layer.symbols());
        for sublayer in layer.sublayers().iter() {
            self.render_layer(instance, sublayer, scissor_stack, was_ever_masked);
        }

        if is_masked {
            let framebuffers = self.framebuffers.as_ref().unwrap();
            framebuffers.composed.bind();
            self.mask_composer.render();
        }

        if scissor_box_changed {
            scissor_stack.pop();
            if first_scissor_usage {
                self.disable_scissor_test(instance)
            }
        }
    }
}
