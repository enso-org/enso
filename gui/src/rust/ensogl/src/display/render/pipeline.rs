//! This module defines a render pipeline and render passes.

use crate::prelude::*;

use crate::system::gpu::types::*;
use crate::system::gpu::texture;



// =======================
// === Render Pipeline ===
// =======================

shared! { RenderPipeline
/// The pipeline is a set of subsequent passes which can consume and produce data. Please note that
/// although passes are run sequentially, their dependency graph (data passing graph) can be DAG.
#[derive(Debug,Default)]
pub struct RenderPipelineData {
    passes: Vec<Box<dyn RenderPass>>
}

impl {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Getter.
    pub fn passes_clone(&self) -> Vec<Box<dyn RenderPass>> {
        self.passes.clone()
    }
}}

impl<Pass:RenderPass> Add<Pass> for RenderPipeline {
    type Output = Self;
    fn add(self, pass:Pass) -> Self::Output {
        let pass = Box::new(pass);
        self.rc.borrow_mut().passes.push(pass);
        self
    }
}



// ========================
// === RenderPassOutput ===
// ========================

/// An output definition of a render pass. The output describes a format of framebuffer attachment,
/// which will be the result of running the current pass.
#[derive(Debug)]
pub struct RenderPassOutput {
    /// Name of the pass.
    pub name : String,
    /// Internal texture format of the pass framebuffer's attachment.
    pub internal_format : texture::AnyInternalFormat,
    /// Item texture type of the pass framebuffer's attachment.
    pub item_type : texture::AnyItemType,
    /// Texture parameters that will be set when binding the texture.
    pub texture_parameters : texture::Parameters,
}

impl RenderPassOutput {
    /// Constructor.
    pub fn new<Name:Str,F:Into<texture::AnyInternalFormat>,T:Into<texture::AnyItemType>>
    (name:Name, internal_format:F, item_type:T, texture_parameters:texture::Parameters) -> Self {
        let name            = name.into();
        let internal_format = internal_format.into();
        let item_type       = item_type.into();
        Self {name,internal_format,item_type,texture_parameters}
    }

    /// Getter.
    pub fn name(&self) -> &str {
        &self.name
    }
}



// ==================
// === RenderPass ===
// ==================

/// Generalization of render passes.
pub trait RenderPass : CloneBoxedForRenderPass + Debug + 'static {
    /// The outputs of this pass. If empty, the pass will draw to screen.
    fn outputs(&self) -> Vec<RenderPassOutput> { default() }

    /// Run the current render pass with a reference to global variables object. Render passes are
    /// allowed to read and write values while running. The values will be accessible to subsequent
    /// passes.
    fn run(&mut self, context:&Context, variables:&UniformScope);
}

clone_boxed!(RenderPass);
