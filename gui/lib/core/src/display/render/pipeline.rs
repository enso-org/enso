//! This module defines a render pipeline and render passes.

use crate::prelude::*;

use crate::system::gpu::types::*;



// =======================
// === Render Pipeline ===
// =======================

/// The pipeline is a set of subsequent passes which can consume and produce data. Please note that
/// although passes are run sequentially, their dependency graph (data passing graph) can be DAG.
#[derive(Debug,Default)]
pub struct RenderPipeline {
    passes: Vec<Box<dyn RenderPass>>
}

impl RenderPipeline {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Getter.
    pub fn passes(&self) -> &Vec<Box<dyn RenderPass>> {
        &self.passes
    }
}

impl<Pass:RenderPass> Add<Pass> for RenderPipeline {
    type Output = Self;

    fn add(mut self, pass:Pass) -> Self::Output {
        let pass = Box::new(pass);
        self.passes.push(pass);
        self
    }
}



// ========================
// === RenderPassOutput ===
// ========================

/// An output definition of a render pass. The output describes a format of framebuffer attachment,
/// which will be the result of running the current pass.
pub struct RenderPassOutput {
    name            : String,
    internal_format : texture::AnyInternalFormat,
}

impl RenderPassOutput {
    /// Constructor.
    pub fn new<Name:Str>(name:Name, internal_format:texture::AnyInternalFormat) -> Self {
        let name = name.into();
        Self {name,internal_format}
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
