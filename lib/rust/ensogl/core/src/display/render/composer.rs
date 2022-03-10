//! Render composer definition, which is a render pipeline bound to a specific context.

use crate::display::render::pipeline::*;
use crate::prelude::*;
use crate::system::gpu::*;

use crate::display::render::pass;



// ================
// === Composer ===
// ================

shared! { Composer
/// Render composer is a render pipeline bound to a specific context.
#[derive(Debug)]
pub struct ComposerModel {
    pipeline  : Pipeline,
    passes    : Vec<ComposerPass>,
    variables : UniformScope,
    context   : Context,
    width     : i32,
    height    : i32,
}

impl {
    /// Constructor
    pub fn new
    ( pipeline  : &Pipeline
    , context   : &Context
    , variables : &UniformScope
    , width     : i32
    , height    : i32
    ) -> Self {
        let pipeline  = pipeline.clone_ref();
        let passes    = default();
        let context   = context.clone();
        let variables = variables.clone_ref();
        let mut this  = Self {pipeline,passes,variables,context,width,height};
        this.init_passes();
        this
    }

    /// Set a new pipeline for this composer.
    pub fn set_pipeline(&mut self, pipeline:&Pipeline) {
        self.pipeline = pipeline.clone_ref();
        self.init_passes();
    }

    /// Resize the composer and reinitialize all of its layers.
    pub fn resize(&mut self, width:i32, height:i32) {
        self.width  = width;
        self.height = height;
        self.init_passes();
    }

    /// Initialize all pass definitions from the [`Pipeline`].
    fn init_passes(&mut self) {
        let ctx    = &self.context;
        let vars   = &self.variables;
        let width  = self.width;
        let height = self.height;
        let defs   = self.pipeline.passes_clone();
        let passes = defs.into_iter().map(|pass| ComposerPass::new(ctx,vars,pass,width,height));
        self.passes = passes.collect_vec();
    }

    /// Run all the registered passes in this composer.
    pub fn run(&mut self) {
        for pass in &mut self.passes {
            pass.run();
        }
    }
}}



// ====================
// === ComposerPass ===
// ====================

/// A `pass::Definition` bound to a specific rendering context.
#[derive(Derivative)]
#[derivative(Debug)]
struct ComposerPass {
    #[derivative(Debug = "ignore")]
    pass:     Box<dyn pass::Definition>,
    instance: pass::Instance,
}

impl Deref for ComposerPass {
    type Target = pass::Instance;
    fn deref(&self) -> &Self::Target {
        &self.instance
    }
}

impl DerefMut for ComposerPass {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.instance
    }
}

impl ComposerPass {
    /// Constructor
    #[allow(clippy::borrowed_box)]
    pub fn new(
        context: &Context,
        variables: &UniformScope,
        mut pass: Box<dyn pass::Definition>,
        width: i32,
        height: i32,
    ) -> Self {
        let instance = pass::Instance::new(context, variables, width, height);
        pass.initialize(&instance);
        Self { pass, instance }
    }

    /// Run the pass.
    pub fn run(&mut self) {
        self.pass.run(&self.instance);
    }
}
