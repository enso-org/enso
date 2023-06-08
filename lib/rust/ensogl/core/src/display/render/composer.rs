//! Render composer definition, which is a render pipeline bound to a specific context.

use crate::display::render::pipeline::*;
use crate::prelude::*;
use crate::system::gpu::*;

use crate::display::render::pass;
use crate::display::scene::UpdateStatus;



// ================
// === Composer ===
// ================

shared! { Composer
/// Render composer is a render pipeline bound to a specific context.
#[derive(Debug)]
pub struct ComposerModel {
    pipeline : Pipeline,
    passes : Vec<ComposerPass>,
    variables : UniformScope,
    context : Context,
    width : i32,
    height : i32,
    pixel_ratio : f32,
}

impl {
    /// Constructor
    pub fn new
    ( pipeline: &Pipeline
    , context: &Context
    , variables: &UniformScope
    , width: i32
    , height: i32
    , pixel_ratio: f32
    ) -> Self {
        let pipeline  = pipeline.clone_ref();
        let passes    = default();
        let context   = context.clone();
        let variables = variables.clone_ref();
        let mut this  = Self {pipeline, passes, variables, context, width, height, pixel_ratio};
        this.init_passes();
        this
    }

    /// Set a new pipeline for this composer.
    pub fn set_pipeline(&mut self, pipeline:&Pipeline) {
        self.pipeline = pipeline.clone_ref();
        self.init_passes();
    }

    /// Resize the composer and reinitialize all of its screen-size-dependent layers.
    pub fn resize(&mut self, width: i32, height: i32, pixel_ratio: f32) {
        if width == self.width && height == self.height && pixel_ratio == self.pixel_ratio {
            // Some resize events are spurious; it is not necessary to reinitialize the passes if
            // the size hasn't actually changed.
            return;
        }
        self.width = width;
        self.height = height;
        self.pixel_ratio = pixel_ratio;
        let defs = self.pipeline.passes_clone();
        for (pass, def) in self.passes.iter_mut().zip(defs) {
            pass.resize(def, width, height, pixel_ratio);
        }
    }

    /// Initialize all pass definitions from the [`Pipeline`].
    fn init_passes(&mut self) {
        let ctx    = &self.context;
        let vars   = &self.variables;
        let width  = self.width;
        let height = self.height;
        let pixel_ratio = self.pixel_ratio;
        let defs   = self.pipeline.passes_clone();
        let passes = defs
            .into_iter()
            .map(|pass| ComposerPass::new(ctx, vars, pass, width, height, pixel_ratio));
        self.passes = passes.collect_vec();
    }

    /// Run all the registered passes in this composer.
    pub fn run(&mut self, update_status: UpdateStatus) {
        for pass in &mut self.passes {
            pass.run(update_status);
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
        pixel_ratio: f32,
    ) -> Self {
        let instance = pass::Instance::new(context, variables, width, height, pixel_ratio);
        pass.initialize(&instance);
        Self { pass, instance }
    }

    /// Run the pass.
    pub fn run(&mut self, update_status: UpdateStatus) {
        self.pass.run(&self.instance, update_status);
    }

    /// Update the pass for a change in screen size. Depending on the pass, this may require
    /// reinitialization.
    pub fn resize(
        &mut self,
        def: Box<dyn pass::Definition>,
        width: i32,
        height: i32,
        pixel_ratio: f32,
    ) {
        if def.is_screen_size_independent() {
            self.instance.width = width;
            self.instance.height = height;
            self.instance.pixel_ratio = pixel_ratio;
        } else {
            let ctx = self.context.clone();
            let vars = mem::take(&mut self.variables);
            *self = ComposerPass::new(&ctx, &vars, def, width, height, pixel_ratio);
        }
    }
}
