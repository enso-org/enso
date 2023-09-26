//! Render composer definition, which is a render pipeline bound to a specific context.

use crate::display::render::pipeline::*;
use crate::prelude::*;
use crate::system::gpu::*;

use crate::display::render::pass;
use crate::display::scene::UpdateStatus;
use crate::system::gpu::context::ContextLost;



// ================
// === Composer ===
// ================

/// Render composer is a render pipeline bound to a specific context.
#[derive(Debug)]
pub struct Composer {
    passes:      Vec<Box<dyn pass::Instance>>,
    variables:   Rc<RefCell<UniformScope>>,
    context:     Context,
    width:       i32,
    height:      i32,
    pixel_ratio: f32,
}

impl Composer {
    /// Constructor
    pub fn new(
        pipeline: Pipeline,
        context: &Context,
        variables: &Rc<RefCell<UniformScope>>,
        width: i32,
        height: i32,
        pixel_ratio: f32,
    ) -> Self {
        let passes = default();
        let context = context.clone();
        let variables = variables.clone_ref();
        let mut this = Self { passes, variables, context, width, height, pixel_ratio };
        this.set_pipeline(pipeline);
        this
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
        for pass in &mut self.passes {
            pass.resize(width, height, pixel_ratio);
        }
    }

    /// Set a new pipeline for this composer.
    pub fn set_pipeline(&mut self, pipeline: Pipeline) {
        let width = self.width;
        let height = self.height;
        let pixel_ratio = self.pixel_ratio;
        let defs = pipeline.passes();
        let instance =
            pass::InstanceInfo::new(&self.context, &self.variables, width, height, pixel_ratio);
        let passes: Result<Vec<_>, ContextLost> =
            defs.iter().map(|def| def.instantiate(instance.clone())).collect();
        self.passes = passes.unwrap_or_default();
    }

    /// Run all the registered passes in this composer.
    pub fn run(&mut self, update_status: UpdateStatus) {
        for pass in &mut self.passes {
            pass.run(update_status);
        }
    }
}
