//! This module defines a render pipeline, a set of interconnected render passes.

use crate::prelude::*;

use crate::display::render::pass;



// =======================
// === Render Pipeline ===
// =======================

shared! { Pipeline
/// The pipeline is a set of subsequent passes which can consume and produce data. Please note that
/// although passes are run sequentially, their dependency graph (data passing graph) can be DAG.
#[derive(Debug,Default)]
pub struct PipelineModel {
    passes: Vec<Box<dyn pass::Definition>>
}

impl {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Getter.
    pub fn passes_clone(&self) -> Vec<Box<dyn pass::Definition>> {
        self.passes.clone()
    }
}}

impl<Pass: pass::Definition> Add<Pass> for Pipeline {
    type Output = Self;
    fn add(self, pass: Pass) -> Self::Output {
        let pass = Box::new(pass);
        self.rc.borrow_mut().passes.push(pass);
        self
    }
}
