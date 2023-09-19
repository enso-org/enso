//! This module defines a render pipeline, a set of interconnected render passes.

use crate::prelude::*;

use crate::display::render::pass;



// =======================
// === Render Pipeline ===
// =======================

/// The pipeline is a set of subsequent passes which can consume and produce data. Please note that
/// although passes are run sequentially, their dependency graph (data passing graph) can be DAG.
#[derive(Debug, Clone)]
pub struct Pipeline {
    passes: Rc<[Box<dyn pass::Definition>]>,
}

impl Pipeline {
    /// Constructor.
    pub fn new(passes: Rc<[Box<dyn pass::Definition>]>) -> Self {
        Self { passes }
    }

    /// Getter.
    pub fn passes(&self) -> &[Box<dyn pass::Definition>] {
        &self.passes
    }
}

impl Default for Pipeline {
    fn default() -> Self {
        Self { passes: Rc::new([]) }
    }
}
