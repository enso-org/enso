//! Definition of empty widget that represents insertion point, which is a span node representing
//! a position where a new expression can be inserted. Does not correspond to any AST, but instead
//! is placed between spans for AST nodes. It is often used as an temporary edge endpoint when
//! dragging an edge.
//!
//! See also [`span_tree::node::InsertionPoint`].

use crate::prelude::*;

use ensogl::display::object;



// ======================
// === InsertionPoint ===
// ======================

/// Insertion point widget configuration options.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Config;


/// Insertion point widget. Displays nothing when not connected.
#[derive(Clone, Debug)]
pub struct Widget {
    root: object::Instance,
}

impl super::SpanWidget for Widget {
    type Config = Config;

    fn root_object(&self) -> &object::Instance {
        &self.root
    }

    fn new(_: &Config, _: &super::ConfigContext) -> Self {
        let root = object::Instance::new_named("widget::InsertionPoint");
        root.set_size(Vector2::<f32>::zero());
        Self { root }
    }

    fn configure(&mut self, _: &Config, _: super::ConfigContext) {}
}
