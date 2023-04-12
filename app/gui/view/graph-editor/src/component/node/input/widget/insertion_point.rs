//! Definition of empty widget that represents insertion point.

use crate::prelude::*;

use ensogl::application::Application;
use ensogl::display::object;



// ======================
// === InsertionPoint ===
// ======================

/// Insertion point widget configuration options.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Config;


/// Insertion point widget. Displays nothing.
#[derive(Clone, Debug)]
pub struct Widget {
    root: object::Instance,
}

impl super::SpanWidget for Widget {
    type Config = Config;

    fn root_object(&self) -> &object::Instance {
        &self.root
    }

    fn new(_: &Config, _: &Application, _: &super::WidgetsFrp) -> Self {
        let root = object::Instance::new();
        Self { root }
    }

    fn configure(&mut self, _: &Config, _: super::ConfigContext) {}
}
