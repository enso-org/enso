//! Definition of static text label widget.

use crate::prelude::*;

// =============
// === Label ===
// =============

#[derive(Debug, Clone, Copy, PartialEq, Default)]

/// Label widget configuration options.
pub struct Config;

#[derive(Clone, Copy, Debug)]
pub struct Widget;

impl super::SpanWidget for Widget {
    type Config = Config;
    fn new(config: &Config, ctx: super::ConfigContext<'_>) -> Self {
        Self {}
    }

    fn configure(&mut self, config: &Config, ctx: super::ConfigContext<'_>) {}
}
