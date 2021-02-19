//! Visualization Metadata contains information about the runtime state of the visualization.

use crate::prelude::*;

use crate::data::enso;

/// Description of the visualization state, emitted with visualization_enabled event in GraphEditor.
#[derive(Clone,Debug,Default)]
pub struct Metadata {
    /// An Enso lambda, called on the Engine side before sending data to IDE, allowing us to do some
    /// compression or filtering for the best performance. See also _Lazy Visualization_ section
    /// [here](http://dev.enso.org/docs/ide/product/visualizations.html).
    pub preprocessor: Option<enso::Code>,
}
