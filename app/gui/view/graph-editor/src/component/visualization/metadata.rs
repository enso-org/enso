//! Visualization Metadata contains information about the runtime state of the visualization.

use crate::prelude::*;

use crate::component::visualization;



/// Description of the visualization state, emitted with visualization_shown event in GraphEditor.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Metadata {
    /// An Enso lambda, called on the Engine side before sending data to IDE, allowing us to do
    /// some compression or filtering for the best performance. See also _Lazy Visualization_
    /// section [here](http://dev.enso.org/docs/ide/product/visualizations.html).
    pub preprocessor: visualization::instance::PreprocessorConfiguration,
}

impl Metadata {
    /// Convenience helper for wrapping preprocessor into visualization's metadata.
    pub fn new(preprocessor: &visualization::instance::PreprocessorConfiguration) -> Self {
        Self { preprocessor: preprocessor.clone_ref() }
    }
}
