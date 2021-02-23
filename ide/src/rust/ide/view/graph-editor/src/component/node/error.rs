//! Contains a struct definition for error information on nodes.
use crate::prelude::*;

use crate::builtin::visualization::native::error as error_visualization;

use serde::Deserialize;
use serde::Serialize;



// =============
// === Error ===
// =============

/// An error kind.
#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum Kind {Panic,Dataflow}

/// Additional error information (beside the error value itself) for some erroneous node.
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct Error {
    pub kind : Immutable<Kind>,
    /// An error message overriding the error visualization data. Should be set in cases when the
    /// visualization won't work (e.g. in case of panics).
    pub message : Rc<Option<String>>,
    /// Flag indicating that the error is propagated from another node visible on the scene.
    pub propagated : Immutable<bool>,
}

impl Error {
    /// Return data which should be sent to the Error Visualization to display this error.
    /// Returns [`None`] if the data should arrive from the Engine.
    pub fn visualization_data(&self) -> Option<error_visualization::Input> {
        Some(error_visualization::Input {
            kind    : Some(*self.kind),
            message : self.message.as_ref().as_ref()?.clone(),
        })
    }
}
