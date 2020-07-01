//! Contains definition of trivial logger that discards all messages except warnings and errors.

use enso_prelude::*;

use crate::Message;
use crate::AnyLogger;
use crate::enabled;

use enso_shapely::CloneRef;
use std::fmt::Debug;



// ==============
// === Logger ===
// ==============

/// Trivial logger that discards all messages except warnings and errors.
#[derive(Clone,CloneRef,Debug,Default)]
pub struct Logger {
    enabled : enabled::Logger,
}


// === Impls ===

impls!{ From + &From <enabled::Logger> for Logger { |logger| Self::new(logger.path()) }}

impl AnyLogger for Logger {
    type Owned = Self;
    fn new     (path:impl Into<ImString>) -> Self { Self {enabled : enabled::Logger::new(path) } }
    fn path    (&self) -> &str { self.enabled.path() }
    fn warning (&self, msg:impl Message) { self.enabled.warning (msg) }
    fn error   (&self, msg:impl Message) { self.enabled.error   (msg) }
}
