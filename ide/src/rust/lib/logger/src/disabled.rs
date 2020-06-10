//! Contains definition of trivial logger that discards all messages except warnings and errors.

use enso_prelude::*;

use crate::LogMsg;
use crate::AnyLogger;

use shapely::CloneRef;
use std::fmt::Debug;



// ==============
// === Logger ===
// ==============

/// Trivial logger that discards all messages except warnings and errors.
#[derive(Clone,CloneRef,Debug,Default)]
pub struct Logger{
    /// Path that is used as an unique identifier of this logger.
    pub path:Rc<String>,
}



// ===================
// === Conversions ===
// ===================

impls!{ From + &From <crate::enabled::Logger> for Logger { |logger| Self::new(logger.path()) }}



// ======================
// === AnyLogger Impl ===
// ======================

impl AnyLogger for Logger {
    type This = Self;

    fn path(&self) -> &str {
        self.path.as_str()
    }

    fn new(path:impl Str) -> Self {
        Self {path:Rc::new(path.into())}
    }

    fn trace      <M: LogMsg>(&self,   _:M) {                                                 }
    fn debug      <M: LogMsg>(&self,   _:M) {                                                 }
    fn info       <M: LogMsg>(&self,   _:M) {                                                 }
    fn warning    <M: LogMsg>(&self, msg:M) { crate::enabled::Logger::warning(&self.path,msg) }
    fn error      <M: LogMsg>(&self, msg:M) { crate::enabled::Logger::error  (&self.path,msg) }
    fn group_begin<M: LogMsg>(&self,   _:M) {                                                 }
    fn group_end             (&self       ) {                                                 }
}
