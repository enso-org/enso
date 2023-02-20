//! Span tree shape and contained data depends not only on the AST but also some context-dependent
//! information. This module defined trait [`Context`] that provides the information known to
//! Span Tree during its construction.

use crate::prelude::*;

use crate::ArgumentInfo;

use ast::Id;



/// Additional information available on nodes that are an invocation of a known methods.
#[derive(Clone, Debug, Eq, PartialEq, Default)]
pub struct CalledMethodInfo {
    /// Whether or not this call represents a static method.
    pub is_static:      bool,
    /// Whether or not this call represents a constructor.
    pub is_constructor: bool,
    /// Whether or not this method was called as a qualified function (e.g. `Text.trim`).
    /// This also applies to all static methods, as those are always called that way. This
    /// information is only available when it is resolved from computed value registry for specific
    /// call expression. Otherwise it is `None`.
    pub called_on_type: Option<bool>,
    /// Information about arguments taken by a called method.
    pub parameters:     Vec<ArgumentInfo>,
}

impl CalledMethodInfo {
    /// Assign call and target expression IDs to all parameters.
    pub fn with_call_id(mut self, call_id: Option<Id>) -> Self {
        self.parameters.iter_mut().for_each(|arg| {
            arg.call_id = call_id;
        });
        self
    }

    /// Add information whether this method was called on .
    pub fn with_called_on_type(mut self, called_on_type: bool) -> Self {
        self.called_on_type = Some(called_on_type);
        self
    }
}



// ===============
// === Context ===
// ===============

/// Entity that is able to provide information whether a given expression is a known method
/// invocation. If so, additional information is provided.
pub trait Context {
    /// Check if the given expression is known to be a call to a known method. If so, return the
    /// available information.
    ///
    /// The `name` parameter can be used to pass a known target method identifier (if the caller
    /// knows what name is supplied at the invocation site).
    ///
    /// Trait implementors may used it to filter-out results, however they are not required to do
    /// so. Caller should not assume that the called method has the same name as given identifier.
    fn call_info(&self, id: Id, name: Option<&str>) -> Option<CalledMethodInfo>;
}



// =============
// === Empty ===
// =============

/// An empty context that provides no information whatsoever.
#[derive(Copy, Clone, Debug)]
pub struct Empty;

impl Context for Empty {
    fn call_info(&self, _id: Id, _name: Option<&str>) -> Option<CalledMethodInfo> {
        None
    }
}
