//! This module provides a view for breadcrumbs, enabling us to know which node the graph being
//! edited belongs to and navigating through them.

use ensogl::prelude::*;

use engine_protocol::language_server::MethodPointer;



// ==============
// === Export ===
// ==============

pub use ensogl_component::breadcrumbs::*;


// ===========================
// === SharedMethodPointer ===
// ===========================

/// Information about target definition for node entering.
#[derive(Clone, Debug, Deref, PartialEq, Eq)]
pub struct SharedMethodPointer(pub Rc<MethodPointer>);

impl From<MethodPointer> for SharedMethodPointer {
    fn from(method_pointer: MethodPointer) -> Self {
        Self(Rc::new(method_pointer))
    }
}



// =================
// === LocalCall ===
// =================

/// A specific function call occurring within another function's definition body.
/// It's closely related to the `LocalCall` type defined in `Language Server` types, but uses the
/// new type `SharedMethodPointer`.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LocalCall {
    /// An expression being a call to a method.
    pub call:       engine_protocol::language_server::ExpressionId,
    /// A pointer to the called method.
    pub definition: SharedMethodPointer,
}

impl From<LocalCall> for Breadcrumb {
    fn from(val: LocalCall) -> Self {
        Breadcrumb::new_without_icon(&val.definition.name)
    }
}
