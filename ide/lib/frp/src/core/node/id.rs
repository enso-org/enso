//! This module defines FRP node identifiers. They are mainly used for debugging purposes.

use crate::prelude::*;



// =============
// === HasId ===
// =============

/// Each FRP node is assigned with an unique ID. This is currently used mainly for debugging
/// purposes.
pub trait HasId {
    /// Id of the entity.
    fn id(&self) -> usize;
}

impl<T:Unwrap> HasId for T
where Content<T> : HasId {
    default fn id(&self) -> usize {
        self.unwrap().id()
    }
}


// ====================
// === HasDisplayId ===
// ====================

/// Each FRP node can also be assigned with a `display_id`. Unlike `id`, the `display_id` does not
/// have to be unique. Nodes with the same `display_id` are displayed as a single node in the graph
/// view. Note that `display_id` defaults to `id` if not set explicitly to other value.
pub trait HasDisplayId {
    /// Getter.
    fn display_id(&self) -> usize;
    /// Setter.
    fn set_display_id(&self, id:usize);
}

impl<T> HasDisplayId for T
where T:Unwrap, Content<T> : HasDisplayId {
    default fn display_id(&self) -> usize {
        self.unwrap().display_id()
    }

    default fn set_display_id(&self, id:usize) {
        self.unwrap().set_display_id(id)
    }
}
