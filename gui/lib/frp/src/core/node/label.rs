//! This module defines FRP node labels. They are mainly used for debugging purposes.

use crate::prelude::*;



// =============
// === Label ===
// =============

/// Abstraction for labeled entities. Used mainly for debugging purposes.
pub trait HasLabel {
    /// Label of the entity.
    fn label(&self) -> &CowString;
}

impl<T:Unwrap> HasLabel for T
    where Content<T> : HasLabel {
    default fn label(&self) -> &CowString {
        self.unwrap().label()
    }
}
