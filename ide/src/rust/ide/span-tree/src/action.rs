//! A module containing all actions provided by SpanTree.
//!
//! The actions are in WIP state - they will be implemented along connection operations.

use crate::node;

/// An API for SpanTree nodes for doing actions.
#[allow(missing_docs)]
pub trait SpanTreeActions {
    fn can_set          (&self) -> bool;
    fn can_insert_before(&self) -> bool;
    fn can_erase        (&self) -> bool;

    //TODO[ao] Add functions for actually do the action.
}

impl<'a> SpanTreeActions for node::Ref<'a> {
    fn can_set          (&self) -> bool { false }
    fn can_insert_before(&self) -> bool { false }
    fn can_erase        (&self) -> bool { false }
}
