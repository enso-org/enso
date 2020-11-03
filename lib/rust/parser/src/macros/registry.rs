//! The macro registry that can be queried during the process of macro resolution.

use crate::prelude::*;



// ================
// === Registry ===
// ================

/// The registry is responsible for the registration of macro definitions, and the querying of said
/// definitions.
#[derive(Clone,Debug,Default,Eq,PartialEq)]
pub struct Registry {
    // TODO [AA] Some way of mapping names to a tree section.
    // TODO [AA] The tree.
}
