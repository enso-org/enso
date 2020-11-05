//! The macro builder.

use crate::prelude::*;

use crate::macros::literal::Literal;



// ===============
// === Builder ===
// ===============

/// A builder for macro results.
#[derive(Clone,Debug,Eq,PartialEq)]
pub struct Builder {
    segment_head : Literal
}

impl Builder {
    /// Constructor.
    pub fn new(segment_head:Literal) -> Self {
        Self{segment_head}
    }
}
