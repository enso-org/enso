//! This module contains structures describing alignment.

use crate::prelude::*;

// =================
// === Alignment ===
// =================

/// Camera alignment. It describes where the origin of the camera should be aligned to.
#[derive(Clone,Copy,Debug)]
pub struct Alignment {
    /// Horizontal alignment.
    pub horizontal : HorizontalAlignment,

    /// Vertical alignment.
    pub vertical   : VerticalAlignment,
}

/// Horizontal alignments.
#[derive(Clone,Copy,Debug)]
#[allow(missing_docs)]
pub enum HorizontalAlignment {Left,Center,Right}

/// Vertical alignments.
#[derive(Clone,Copy,Debug)]
#[allow(missing_docs)]
pub enum VerticalAlignment {Top,Center,Bottom}

impl Default for HorizontalAlignment { fn default() -> Self { Self::Left } }
impl Default for VerticalAlignment   { fn default() -> Self { Self::Bottom } }
impl Default for Alignment {
    fn default() -> Self {
        let horizontal = default();
        let vertical   = default();
        Self {horizontal,vertical}
    }
}
