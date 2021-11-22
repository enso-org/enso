//! This module contains structures describing alignment.

use crate::prelude::*;



// =================
// === Alignment ===
// =================

/// Alignment abstraction. Example usage is camera origin placement allowing it to behave correctly
/// when scaling the scene.
#[derive(Clone, Copy, Debug)]
#[allow(missing_docs)]
pub struct Alignment {
    pub horizontal: Horizontal,
    pub vertical:   Vertical,
}

/// Horizontal alignments.
#[derive(Clone, Copy, Debug)]
#[allow(missing_docs)]
pub enum Horizontal {
    Left,
    Center,
    Right,
}

/// Vertical alignments.
#[derive(Clone, Copy, Debug)]
#[allow(missing_docs)]
pub enum Vertical {
    Top,
    Center,
    Bottom,
}


// === Smart Constructors ===

#[allow(missing_docs)]
#[allow(missing_docs)]
impl Alignment {
    /// Constructor.
    pub fn new(horizontal: Horizontal, vertical: Vertical) -> Self {
        Self { horizontal, vertical }
    }

    pub fn center() -> Self {
        Self::new(Horizontal::Center, Vertical::Center)
    }
    pub fn bottom_left() -> Self {
        Self::new(Horizontal::Left, Vertical::Bottom)
    }
    pub fn bottom_right() -> Self {
        Self::new(Horizontal::Right, Vertical::Bottom)
    }
    pub fn bottom_center() -> Self {
        Self::new(Horizontal::Center, Vertical::Bottom)
    }
    pub fn top_left() -> Self {
        Self::new(Horizontal::Left, Vertical::Top)
    }
    pub fn top_right() -> Self {
        Self::new(Horizontal::Right, Vertical::Top)
    }
    pub fn top_center() -> Self {
        Self::new(Horizontal::Center, Vertical::Top)
    }
    pub fn center_left() -> Self {
        Self::new(Horizontal::Left, Vertical::Center)
    }
    pub fn center_right() -> Self {
        Self::new(Horizontal::Right, Vertical::Center)
    }
}


// === Defaults ===

impl Default for Horizontal {
    fn default() -> Self {
        Self::Left
    }
}
impl Default for Vertical {
    fn default() -> Self {
        Self::Bottom
    }
}
impl Default for Alignment {
    fn default() -> Self {
        let horizontal = default();
        let vertical = default();
        Self { horizontal, vertical }
    }
}

impl From<&Horizontal> for Horizontal {
    fn from(t: &Horizontal) -> Self {
        *t
    }
}
impl From<&Vertical> for Vertical {
    fn from(t: &Vertical) -> Self {
        *t
    }
}
impl From<&Alignment> for Alignment {
    fn from(t: &Alignment) -> Self {
        *t
    }
}
