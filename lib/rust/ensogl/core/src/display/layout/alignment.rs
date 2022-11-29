//! This module contains structures describing alignment.

use crate::prelude::*;



// =================
// === Alignment ===
// =================

/// Alignment. In one dimension the alignment is either start, center, or end. In 2 dimensions,
/// [`Start`] means "left" horizontally and "bottom" vertically, while [`End`] means "right"
/// horizontally and "top" vertically.
#[derive(Clone, Copy, Debug, PartialEq)]
#[allow(missing_docs)]
pub enum Alignment {
    Start,
    Center,
    End,
}

impl Alignment {
    pub fn as_number(self) -> f32 {
        match self {
            Alignment::Start => 0.0,
            Alignment::Center => 0.5,
            Alignment::End => 1.0,
        }
    }
}


// === Smart Constructors ===

/// 2-dimensional alignment.
#[allow(missing_docs)]
pub mod dim2 {
    use super::*;

    /// Constructor.
    pub fn new(horizontal: Alignment, vertical: Alignment) -> Vector2<Alignment> {
        Vector2(horizontal, vertical)
    }

    pub fn center() -> Vector2<Alignment> {
        new(Alignment::Center, Alignment::Center)
    }

    pub fn bottom_left() -> Vector2<Alignment> {
        new(Alignment::Start, Alignment::Start)
    }

    pub fn bottom_right() -> Vector2<Alignment> {
        new(Alignment::End, Alignment::Start)
    }

    pub fn bottom_center() -> Vector2<Alignment> {
        new(Alignment::Center, Alignment::Start)
    }

    pub fn top_left() -> Vector2<Alignment> {
        new(Alignment::Start, Alignment::End)
    }

    pub fn top_right() -> Vector2<Alignment> {
        new(Alignment::End, Alignment::End)
    }

    pub fn top_center() -> Vector2<Alignment> {
        new(Alignment::Center, Alignment::End)
    }

    pub fn center_left() -> Vector2<Alignment> {
        new(Alignment::Start, Alignment::Center)
    }

    pub fn center_right() -> Vector2<Alignment> {
        new(Alignment::End, Alignment::Center)
    }
}


// === Defaults ===

impl Default for Alignment {
    fn default() -> Self {
        Self::Start
    }
}

impl From<&Alignment> for Alignment {
    fn from(t: &Alignment) -> Self {
        *t
    }
}
