//! This module contains structures describing alignment.

use crate::prelude::*;



// ===============================
// === 1-dimensional Alignment ===
// ===============================

/// Alignment. In one dimension the alignment is either start, center, or end. In 2 dimensions,
/// [`Start`] means "left" horizontally and "bottom" vertically, while [`End`] means "right"
/// horizontally and "top" vertically.
#[derive(Clone, Copy, Debug, PartialEq)]
#[allow(missing_docs)]
pub enum Dim1 {
    Start,
    Center,
    End,
}

/// Constructors
#[allow(missing_docs)]
impl Dim1 {
    pub fn left() -> Self {
        Self::Start
    }
    pub fn center() -> Self {
        Self::Center
    }
    pub fn right() -> Self {
        Self::End
    }
    pub fn bottom() -> Self {
        Self::Start
    }
    pub fn top() -> Self {
        Self::End
    }
}

impl Dim1 {
    pub fn as_number(self) -> f32 {
        match self {
            Dim1::Start => 0.0,
            Dim1::Center => 0.5,
            Dim1::End => 1.0,
        }
    }
}

impl Default for Dim1 {
    fn default() -> Self {
        Self::Start
    }
}

impl From<&Dim1> for Dim1 {
    fn from(t: &Dim1) -> Self {
        *t
    }
}



// ===============================
// === 1-dimensional Alignment ===
// ===============================

#[derive(Clone, Copy, Debug, Deref, Default, PartialEq)]
pub struct Dim2 {
    pub vector: Vector2<Dim1>,
}

/// Constructors.
#[allow(missing_docs)]
impl Dim2 {
    pub fn new(horizontal: Dim1, vertical: Dim1) -> Self {
        Self { vector: Vector2(horizontal, vertical) }
    }

    pub fn left_bottom() -> Self {
        Self::new(Dim1::left(), Dim1::bottom())
    }

    pub fn left_center() -> Self {
        Self::new(Dim1::left(), Dim1::center())
    }

    pub fn left_top() -> Self {
        Self::new(Dim1::left(), Dim1::top())
    }

    pub fn center_bottom() -> Self {
        Self::new(Dim1::center(), Dim1::bottom())
    }

    pub fn center() -> Self {
        Self::new(Dim1::center(), Dim1::center())
    }

    pub fn center_top() -> Self {
        Self::new(Dim1::center(), Dim1::top())
    }

    pub fn right_bottom() -> Self {
        Self::new(Dim1::right(), Dim1::bottom())
    }

    pub fn right_center() -> Self {
        Self::new(Dim1::right(), Dim1::center())
    }

    pub fn right_top() -> Self {
        Self::new(Dim1::right(), Dim1::top())
    }
}

impl Dim2 {
    pub fn as_number(self) -> Vector2<f32> {
        Vector2(self.vector.x.as_number(), self.vector.y.as_number())
    }
}
