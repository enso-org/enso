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
    pub fn normalized(self) -> f32 {
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

#[allow(missing_docs)]
impl Dim2 {
    /// Constructor.
    pub fn new(horizontal: Dim1, vertical: Dim1) -> Self {
        Self { vector: Vector2(horizontal, vertical) }
    }

    /// Constructor.
    pub fn center() -> Self {
        Self::center_center()
    }
}

impl Dim2 {
    pub fn normalized(self) -> Vector2<f32> {
        Vector2(self.vector.x.normalized(), self.vector.y.normalized())
    }
}

#[macro_export]
macro_rules! with_alignemnt_dim2_anchors {
    ($f:path [$($args:tt)*]) => {
        $f! { [$($args)*] [left center right] [bottom center top] }
    };
}

macro_rules! gen_dim2_cons {
    ([$([$x:ident $y:ident])*]) => { paste! {
        impl Dim2 {$(
            /// Constructor.
            pub fn [<$x _ $y>]() -> Self {
                Self::new(Dim1::$x(), Dim1::$y())
            }
        )*}
    }}
}

with_alignemnt_dim2_anchors!(enso_shapely::cartesian[gen_dim2_cons]);
