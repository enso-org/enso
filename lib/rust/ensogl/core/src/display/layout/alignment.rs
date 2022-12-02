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
// === 2-dimensional Alignment ===
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
}

impl Dim2 {
    pub fn normalized(self) -> Vector2<f32> {
        Vector2(self.vector.x.normalized(), self.vector.y.normalized())
    }
}



// ======================================
// === 2-dimensional Alignment Macros ===
// ======================================

/// Runs the provided macro with two alignment anchor arrays, one for horizontal and one for
/// vertical. For example, if run with the arguments `f [args]`, it results in:
///
/// ```text
/// f!{ [args] [left center right] [bottom center top] }
/// ```
///
/// The `[args]` argument is optional.
#[macro_export]
macro_rules! with_alignment_dim2_anchors {
    ($f:path $([$($args:tt)*])?) => {
        $f! { $([$($args)*])? [left center right] [bottom center top] }
    };
}

/// Runs the provided macro with an alignment anchor matrix. For example, if run with the arguments
/// `f [args]`, it results in:
///
/// ```text
/// f!{ [args] [left bottom] [left center] ... [center bottom] ... [bottom top] }
/// ```
///
/// The `[args]` argument is optional.
#[macro_export]
macro_rules! with_alignment_dim2_matrix {
    ($f:path $([$($args:tt)*])?) => {
        $crate::with_alignment_dim2_anchors! {enso_shapely::cartesian [$f $([$($args)*])?] }
    };
}

/// Runs the provided macro with an alignment anchor matrix annotated with a name for the anchor
/// pair. The name is created as `$x_$y` with the exception for both anchors being `center`, then
/// the name is simply `center`. For example, if run with the arguments `f [args]`, it results in:
///
/// ```text
/// f!{ [args]
///     [left_bottom left bottom]
///     [left_center left center]
///     ...
///     [center_bottom center bottom]
///     [center center center]
///     [center_top center top]
///     ...
///     [bottom top]
/// }
/// ```
///
/// The `[args]` argument is optional.
#[macro_export]
macro_rules! with_alignment_dim2_named_matrix {
    ($f:path $([$($args:tt)*])?) => {
        $crate::with_alignment_dim2_matrix! {
            $crate::with_alignment_dim2_named_matrix [$f $([$($args)*])?]
        }
    };
    ([$($fs:tt)*] $($ts:tt)*) => {
        $crate::with_alignment_dim2_named_matrix! {@ [$($fs)*] [] $($ts)*}
    };
    (@ $fs:tt [$($out:tt)*] [[center center] $($ts:tt)*]) => {
        $crate::with_alignment_dim2_named_matrix! {@ $fs [$($out)* [center center center]] [$($ts)*]}
    };
    (@ $fs:tt [$($out:tt)*] [[$x:ident $y:ident] $($ts:tt)*]) => { paste! {
        $crate::with_alignment_dim2_named_matrix! {@ $fs [$($out)* [[<$x _ $y>] $x $y]] [$($ts)*]}
    }};
    (@ [$f:path $([$($args:tt)*])?] $out:tt []) => {
        $f! { $([$($args)*])? $out }
    };
}


// ============================================
// === 2-dimensional Alignment Constructors ===
// ============================================

macro_rules! gen_dim2_cons {
    ([$([$f:ident $x:ident $y:ident])*]) => {
        impl Dim2 {$(
            /// Constructor.
            pub fn $f() -> Self {
                Self::new(Dim1::$x(), Dim1::$y())
            }
        )*}
    }
}

with_alignment_dim2_named_matrix!(gen_dim2_cons);
