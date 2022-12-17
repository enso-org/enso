//! This module contains structures describing alignment.

use crate::prelude::*;



// ===============================
// === 1-dimensional Alignment ===
// ===============================

/// One-dimensional alignment.
///
/// In one dimension the alignment is either start, center, or end. In 2 dimensions, [`Start`] means
/// "left" horizontally and "bottom" vertically, while [`End`] means "right" horizontally and "top"
/// vertically.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
    /// A normalized value of the alignment, in the range [0.0, 1.0].
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



// ========================================
// === 1-dimensional Optional Alignment ===
// ========================================

/// Optional version of `Dim1`. It expresses an alignment that could not be set. It can be used for
/// example as an alignment override.
#[derive(Clone, Copy, Debug, Deref, DerefMut, Default, PartialEq, Eq, From)]
#[allow(missing_docs)]
pub struct OptDim1 {
    pub value: Option<Dim1>,
}

/// Constructors
#[allow(missing_docs)]
impl OptDim1 {
    pub fn left() -> Self {
        Self::from(Some(Dim1::left()))
    }
    pub fn center() -> Self {
        Self::from(Some(Dim1::center()))
    }
    pub fn right() -> Self {
        Self::from(Some(Dim1::right()))
    }
    pub fn bottom() -> Self {
        Self::from(Some(Dim1::bottom()))
    }
    pub fn top() -> Self {
        Self::from(Some(Dim1::top()))
    }
}



// ===============================
// === 2-dimensional Alignment ===
// ===============================

/// Two-dimensional alignment. It is equivalent to [`Vector2<Dim1>`], but also has many handy
/// associated functions.
#[derive(Clone, Copy, Debug, Deref, Default, PartialEq, Eq)]
#[allow(missing_docs)]
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
    /// A normalized value of the alignment, in the range [0.0, 1.0].
    pub fn normalized(self) -> Vector2<f32> {
        Vector2(self.vector.x.normalized(), self.vector.y.normalized())
    }
}



// ========================================
// === 2-dimensional Optional Alignment ===
// ========================================

/// Optional version of `Dim2`. It expresses an alignment that could not be set. It can be used for
/// example as an alignment override.
#[derive(Clone, Copy, Debug, Deref, DerefMut, Default, PartialEq, Eq)]
#[allow(missing_docs)]
pub struct OptDim2 {
    pub vector: Vector2<OptDim1>,
}

#[allow(missing_docs)]
impl OptDim2 {
    /// Constructor.
    pub fn new(horizontal: OptDim1, vertical: OptDim1) -> Self {
        Self { vector: Vector2(horizontal, vertical) }
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
macro_rules! with_alignment_dim2_anchors_cartesian {
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
macro_rules! with_alignment_dim2_named_anchors_cartesian {
    ($f:path $([$($args:tt)*])?) => {
        $crate::with_alignment_dim2_anchors_cartesian! {
            $crate::with_alignment_dim2_named_anchors_cartesian [$f $([$($args)*])?]
        }
    };
    ([$($fs:tt)*] $($ts:tt)*) => {
        $crate::with_alignment_dim2_named_anchors_cartesian! {@ [$($fs)*] [] $($ts)*}
    };
    (@ $fs:tt [$($out:tt)*] [[center center] $($ts:tt)*]) => {
        $crate::with_alignment_dim2_named_anchors_cartesian! {@ $fs [$($out)* [center center center]] [$($ts)*]}
    };
    (@ $fs:tt [$($out:tt)*] [[$x:ident $y:ident] $($ts:tt)*]) => { paste! {
        $crate::with_alignment_dim2_named_anchors_cartesian! {@ $fs [$($out)* [[<$x _ $y>] $x $y]] [$($ts)*]}
    }};
    (@ [$f:path $([$($args:tt)*])?] $out:tt []) => {
        $f! { $([$($args)*])? $out }
    };
}



// ===============================================
// === 2-dimensional Optional Alignment Macros ===
// ===============================================

/// Runs the provided macro with two arrays of anchors for horizontal and vertical alignment.
#[macro_export]
macro_rules! with_alignment_opt_dim2_anchors {
    ($f:path $([$($args:tt)*])?) => {
        $f! { $([$($args)*])?
            [[left] [center] [right] [default] []]
            [[bottom] [center] [top] [default] []]
        }
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
macro_rules! with_alignment_opt_dim2_matrix {
    ($f:path $([$($args:tt)*])?) => {
        $crate::with_alignment_opt_dim2_anchors! {enso_shapely::cartesian [$f $([$($args)*])?] }
    };
}

/// Runs the provided macro with an optional alignment anchor matrix. The passed values are of form
/// `[name [x_anchor] [y_anchor]]`. The values [`x_anchor`] and [`y_anchor`] are optional and can be
/// missing. The name is created as `$x_$y` with the exception for both anchors being 'center' or
/// 'default', when the name is simply 'center' or 'default', respectively. For example, if run with
/// the arguments `f [args]`, it results in:
///
/// ```text
/// f!{
///     [left_bottom [left][bottom]]
///     [left_center [left][center]]
///     [left_top [left][top]]
///     [left_default [left][default]]
///     [left [left][]]
///     ...
/// ```
/// The 'default' value means that the alignment should be set to its default value. The `[args]`
/// argument is optional.
#[macro_export]
macro_rules! with_alignment_opt_dim2_named_matrix_sparse {
    ($(#$meta:tt)* $f:path $([$($args:tt)*])?) => {
        $crate::with_alignment_opt_dim2_matrix! {
            $crate::with_alignment_opt_dim2_named_matrix_sparse [$(#$meta)* $f $([$($args)*])?]
        }
    };
    ([$($fs:tt)*] $($ts:tt)*) => {
        $crate::with_alignment_opt_dim2_named_matrix_sparse! {@ [$($fs)*] [] $($ts)*}
    };
    (@ $fs:tt [$($out:tt)*] [[[center] [center]] $($ts:tt)*]) => {
        $crate::with_alignment_opt_dim2_named_matrix_sparse! {
            @ $fs [$($out)* [center [center] [center]]] [$($ts)*]
        }
    };
    (@ $fs:tt [$($out:tt)*] [[[center] []] $($ts:tt)*]) => {
        $crate::with_alignment_opt_dim2_named_matrix_sparse! {
            @ $fs [$($out)* [center_x [center] []]] [$($ts)*]
        }
    };
    (@ $fs:tt [$($out:tt)*] [[[] [center]] $($ts:tt)*]) => {
        $crate::with_alignment_opt_dim2_named_matrix_sparse! {
            @ $fs [$($out)* [center_y [] [center]]] [$($ts)*]
        }
    };
    (@ $fs:tt [$($out:tt)*] [[[default] [default]] $($ts:tt)*]) => {
        $crate::with_alignment_opt_dim2_named_matrix_sparse! {
            @ $fs [$($out)* [default [default] [default]]] [$($ts)*]
        }
    };
    (@ $fs:tt [$($out:tt)*] [[[default] []] $($ts:tt)*]) => {
        $crate::with_alignment_opt_dim2_named_matrix_sparse! {
            @ $fs [$($out)* [default_x [default] []]] [$($ts)*]
        }
    };
    (@ $fs:tt [$($out:tt)*] [[[] [default]] $($ts:tt)*]) => {
        $crate::with_alignment_opt_dim2_named_matrix_sparse! {
            @ $fs [$($out)* [default_y [] [default]]] [$($ts)*]
        }
    };
    (@ $fs:tt [$($out:tt)*] [[[$x:ident] [$y:ident]] $($ts:tt)*]) => { paste! {
        $crate::with_alignment_opt_dim2_named_matrix_sparse! {
            @ $fs [$($out)* [[<$x _ $y>] [$x] [$y]]] [$($ts)*]
        }
    }};
    (@ $fs:tt [$($out:tt)*] [[[$x:ident] []] $($ts:tt)*]) => { paste! {
        $crate::with_alignment_opt_dim2_named_matrix_sparse! {
            @ $fs [$($out)* [[<$x>] [$x] []]] [$($ts)*]
        }
    }};
    (@ $fs:tt [$($out:tt)*] [[[] [$y:ident]] $($ts:tt)*]) => { paste! {
        $crate::with_alignment_opt_dim2_named_matrix_sparse! {
            @ $fs [$($out)* [[<$y>] [] [$y]]] [$($ts)*]
        }
    }};
    (@ $fs:tt [$($out:tt)*] [[[] []] $($ts:tt)*]) => { paste! {
        $crate::with_alignment_opt_dim2_named_matrix_sparse! {@ $fs [$($out)*] [$($ts)*]}
    }};
    (@ [$(#$meta:tt)* $f:path $([$($args:tt)*])?] $out:tt []) => {
        $f! { $(#$meta)* $([$($args)*])? $out }
    };
}

/// Runs the provided macro with an optional alignment anchor matrix. For example, if run with the
/// arguments `f [args]`, it results in:
///
/// ```text
/// f!{
///     [left_bottom [left][bottom]]
///     [left_center [left][center]]
///     [left_top [left][top]]
///     [left_default [left][default]]
///     [center_bottom [center][bottom]]
///     ...
/// ```
/// The 'default' value means that the alignment should be set to its default value. The `[args]`
/// argument is optional.
#[macro_export]
macro_rules! with_alignment_opt_dim2_named_matrix {
    ($f:path $([$($args:tt)*])?) => {
        $crate::with_alignment_opt_dim2_named_matrix_sparse! {
            $crate::with_alignment_opt_dim2_named_matrix [$f $([$($args)*])?]
        }
    };
    ([$($fs:tt)*] $($ts:tt)*) => {
        $crate::with_alignment_opt_dim2_named_matrix! {@ [$($fs)*] [] $($ts)*}
    };
    (@ $fs:tt [$($out:tt)*] [[$n:ident [$x:ident] [$y:ident]] $($ts:tt)*]) => {
        $crate::with_alignment_opt_dim2_named_matrix! {@ $fs [$($out)* [$n [$x] [$y]]] [$($ts)*]}
    };
    (@ $fs:tt $out:tt [[$n:ident [$x:ident] []] $($ts:tt)*]) => {
        $crate::with_alignment_opt_dim2_named_matrix! {@ $fs $out [$($ts)*]}
    };
    (@ $fs:tt $out:tt [[$n:ident [] [$y:ident]] $($ts:tt)*]) => {
        $crate::with_alignment_opt_dim2_named_matrix! {@ $fs $out [$($ts)*]}
    };
    (@ [$f:path $([$($args:tt)*])?] $out:tt []) => {
        $f! { $([$($args)*])? $out }
    };
}



// ============================================
// === 2-dimensional Alignment Constructors ===
// ============================================

macro_rules! gen_dim2_cons {
    ([$([$f:ident $x:ident $y:ident])*]) => { paste! {
        impl Dim2 {$(
            /// Constructor.
            pub fn $f() -> Self {
                Self::new(Dim1::$x(), Dim1::$y())
            }

            /// Change the alignment.
            pub fn [<align_ $f>](&mut self) {
                *self = Self::$f();
            }
        )*}
    }}
}

with_alignment_dim2_named_anchors_cartesian!(gen_dim2_cons);



// =====================================================
// === 2-dimensional Optional Alignment Constructors ===
// =====================================================

macro_rules! gen_opt_dim2_cons {
    ([$([$f:ident [$x:ident] [$y:ident]])*]) => {
        // We disable the clippy check because we are generating a `default` method here.
        #[allow(clippy::should_implement_trait)]
        impl OptDim2 {$(
            /// Constructor.
            pub fn $f() -> Self {
                Self::new(OptDim1::$x(), OptDim1::$y())
            }
        )*}
    }
}

with_alignment_opt_dim2_named_matrix!(gen_opt_dim2_cons);



// ================================================
// === 2-dimensional Optional Alignment Setters ===
// ================================================

macro_rules! gen_opt_dim2_setters {
    ([$([$f:ident [$($x:ident)?] [$($y:ident)?]])*]) => { paste! {
        impl OptDim2 {$(
            /// Constructor.
            pub fn [<align_ $f>](&mut self) {
                $(self.x = OptDim1::$x();)?
                $(self.y = OptDim1::$y();)?
            }
        )*}
    }}
}

with_alignment_opt_dim2_named_matrix_sparse!(gen_opt_dim2_setters);
