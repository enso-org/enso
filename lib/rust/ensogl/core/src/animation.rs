//! This module provides different kind of animation utilities, such as physics based animation and
//! animation with easing functions.


// ==============
// === Export ===
// ==============

pub mod easing;
pub mod physics;



mod frp;
mod loops;

pub use frp::*;
pub use loops::*;



// =============
// === Utils ===
// =============

use std::ops::Add;
use std::ops::Mul;

/// A generic trait constraint for interpolable types.
pub trait Interpolable<T: Copy> = Mul<f32, Output = T> + Add<T, Output = T> + Copy;

/// Linear interpolation function for any type implementing T * f32 and T + T.
pub fn linear_interpolation<T: Interpolable<T>>(a: T, b: T, t: f32) -> T {
    a * (1.0 - t) + b * t
}
