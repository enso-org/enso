// FIXME: Animators structs should get EventLoop as parameter. The whole application should have
// only one RequestAnimationFrame loop going on to avoid its overhead.

pub mod physics;
mod continuous_time_animator;
mod animator;
mod fixed_step_animator;

pub use continuous_time_animator::ContinuousTimeAnimator;
pub use animator::Animator;
pub use fixed_step_animator::FixedStepAnimator;
pub use fixed_step_animator::IntervalCounter;

// ===================
// === FnAnimation ===
// ===================

pub trait FnAnimation = FnMut(f32) + 'static;



// FIXME: The objects in this section needs a better place.
// =============
// === Utils ===
// =============

use nalgebra::clamp;
use std::ops::Mul;
use std::ops::Add;

pub fn linear_interpolation<T>(a:T, b:T, t:f32) -> T
where T : Mul<f32, Output = T> + Add<T, Output = T> {
    let t = clamp(t, 0.0, 1.0);
    a * (1.0 - t) + b * t
}
