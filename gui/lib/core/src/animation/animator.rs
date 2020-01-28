//! This module implements the Animation struct which runs a callback once per frame with a time
//! difference from the last frame as its input.

pub mod continuous;
pub mod fixed_step;
pub mod easing;

use continuous::ContinuousAnimator;



// =========================
// === AnimationCallback ===
// =========================

/// AnimationCallback used by `Animator`, `ContinuousAnimator` and `FixedStepAnimator`.
pub trait AnimationCallback = FnMut(f64) + 'static;



// ====================
// === AnimatorData ===
// ====================

struct AnimatorData {
    callback    : Box<dyn AnimationCallback>,
    previous_ms : Option<f64>
}

impl AnimatorData {
    pub fn new<F:AnimationCallback>(f:F) -> Self {
        let callback    = Box::new(f);
        let previous_ms = None;
        Self {callback,previous_ms}
    }
}



// ================
// === Animator ===
// ================

/// This struct which runs a callback once per frame with a time difference from the last frame
/// as its input.
pub struct Animator {
    _continuous_animator: ContinuousAnimator
}

impl Animator {
    /// Creates `Animator` with an `AnimationCallback`.
    pub fn new<F:AnimationCallback>(f:F) -> Self {
        let mut data             = AnimatorData::new(f);
        let _continuous_animator = ContinuousAnimator::new(move |current_ms| {
            if let Some(previous_ms) = data.previous_ms {
                let delta_ms = current_ms - previous_ms;
                (data.callback)(delta_ms);
            }
            data.previous_ms = Some(current_ms);
        });
        Self { _continuous_animator }
    }
}
