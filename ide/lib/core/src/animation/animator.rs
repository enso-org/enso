pub mod continuous;
pub mod fixed_step;

use continuous::ContinuousAnimator;
use crate::system::web::animation_frame_loop::AnimationFrameLoop;



// =========================
// === AnimationCallback ===
// =========================

pub trait AnimationCallback = FnMut(f32) + 'static;



// ====================
// === AnimatorData ===
// ====================

struct AnimatorData {
    callback    : Box<dyn AnimationCallback>,
    previous_ms : Option<f32>
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

/// This structure runs an animation every frame with the time difference from the last frame as
/// its input.
pub struct Animator {
    _continuous_animator: ContinuousAnimator
}

impl Animator {
    pub fn new<F:AnimationCallback>(mut event_loop:&mut AnimationFrameLoop, f:F) -> Self {
        let mut data             = AnimatorData::new(f);
        let _continuous_animator = ContinuousAnimator::new(&mut event_loop, move |current_ms| {
            if let Some(previous_ms) = data.previous_ms {
                let delta_ms = current_ms - previous_ms;
                (data.callback)(delta_ms);
            }
            data.previous_ms = Some(current_ms);
        });
        Self { _continuous_animator }
    }
}
