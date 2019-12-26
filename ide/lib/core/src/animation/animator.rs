use super::ContinuousTimeAnimator;
use super::FnAnimation;



// ====================
// === AnimatorData ===
// ====================

struct AnimatorData {
    closure     : Box<dyn FnMut(f32)>,
    previous_ms : Option<f32>
}

impl AnimatorData {
    pub fn new<F:FnAnimation>(f:F) -> Self {
        let closure     = Box::new(f);
        let previous_ms = None;
        Self { closure, previous_ms }
    }
}



// ================
// === Animator ===
// ================

/// This structure runs an animation every frame with the time difference from the last frame as
/// its input.
pub struct Animator {
    _continuous_animator: ContinuousTimeAnimator
}

impl Animator {
    pub fn new<F:FnAnimation>(f:F) -> Self {
        let mut data             = AnimatorData::new(f);
        let _continuous_animator = ContinuousTimeAnimator::new(move |current_ms| {
            if let Some(previous_ms) = data.previous_ms {
                let delta_ms = current_ms - previous_ms;
                (data.closure)(delta_ms);
            }
            data.previous_ms = Some(current_ms);
        });
        Self { _continuous_animator }
    }
}
