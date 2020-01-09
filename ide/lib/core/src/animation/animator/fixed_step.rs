#![allow(missing_docs)]

use super::Animator;
use super::AnimationCallback;

use nalgebra::zero;
use crate::system::web::animation_frame_loop::AnimationFrameLoop;



// =======================
// === IntervalCounter ===
// =======================

/// This struct counts the intervals in a time period.
#[derive(Debug)]
pub struct IntervalCounter {
    pub interval_duration : f32,
    pub accumulated_time  : f32
}

impl IntervalCounter {
    pub fn new(interval_duration:f32) -> Self {
        let accumulated_time = zero();
        Self { interval_duration, accumulated_time }
    }

    /// Adds time to the counter and returns the number of intervals it reached.
    pub fn add_time(&mut self, time:f32) -> u32 {
        self.accumulated_time += time;
        let count = (self.accumulated_time / self.interval_duration) as u32;
        self.accumulated_time -= count as f32 * self.interval_duration;
        count
    }

}



// =============================
// === FixedStepAnimatorData ===
// =============================

struct FixedStepAnimatorData {
    closure : Box<dyn FnMut(f32)>,
    counter : IntervalCounter
}

impl FixedStepAnimatorData {
    pub fn new<F:AnimationCallback>(steps_per_second:f32, f:F) -> Self {
        let closure          = Box::new(f);
        let step_duration    = 1000.0 / steps_per_second;
        let counter          = IntervalCounter::new(step_duration);
        Self { closure,counter }
    }
}



// ================
// === Animator ===
// ================

/// This structure attempts to run a closure at a fixed time rate.
///
/// # Internals
/// If, for instance, we want to run AnimationCallback once per second, it's delta_ms
/// (AnimationCallback(delta_ms)) will be 1000ms. But keep in mind that if the actual frame
/// takes longer, say 2000ms, AnimationCallback will be called twice in the same moment, but
/// its delta_ms parameter will always be fixed to 1 second.
pub struct FixedStepAnimator {
    _animator: Animator
}

impl FixedStepAnimator {
    pub fn new<F:AnimationCallback>
    (mut event_loop:&mut AnimationFrameLoop, steps_per_second:f32, f:F) -> Self {
        let mut data = FixedStepAnimatorData::new(steps_per_second, f);
        let _animator = Animator::new(&mut event_loop, move |delta_ms| {
            let intervals = data.counter.add_time(delta_ms);
            for _ in 0..intervals {
                (data.closure)(data.counter.interval_duration);
            }
        });
        Self { _animator }
    }
}
