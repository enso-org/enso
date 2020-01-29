//! This module implements `IntervalCounter`, a helper to determine how many
//! intervals of an arbitrary length a time duration has and `FixedStepAnimator`, an utility to
//! guarantee an `AnimationCallback` is running at a fixed rate.

use super::Animator;
use super::AnimationCallback;

use nalgebra::zero;



// =======================
// === IntervalCounter ===
// =======================

/// This struct counts the intervals in a time period.
#[derive(Clone,Copy,Debug)]
pub struct IntervalCounter {
    /// Interval duration.
    pub interval_duration : f64,

    /// The amount of time accumulated by `IntervalCounter`'s `add_time`.
    pub accumulated_time  : f64
}

impl IntervalCounter {
    /// Creates `IntervalCounter` which counts how many intervals with an `interval_duration`.
    pub fn new(interval_duration:f64) -> Self {
        let accumulated_time = zero();
        Self { interval_duration, accumulated_time }
    }

    /// Adds time to the counter and returns the number of intervals it reached.
    pub fn add_time(&mut self, time:f64) -> u32 {
        self.accumulated_time += time;
        let count = (self.accumulated_time / self.interval_duration) as u32;
        self.accumulated_time -= count as f64 * self.interval_duration;
        count
    }
}



// =============================
// === FixedStepAnimatorData ===
// =============================

struct FixedStepAnimatorData {
    callback : Box<dyn AnimationCallback>,
    counter  : IntervalCounter
}

impl FixedStepAnimatorData {
    pub fn new<F:AnimationCallback>(steps_per_second:f64, f:F) -> Self {
        let callback         = Box::new(f);
        let step_duration    = 1000.0 / steps_per_second;
        let counter          = IntervalCounter::new(step_duration);
        Self {callback,counter}
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
#[derive(Debug)]
pub struct FixedStepAnimator {
    _animator: Animator
}

impl FixedStepAnimator {
    /// Registers `FixedStepAnimator` in `EventLoop`, running `AnimationCallback` at a fixed rate
    /// determined by `steps_per_second`.
    pub fn new<F:AnimationCallback>(steps_per_second:f64, f:F) -> Self {
        let mut data = FixedStepAnimatorData::new(steps_per_second, f);
        let _animator = Animator::new(move |delta_ms| {
            let intervals = data.counter.add_time(delta_ms);
            for _ in 0..intervals {
                (data.callback)(data.counter.interval_duration);
            }
        });
        Self { _animator }
    }
}
