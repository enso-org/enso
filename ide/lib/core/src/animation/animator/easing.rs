//! This module provides the implementation of EasingAnimator, an utility which uses a `FnEasing`
//! function to feed a `EasingAnimationCallback` with a value between two `Interpolable` values.

use crate::prelude::*;

use super::ContinuousAnimator;
use crate::animation::easing::FnEasing;
use crate::animation::linear_interpolation;
use crate::animation::Interpolable;

use nalgebra::clamp;



// ===============================
// === EasingAnimationCallback ===
// ===============================

/// Callback used by `EasingAnimator`.
pub trait EasingAnimationCallback<T> = FnMut(T) + 'static;



// ==========================
// === EasingAnimatorData ===
// ==========================

struct EasingAnimatorData<T:Interpolable<T>> {
    initial_value       : T,
    final_value         : T,
    duration_ms: f64,
    continuous_animator : Option<ContinuousAnimator>
}



// ======================
// === EasingAnimator ===
// ======================

/// Argument used in EasingAnimator's constructor.
pub trait InterpolableArgument<T:Copy> = Interpolable<T> + 'static;

/// This struct animates from `origin_position` to `target_position` using easing functions.
pub struct EasingAnimator<T:Interpolable<T>> {
    data : Rc<RefCell<EasingAnimatorData<T>>>
}

impl<T:InterpolableArgument<T>> EasingAnimator<T> {
    /// Creates an EasingAnimator using a `easing_function` to interpolate between `initial_value`
    /// and `final_value` in `duration_seconds`, calling its value in `easing_animation_callback`.
    pub fn new<F:FnEasing,C:EasingAnimationCallback<T>>
    ( mut easing_animation_callback : C
    , easing_function               : F
    , initial_value                 : T
    , final_value                   : T
    , duration_seconds              : f64) -> Self {
        let duration_ms         = duration_seconds * 1000.0;
        let continuous_animator = None;
        let data = EasingAnimatorData{
            initial_value,
            final_value,
            duration_ms,
            continuous_animator
        };
        let data = Rc::new(RefCell::new(data));
        let weak = Rc::downgrade(&data);
        let continuous_animator = ContinuousAnimator::new(move |time_ms| {
            if let Some(data) = weak.upgrade() {
                let data          = data.borrow();
                let duration_ms   = data.duration_ms;
                let initial_value = data.initial_value;
                let final_value   = data.final_value;
                let time_seconds  = time_ms / duration_ms;
                let time_seconds  = clamp(time_seconds, 0.0, 1.0);
                let time_seconds  = easing_function(time_seconds as f32);
                let value         = linear_interpolation(initial_value, final_value, time_seconds);
                easing_animation_callback(value);
            }
        });
        data.borrow_mut().continuous_animator = Some(continuous_animator);
        Self {data}
    }

    /// Starts a new animation with `initial_value` to `final_value` in
    /// `duration_seconds`.
    pub fn animate
    (&mut self, initial_value:T, final_value:T, duration_seconds:f64) {
        let mut data          = self.data.borrow_mut();
        data.initial_value    = initial_value;
        data.final_value      = final_value;
        data.duration_ms      = duration_seconds * 1000.0;
        data.continuous_animator.as_mut().map(|animator| animator.set_time(0.0));
    }
}
