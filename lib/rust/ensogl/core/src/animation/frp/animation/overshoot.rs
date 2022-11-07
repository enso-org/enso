//! Defines an animation driver for value with soft bounds.
//!
//! This animation is used to animate values with soft bounds. Depending on the input method, the
//! target value might be allowed to exceed the bounds, but after very short time it will smoothly
//! return into the bounds. This is useful for animating scrollbars, where scrolling past the end of
//! the content should be possible for a short time, but the scrollbar should quickly return to the
//! bounds. The sensitivity for setting the value past the bounds is reduced and value is limited to
//! prevent the overshoot from being too large or too jumpy.

use crate::prelude::*;

use crate::animation::delayed::DelayedAnimation;
use crate::animation::linear_interpolation;
use crate::Animation;

use enso_frp as frp;



// ==================
// === Constants  ===
// ==================

/// Determines how much of out-of-bounds portion of value change is actually applied to the
/// animation target value. Larger values will make the overshoot more noticeable.
const OUT_OF_BOUNDS_SENSITIVITY: f32 = 0.24;



// ===========
// === Frp ===
// ===========

crate::define_endpoints! {
    Input {
        /// Apply relative change to target value. Allow the value to overshoot.
        soft_change_by (f32),
        /// Apply relative change to target value. Remove any overshoot.
        hard_change_by (f32),
        /// Apply absolute change to target value. Allow the value to overshoot.
        soft_change_to (f32),
        /// Apply absolute change to target value, Remove any overshoot.
        hard_change_to (f32),
        /// Set the minimum bound of the target value. Values smaller than set bound will be
        /// considered as overshoot and will bounce back.
        set_min_bound (f32),
        /// Set the maximum bound of the target value. Values larger than set bound will be
        /// considered as overshoot will bounce back.
        set_max_bound (f32),
        /// Set the absolute maximum overshoot amplitude that the target value can have. If the
        /// value overshoots more than set limit, it will be immediately clamped to that limit.
        set_overshoot_limit (f32),
        /// Set the time interval from when soft value change is applied to when bounce back
        /// animation starts.
        set_bounce_delay (f32),
        /// Skip the animation and set the target value to the current value.
        skip (),
    }
    Output {
        /// The current value of the animation.
        value (f32),
        /// Current target value of the animation.
        target (f32),
    }
}



// ==========================
// === OvershootAnimation ===
// ==========================

/// An [`Animation`] wrapper that allows the value to be smoothly manipulated within set bounds,
/// overshoot set bounds and bounce back. This is accomplished by modulating and limiting the target
/// value passed to the internal Animation once its out of bounds.
///
/// ## Input controls
/// - The bounds are set using `set_min_bound` and `set_max_bound` endpoints. Set bounds will be
///   applied next time a target value change is attempted.
/// - The target value can be manipulated by relative change using `*_change_by` endpoints or set to
///   absolute value using `*_change_to` endpoints. Using `soft_*` endpoints will limit the target
///   value smoothly over time, while using `hard_*` endpoints will immediately apply the bounds to
///   the target value, not allowing any overshoot.
/// - The maximum allowed out-of-bounds amplitude can be set with `set_overshoot_limit` endpoint.
/// - When an event is emitted to the `skip` endpoint, the animated value will immediately be set
///   the current target value, within the set bounds. Any overshoot is removed.
/// - The delay between when the target value is set and when the bounce back animation starts is
///   controlled with `set_bounce_delay` endpoint.
///
/// ## Overshoot behavior
/// When the target value is set out of bounds using `*_soft` endpoint, the out-of-bounds portion of
/// the value is modulated with `OUT_OF_BOUNDS_SENSITIVITY` constant factor, and potentially limited
/// by value set through `set_overshoot_limit` endpoint. Many target value changes can be applied in
/// quick succession. After a `set_bounce_delay` amount of time passes since last target change, the
/// bounce-back animation is applied by snapping the target value to the closest bound.
#[derive(Clone, CloneRef, Debug, Shrinkwrap)]
pub struct OvershootAnimation {
    /// Public FRP api.
    pub frp: FrpEndpoints,
}

impl OvershootAnimation {
    /// Constructor. Hooks into preexisting network. Created `OvershootAnimation` struct does not
    /// need to be persisted, all created FRP nodes will be managed by the passed-in network.
    pub fn new(network: &frp::Network) -> Self {
        let frp = Frp::extend(network);
        let out = &frp.source;

        let animation = Animation::<f32>::new(network);

        let soft_bounce_cool_off = DelayedAnimation::new(network);
        soft_bounce_cool_off.frp.set_delay(50.0);
        soft_bounce_cool_off.frp.set_duration(0.0);

        frp::extend! { network

            frp.soft_change_to <+ frp.soft_change_by.map2(&animation.target,|delta,val| *val+*delta);
            frp.hard_change_to <+ frp.hard_change_by.map2(&animation.target,|delta,val| *val+*delta);

            // Hard changes are bounded on input.
            input_bounded <- frp.hard_change_to.map3(&frp.set_min_bound, &frp.set_max_bound,
                |t, min, max| t.clamp(*min, *max));


            unbounded_position <- any(frp.soft_change_to, input_bounded);
            bounded_position <- unbounded_position.map3(&frp.set_min_bound, &frp.set_max_bound,
                |t, min, max| t.clamp(*min, *max));
            soft_position <- unbounded_position.map3(&bounded_position, &frp.set_overshoot_limit,
                |unbounded, bounded, overshoot_limit| {
                    let position = linear_interpolation(*bounded, *unbounded, OUT_OF_BOUNDS_SENSITIVITY);
                    position.clamp(bounded - overshoot_limit, bounded + overshoot_limit)
                });

            soft_bounce_cool_off.frp.set_delay <+ frp.set_bounce_delay;
            soft_bounce_cool_off.frp.reset <+_ unbounded_position;
            soft_bounce_cool_off.frp.start <+_ unbounded_position;

            end_bounce_animation <- any(soft_bounce_cool_off.frp.on_end, frp.skip);
            run_bounce_animation <- bool(&end_bounce_animation, &soft_bounce_cool_off.frp.on_reset);

            animation.target <+ run_bounce_animation.switch(&bounded_position, &soft_position);
            animation.skip <+ frp.skip;

            out.value <+ animation.value;
            out.target <+ animation.target;
        }

        frp.set_min_bound(0.0);
        frp.set_max_bound(0.0);
        // Sensible absolute limit highly depends on context. So by default, we don't want to limit
        // at all. Setting the limit to infinity effectively disables it.
        frp.set_overshoot_limit(f32::INFINITY);

        OvershootAnimation { frp }
    }
}
