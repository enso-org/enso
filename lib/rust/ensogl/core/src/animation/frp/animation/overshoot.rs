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
use crate::application;
use crate::application::frp::API;
use crate::application::Application;
use crate::data::color;
use crate::display;
use crate::display::shape::StyleWatchFrp;
use crate::Animation;

use enso_frp as frp;



// ==================
// === Constants  ===
// ==================

/// Determines how much of out-of-bounds portion of value change is actually applied to the
/// animation target value. Larger values will make the overshoot more noticeable.
const OUT_OF_BOUNDS_SENSITIVITY: f32 = 0.3;


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
        /// Indicates whether the value is currently being animated.
        animating (bool),
    }
}



// ====================================
// === OvershootAnimation component ===
// ====================================

/// An [`Animation`] wrapper that allows the value to be smoothly manipulated within set bounds,
/// overshoot set bounds and bounce back.
#[derive(Clone, CloneRef, Debug, Shrinkwrap)]
pub struct OvershootAnimation {
    pub frp: FrpEndpoints,
}

impl OvershootAnimation {
    pub fn new(network: &frp::Network) -> Self {
        let frp = Frp::extend(network);
        // let out = &frp.private().output; // version for `define_endpoints_2`
        let out = &frp.source;

        let animation = Animation::<f32>::new(network);

        let soft_bounce_cool_off = DelayedAnimation::new(network);
        soft_bounce_cool_off.frp.set_delay(50.0);
        soft_bounce_cool_off.frp.set_duration(0.0);

        frp::extend! { network
            frp.soft_change_to <+ frp.soft_change_by.map2(&animation.value,|delta,val| *val+*delta);
            frp.hard_change_to <+ frp.hard_change_by.map2(&animation.value,|delta,val| *val+*delta);

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
            out.animating <+ bool(&animation.on_end, &animation.value).on_change();
        }

        frp.set_min_bound(0.0);
        frp.set_max_bound(0.0);
        // Sensible absolute limit highly depends on context. So by default, we don't want to limit
        // at all. Setting the limit to infinity effectively disables it.
        frp.set_overshoot_limit(f32::INFINITY);

        OvershootAnimation { frp }
    }
}

// impl Deref for OvershootAnimation {
//     type Target = Frp;
//     fn deref(&self) -> &Self::Target {
//         &self.frp
//     }
// }
