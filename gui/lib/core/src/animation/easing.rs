//! This module implements easing formulas and related utilities. To learn more about easing
//! functions, please refer to the following link: https://easings.net/en

// TODO: It would be useful to make the easing functions polymorphic. Maybe we can make use of
// nalgebra::RealField?

use core::f32::consts::PI;

/// Easing function signature.
pub trait FnEasing = 'static + Fn(f32) -> f32;



// ========================
// === Easing functions ===
// ========================

macro_rules! easing_fn {
    (pub fn $name:ident(t:f32) -> f32 $block:block) => { paste::item! {
        /// A $name-in transition.
        pub fn [<$name _in>](t:f32) -> f32 $block
        /// A $name-out transition.
        pub fn [<$name _out>](t:f32) -> f32 { 1.0 - [<$name _in>](1.0 - t) }
        /// A $name-in-out transition.
        pub fn [<$name _in_out>](t:f32) -> f32 {
            let t = t * 2.0;
            if t < 1.0 {
                [<$name _in>](t) / 2.0
            } else {
                ([<$name _out>](t - 1.0) + 1.0) / 2.0
            }
        }
    } };
}

macro_rules! easing_fns {
    ($(pub fn $name:ident(t:f32) -> f32 $block:block),*) => {
        $(easing_fn!(pub fn $name(t:f32) -> f32 $block);)*
    }
}

easing_fns!(
pub fn bounce(t:f32) -> f32 {
    if t < 1.0 / 2.75 { (7.5625 * t * t) }
    else if t < 2.0 / 2.75 {
        let t = t - 1.5 / 2.75;
        (7.5625 * t * t + 0.75)
    } else if t < 2.5 / 2.75 {
        let t = t - 2.25 / 2.75;
        (7.5625 * t * t + 0.9375)
    } else {
        let t = t - 2.625 / 2.75;
        (7.5625 * t * t + 0.984375)
    }
},

pub fn circ(t:f32) -> f32 { 1.0 - (1.0 - t * t).sqrt() },

pub fn quad(t:f32) -> f32 { t * t },

pub fn cubic(t:f32) -> f32 { t * t * t },

pub fn quart(t:f32) -> f32 { t * t * t * t },

pub fn quint(t:f32) -> f32 { t * t * t * t },

pub fn expo(t:f32) -> f32 {
    if t == 0.0 {
        0.0
    } else {
        2.0_f32.powf(10.0 * (t - 1.0))
    }
},

pub fn sine(t:f32) -> f32 { - (t * PI/2.0).cos() + 1.0 },

pub fn back(t:f32) -> f32 { back_in_params(t, 1.70158) },

pub fn elastic(t:f32) -> f32 { elastic_in_params(t, 0.3, 1.0) }
);

/// A linear transition.
pub fn linear(t:f32) -> f32 { t }

/// A back-in transition with params.
pub fn back_in_params(t:f32, overshoot:f32) -> f32 { t * t * ((overshoot + 1.0) * t - overshoot) }

/// A back-out transition with params.
pub fn back_out_params(t:f32, overshoot:f32) -> f32 {
    1.0 - back_in_params(1.0 - t, overshoot)
}

/// A back-in-out transition with params.
pub fn back_in_out_params(t:f32, overshoot:f32) -> f32 {
    let t = t * 2.0;
    if t < 1.0 {
        back_in_params(t, overshoot) / 2.0
    } else {
        (back_out_params(t - 1.0, overshoot) + 1.0) / 2.0
    }
}

/// An elastic-in transition with params.
pub fn elastic_in_params(t:f32, period:f32, amplitude:f32) -> f32 {
    let mut amplitude = amplitude;
    let overshoot     = if amplitude <= 1.0 {
        amplitude = 1.0;
        period / 4.0
    } else {
        period / (2.0 * PI) * (1.0 / amplitude).asin()
    };
    let elastic = amplitude * 2.0_f32.powf(-10.0 * t);
    elastic * ((t * 1.0 - overshoot) * (2.0 * PI) / period).sin() + 1.0
}

/// An elastic-out transition with params.
pub fn elastic_out_params(t:f32, period:f32, amplitude:f32) -> f32 {
    1.0 - elastic_in_params(1.0 - t, period, amplitude)
}

/// An elastic-in-out transition with params.
pub fn elastic_in_out_params(t:f32, period:f32, amplitude:f32) -> f32 {
    let t = t * 2.0;
    if t < 1.0 {
        elastic_in_params(t, period, amplitude) / 2.0
    } else {
        (elastic_out_params(t - 1.0, period, amplitude) + 1.0) / 2.0
    }
}