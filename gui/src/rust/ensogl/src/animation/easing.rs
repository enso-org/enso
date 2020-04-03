//! This module implements easing formulas and related utilities. To learn more about easing
//! functions, please refer to the following link: https://easings.net/en

use crate::prelude::*;

use crate::animation;
use core::f32::consts::PI;



// ========================
// === Easing functions ===
// ========================

/// Easing function signature.
pub trait FnEasing = 'static + Fn(f32) -> f32;

macro_rules! easing_fn {
    (pub fn $name:ident(t:f32) -> f32 $block:block) => { paste::item! {
        /// A $name-in transition.
        pub fn [<$name _in>](t:f32) -> f32 $block
        /// A $name-out transition.
        pub fn [<$name _out>](t:f32) -> f32 { 1.0 - [<$name _in>](1.0 - t) }
        /// A $name-in-out transition.
        pub fn [<$name _in_out>](t:f32) -> f32 {
            let t = t * 2.0;
            if t < 1.0 { [<$name _in>](t) / 2.0 }
            else       { ([<$name _out>](t - 1.0) + 1.0) / 2.0 }
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

pub fn circ    (t:f32) -> f32 { 1.0 - (1.0 - t * t).sqrt() },
pub fn quad    (t:f32) -> f32 { t * t },
pub fn cubic   (t:f32) -> f32 { t * t * t },
pub fn quart   (t:f32) -> f32 { t * t * t * t },
pub fn quint   (t:f32) -> f32 { t * t * t * t },
pub fn expo    (t:f32) -> f32 { if t == 0.0 {0.0} else {2.0_f32.powf(10.0 * (t - 1.0))} },
pub fn sine    (t:f32) -> f32 { - (t * PI/2.0).cos() + 1.0 },
pub fn back    (t:f32) -> f32 { back_in_params(t, 1.70158) },
pub fn elastic (t:f32) -> f32 { elastic_in_params(t, 0.3, 1.0) }
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



// ================
// === Animator ===
// ================

/// Easing animator value.
pub trait Value = Copy + Add<Self,Output=Self> + Mul<f32,Output=Self> + 'static;

/// Easing animator callback.
pub trait Callback<T> = Fn(T) + 'static;

/// Easing animator. Allows animating any value which implements `Value` according to one of the
/// easings functions.
#[derive(CloneRef,Derivative)]
#[derivative(Debug(bound="T:Debug"))]
#[derivative(Clone(bound=""))]
pub struct Animator<T:Value,F,Cb> {
    data           : Rc<AnimatorData<T,F,Cb>>,
    animation_loop : Rc<CloneCell<Option<AnimationStep<T,F,Cb>>>>,
}

/// Internal data of `Animator`.
#[derive(Derivative)]
#[derivative(Debug(bound="T:Debug"))]
#[allow(missing_docs)]
pub struct AnimatorData<T:Value,F,Cb> {
    pub duration     : Cell<f32>,
    pub start_value  : Cell<T>,
    pub end_value    : Cell<T>,
    pub active       : Cell<bool>,
    #[derivative(Debug="ignore")]
    pub tween_fn     : F,
    #[derivative(Debug="ignore")]
    pub callback     : Cb,
}

impl<T:Value,F,Cb> AnimatorData<T,F,Cb>
    where F  : FnEasing,
          Cb : Callback<T> {
    fn step(&self, time:f32) {
        let sample = (time / self.duration.get()).min(1.0);
        let weight = (self.tween_fn)(sample);
        let value  = self.start_value.get() * (1.0-weight) + self.end_value.get() * weight;
        (self.callback)(value);
        if (sample - 1.0) < std::f32::EPSILON {
            self.active.set(false);
        }
    }
}

/// Alias for `FixedFrameRateLoop` with specified step callback.
pub type AnimationStep<T,F,Cb> = animation::Loop<Step<T,F,Cb>>;
pub type Step<T,F,Cb> = impl Fn(animation::TimeInfo);
fn step<T:Value,F,Cb>(easing:&Animator<T,F,Cb>) -> Step<T,F,Cb>
    where F  : FnEasing,
          Cb : Callback<T> {
    let this = easing.clone_ref();
    move |time:animation::TimeInfo| {
        if this.active() {
            this.data.step(time.local);
        } else {
            this.stop();
        }
    }
}

impl<T:Value,F,Cb> Animator<T,F,Cb> where F:FnEasing, Cb:Callback<T> {
    /// Constructor.
    pub fn new(start_value:T, end_value:T, tween_fn:F, callback:Cb) -> Self {
        let duration       = Cell::new(1000.0);
        let start_value    = Cell::new(start_value);
        let end_value      = Cell::new(end_value);
        let active         = default();
        let data           = AnimatorData {duration,start_value,end_value,active,tween_fn,callback};
        let data           = Rc::new(data);
        let animation_loop = default();
        Self {data,animation_loop} . init()
    }

    fn init(self) -> Self {
        self.start();
        self
    }

    fn start(&self) {
        if self.animation_loop.get().is_none() {
            let animation_loop = animation::Loop::new(step(&self));
            self.animation_loop.set(Some(animation_loop));
            self.data.active.set(true);
        }
    }

    fn stop(&self) {
        self.animation_loop.set(None);
        self.data.active.set(false);
    }

    /// Resets the animator.
    pub fn reset(&self) {
        self.stop();
        self.start();
    }

    /// Checks whether the animator is running.
    pub fn active(&self) -> bool {
        self.data.active.get()
    }
}

// === Getters & Setters ===

#[allow(missing_docs)]
impl<T:Value,F,Cb> Animator<T,F,Cb> where F:FnEasing, Cb:Callback<T> {
    pub fn start_value(&self) -> T {
        self.data.start_value.get()
    }

    pub fn end_value(&self) -> T {
        self.data.end_value.get()
    }

    pub fn set_start_value(&self, t:T) {
        self.data.start_value.set(t);
        self.start();
    }

    pub fn set_end_value(&self, t:T) {
        self.data.end_value.set(t);
        self.start();
    }

    pub fn set_duration(&self, t:f32) {
        self.data.duration.set(t);
    }
}




