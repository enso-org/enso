//! This module implements easing formulas and related utilities. To learn more about easing
//! functions, please refer to the following link: https://easings.net/en

use crate::prelude::*;

use crate::animation;
use crate::data::function::Fn1;

use core::f32::consts::PI;


// ==============
// === Export ===
// ==============

pub use crate::animation::physics::inertia::EndStatus;



// ========================
// === Easing functions ===
// ========================

/// Easing function signature.
pub trait FnEasing = 'static + Fn(f32) -> f32;

/// Easing function signature.
pub trait AnyFnEasing = 'static + Fn(f32) -> f32;

/// Easing function signature.
pub trait CloneableFnEasing = 'static + Clone + Fn(f32) -> f32;

macro_rules! define_in_out_easing_fn {
    (fn $tname:ident $name:ident $lambda:expr) => {
        paste::item! {
            /// A $name-in function type.
            pub type [<$tname In>]    = impl Clone + Fn(f32) -> f32;
            /// A $name-out function type.
            pub type [<$tname Out>]   = impl Clone + Fn(f32) -> f32;
            /// A $name-in-out function type.
            pub type [<$tname InOut>] = impl Clone + Fn(f32) -> f32;

            /// A $name-in transition.
            pub fn [<$name _in>]() -> [<$tname In>] { $lambda }
            /// A $name-out transition.
            pub fn [<$name _out>]() -> [<$tname Out>] { |t| { 1.0 - [<$name _in>]()(1.0 - t) } }
            /// A $name-in-out transition.
            pub fn [<$name _in_out>]() -> [<$tname InOut>] { |t| {
                let t = t * 2.0;
                if t < 1.0 { [<$name _in>]()(t) / 2.0 }
                else       { ([<$name _out>]()(t - 1.0) + 1.0) / 2.0 }
            }}

            // FIXME: Crashes the compiler. To be fixed one day.
            // impl Default for [<$tname In>]    { fn default() -> Self { [<$name _in>]() } }
            // impl Default for [<$tname Out>]   { fn default() -> Self { [<$name _out>]() } }
            // impl Default for [<$tname InOut>] { fn default() -> Self { [<$name _in_out>]() } }
        }
    };
}

macro_rules! define_in_out_easing_fns {
    ($(fn $tname:ident $name:ident $lambda:expr)*) => {
        $(define_in_out_easing_fn!(fn $tname $name $lambda);)*
    }
}

define_in_out_easing_fns! {
    fn Circ    circ    |t| { 1.0 - (1.0 - t * t).sqrt() }
    fn Quad    quad    |t| { t * t }
    fn Cubic   cubic   |t| { t * t * t }
    fn Quart   quart   |t| { t * t * t * t }
    fn Quint   quint   |t| { t * t * t * t }
    fn Expo    expo    |t| { if t == 0.0 {0.0} else {2.0_f32.powf(10.0 * (t - 1.0))} }
    fn Sine    sine    |t| { -(t * PI/2.0).cos() + 1.0 }
    fn Back    back    |t| { back_in_params(t, 1.70158) }
    fn Elastic elastic |t| { elastic_in_params(t, 0.3, 1.0) }
    fn Bounce  bounce  |t| {
        if t < 1.0 / 2.75 { 7.5625 * t * t }
        else if t < 2.0 / 2.75 {
            let t = t - 1.5 / 2.75;
            7.5625 * t * t + 0.75
        } else if t < 2.5 / 2.75 {
            let t = t - 2.25 / 2.75;
            7.5625 * t * t + 0.9375
        } else {
            let t = t - 2.625 / 2.75;
            7.5625 * t * t + 0.984_375
        }
    }
}

/// Linear transition type.
pub type Linear = impl Clone + Fn(f32) -> f32;

/// Linear transition.
pub fn linear() -> Linear {
    |t| t
}

/// A back-in transition with params.
pub fn back_in_params(t: f32, overshoot: f32) -> f32 {
    t * t * ((overshoot + 1.0) * t - overshoot)
}

/// A back-out transition with params.
pub fn back_out_params(t: f32, overshoot: f32) -> f32 {
    1.0 - back_in_params(1.0 - t, overshoot)
}

/// A back-in-out transition with params.
pub fn back_in_out_params(t: f32, overshoot: f32) -> f32 {
    let t = t * 2.0;
    if t < 1.0 {
        back_in_params(t, overshoot) / 2.0
    } else {
        (back_out_params(t - 1.0, overshoot) + 1.0) / 2.0
    }
}

/// An elastic-in transition with params.
pub fn elastic_in_params(t: f32, period: f32, amplitude: f32) -> f32 {
    let mut amplitude = amplitude;
    let overshoot = if amplitude <= 1.0 {
        amplitude = 1.0;
        period / 4.0
    } else {
        period / (2.0 * PI) * (1.0 / amplitude).asin()
    };
    let elastic = amplitude * 2.0_f32.powf(-10.0 * t);
    elastic * ((t * 1.0 - overshoot) * (2.0 * PI) / period).sin() + 1.0
}

/// An elastic-out transition with params.
pub fn elastic_out_params(t: f32, period: f32, amplitude: f32) -> f32 {
    1.0 - elastic_in_params(1.0 - t, period, amplitude)
}

/// An elastic-in-out transition with params.
pub fn elastic_in_out_params(t: f32, period: f32, amplitude: f32) -> f32 {
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
pub trait Value = Copy + Add<Self, Output = Self> + Mul<f32, Output = Self> + PartialEq + 'static;

/// Easing animator callback.
pub trait Callback<T> = Fn1<T> + 'static;

/// Handy alias for `Simulator` with a boxed closure callback.
pub type DynAnimator<T, F> = Animator<T, F, Box<dyn Fn(f32)>, Box<dyn Fn(EndStatus)>>;

/// Easing animator. Allows animating any value which implements `Value` according to one of the
/// tween functions.
#[derive(CloneRef, Derivative)]
#[derivative(Clone(bound = ""))]
pub struct Animator<T, F, OnStep = (), OnEnd = ()> {
    data:           Rc<AnimatorData<T, F, OnStep, OnEnd>>,
    animation_loop: AnimationLoop<T, F, OnStep, OnEnd>,
}

impl<T, F, OnStep, OnEnd> Deref for Animator<T, F, OnStep, OnEnd> {
    type Target = Rc<AnimatorData<T, F, OnStep, OnEnd>>;
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T, F, OnStep, OnEnd> Debug for Animator<T, F, OnStep, OnEnd> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Animator")
    }
}

/// Internal data of `Animator`.
#[derive(Derivative)]
#[derivative(Debug(bound = "T:Debug+Copy"))]
#[allow(missing_docs)]
pub struct AnimatorData<T, F, OnStep, OnEnd> {
    pub duration:     Cell<f32>,
    pub start_value:  Cell<T>,
    pub target_value: Cell<T>,
    pub value:        Cell<T>,
    pub active:       Cell<bool>,
    #[derivative(Debug = "ignore")]
    pub tween_fn:     F,
    #[derivative(Debug = "ignore")]
    pub callback:     OnStep,
    #[derivative(Debug = "ignore")]
    pub on_end:       OnEnd,
}

impl<T: Value, F, OnStep, OnEnd> AnimatorData<T, F, OnStep, OnEnd>
where
    F: AnyFnEasing,
    OnStep: Callback<T>,
    OnEnd: Callback<EndStatus>,
{
    fn new(start: T, end: T, tween_fn: F, callback: OnStep, on_end: OnEnd) -> Self {
        let duration = Cell::new(1000.0);
        let value = Cell::new(start);
        let start_value = Cell::new(start);
        let target_value = Cell::new(end);
        let active = default();
        Self { duration, start_value, target_value, value, active, tween_fn, callback, on_end }
    }

    fn step(&self, time: f32) {
        let sample = (time / self.duration.get()).min(1.0);
        let weight = (self.tween_fn)(sample);
        let value = self.start_value.get() * (1.0 - weight) + self.target_value.get() * weight;
        let finished = (sample - 1.0).abs() < std::f32::EPSILON;
        self.callback.call(value);
        self.value.set(value);
        if finished {
            self.active.set(false);
            self.on_end.call(EndStatus::Normal);
        }
    }
}

impl<T: Value, F, OnStep, OnEnd> Animator<T, F, OnStep, OnEnd>
where
    F: AnyFnEasing,
    OnStep: Callback<T>,
    OnEnd: Callback<EndStatus>,
{
    /// Constructor.
    pub fn new_not_started(start: T, end: T, tween_fn: F, callback: OnStep, on_end: OnEnd) -> Self {
        let data = Rc::new(AnimatorData::new(start, end, tween_fn, callback, on_end));
        let animation_loop = default();
        Self { data, animation_loop }
    }

    /// Constructor.
    pub fn new(start: T, end: T, tween_fn: F, callback: OnStep, on_end: OnEnd) -> Self {
        let this = Self::new_not_started(start, end, tween_fn, callback, on_end);
        this.start();
        this
    }

    /// Start the animator.
    pub fn start(&self) {
        if self.animation_loop.get().is_none() {
            let animation_loop = animation::Loop::new(step(self));
            self.animation_loop.set(Some(animation_loop));
            self.data.active.set(true);
        }
    }

    /// Stop the animator.
    pub fn stop(&self) {
        self.animation_loop.set(None);
        self.data.active.set(false);
    }

    /// Resets the animator.
    pub fn reset(&self) {
        self.stop();
        self.start();
    }

    /// Stop the animation, rewind it to the provided value and call the callback.
    pub fn stop_and_rewind_to(&self, value: T) {
        self.stop();
        self.set_start_value_no_restart(value);
        self.data.value.set(value);
        self.data.callback.call(value);
        self.data.on_end.call(EndStatus::Forced);
    }

    /// Stop the animation, rewind it to the initial value and call the callback.
    pub fn stop_and_rewind(&self) {
        self.stop_and_rewind_to(self.start_value());
    }

    /// Check whether the animator is running.
    pub fn active(&self) -> bool {
        self.data.active.get()
    }

    /// Restart the animation using the current value as the new start value.
    pub fn from_now_to(&self, tgt: T) {
        let current = self.value();
        self.data.start_value.set(current);
        self.data.target_value.set(tgt);
        if current != tgt {
            self.data.callback.call(self.start_value());
            self.data.on_end.call(EndStatus::Forced);
            self.reset();
        }
    }

    /// Set the new target value. In case the target value is different from the current target
    /// value, the animation will be restarted with the current value being the new start value.
    pub fn set_target_value(&self, tgt: T) {
        if self.data.target_value.get() != tgt {
            self.from_now_to(tgt)
        }
    }

    /// Stop the animator and set it to the target value.
    pub fn skip(&self) {
        self.stop();
        let value = self.target_value();
        self.data.value.set(value);
        self.data.callback.call(value);
        self.data.on_end.call(EndStatus::Forced);
    }
}


// === Getters & Setters ===

#[allow(missing_docs)]
impl<T: Value, F, OnStep, OnEnd> Animator<T, F, OnStep, OnEnd>
where
    F: AnyFnEasing,
    OnStep: Callback<T>,
    OnEnd: Callback<EndStatus>,
{
    pub fn start_value(&self) -> T {
        self.data.start_value.get()
    }

    pub fn value(&self) -> T {
        self.data.value.get()
    }

    pub fn target_value(&self) -> T {
        self.data.target_value.get()
    }

    pub fn set_start_value_no_restart(&self, t: T) {
        self.data.start_value.set(t);
    }

    pub fn set_target_value_no_restart(&self, t: T) {
        self.data.target_value.set(t);
    }

    pub fn set_duration(&self, t: f32) {
        self.data.duration.set(t);
    }
}


// =====================
// === AnimationLoop ===
// =====================

/// A wrapper over animation loop implementation. This type is defined mainly to make Rust type
/// inferencer happy (not infer infinite, recursive types).
#[derive(CloneRef, Derivative)]
#[derivative(Clone(bound = ""))]
#[derivative(Default(bound = ""))]
#[allow(clippy::type_complexity)]
#[allow(missing_debug_implementations)]
pub struct AnimationLoop<T, F, OnStep, OnEnd> {
    animation_loop: Rc<CloneCell<Option<AnimationStep<T, F, OnStep, OnEnd>>>>,
}

#[allow(clippy::type_complexity)]
impl<T, F, OnStep, OnEnd> Deref for AnimationLoop<T, F, OnStep, OnEnd> {
    type Target = Rc<CloneCell<Option<AnimationStep<T, F, OnStep, OnEnd>>>>;
    fn deref(&self) -> &Self::Target {
        &self.animation_loop
    }
}

impl<T, F, OnStep, OnEnd> AnimationLoop<T, F, OnStep, OnEnd> {
    /// Downgrade to a week reference.
    pub fn downgrade(&self) -> WeakAnimationLoop<T, F, OnStep, OnEnd> {
        let animation_loop = Rc::downgrade(&self.animation_loop);
        WeakAnimationLoop { animation_loop }
    }
}

/// A weak wrapper over animation loop implementation. This type is defined mainly to make Rust type
/// inferencer happy (not infer infinite, recursive types).
#[allow(clippy::type_complexity)]
#[allow(missing_debug_implementations)]
pub struct WeakAnimationLoop<T, F, OnStep, OnEnd> {
    animation_loop: Weak<CloneCell<Option<AnimationStep<T, F, OnStep, OnEnd>>>>,
}

impl<T, F, OnStep, OnEnd> WeakAnimationLoop<T, F, OnStep, OnEnd> {
    /// Upgrade the weak reference.
    pub fn upgrade(&self) -> Option<AnimationLoop<T, F, OnStep, OnEnd>> {
        self.animation_loop.upgrade().map(|animation_loop| AnimationLoop { animation_loop })
    }
}


// === Animation Step ===

/// Alias for `FixedFrameRateLoop` with specified step callback.
pub type AnimationStep<T, F, OnStep, OnEnd> = animation::Loop<Step<T, F, OnStep, OnEnd>>;

/// Callback for an animation step.
pub type Step<T, F, OnStep, OnEnd> = impl Fn(animation::TimeInfo);

fn step<T: Value, F, OnStep, OnEnd>(
    easing: &Animator<T, F, OnStep, OnEnd>,
) -> Step<T, F, OnStep, OnEnd>
where
    F: AnyFnEasing,
    OnStep: Callback<T>,
    OnEnd: Callback<EndStatus>, {
    let data = easing.data.clone_ref();
    let animation_loop = easing.animation_loop.downgrade();
    move |time: animation::TimeInfo| {
        if data.active.get() {
            data.step(time.local)
        } else if let Some(animation_loop) = animation_loop.upgrade() {
            animation_loop.set(None);
        }
    }
}
