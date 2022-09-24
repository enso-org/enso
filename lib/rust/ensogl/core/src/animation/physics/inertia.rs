//! This module implements physics components to simulate a rubber band dynamics.
//! The components have the potential to be further developed and extended in the future into a
//! more sophisticated physics simulator.

use crate::prelude::*;

use crate::animation;
use crate::data::function::Fn0;
use crate::data::function::Fn1;
use crate::types::unit2::Duration;



// =============
// === Value ===
// =============

/// The type of the value of the simulation. In particular, the Value could be `f32`
/// (1-dimensional simulation), or `Vector3<f32>` (3-dimensional simulation).
pub trait Value = 'static
    + Copy
    + Default
    + PartialEq
    + Normalize
    + Magnitude<Output = f32>
    + Add<Output = Self>
    + Mul<f32, Output = Self>;



// ==================
// === Properties ===
// ==================

macro_rules! define_f32_opr_mods {
    ($name:ident $opr:ident $f:ident) => {
        define_f32_opr_mods_lhs! {$name $opr $f}
        define_f32_opr_mods_rhs! {$name $opr $f}
    };
}

macro_rules! define_f32_opr_mods_lhs {
    ($name:ident $opr:ident $f:ident) => {
        impl $opr<$name> for f32 {
            type Output = $name;
            fn $f(self, rhs: $name) -> $name {
                $name { value: self.$f(rhs.value) }
            }
        }

        impl $opr<&$name> for f32 {
            type Output = $name;
            fn $f(self, rhs: &$name) -> $name {
                $name { value: self.$f(rhs.value) }
            }
        }

        impl $opr<$name> for &f32 {
            type Output = $name;
            fn $f(self, rhs: $name) -> $name {
                $name { value: self.$f(rhs.value) }
            }
        }

        impl $opr<&$name> for &f32 {
            type Output = $name;
            fn $f(self, rhs: &$name) -> $name {
                $name { value: self.$f(rhs.value) }
            }
        }
    };
}

macro_rules! define_f32_opr_mods_rhs {
    ($name:ident $opr:ident $f:ident) => {
        impl $opr<f32> for $name {
            type Output = $name;
            fn $f(self, rhs: f32) -> $name {
                $name { value: self.value.$f(rhs) }
            }
        }

        impl $opr<&f32> for $name {
            type Output = $name;
            fn $f(self, rhs: &f32) -> $name {
                $name { value: self.value.$f(rhs) }
            }
        }

        impl $opr<f32> for &$name {
            type Output = $name;
            fn $f(self, rhs: f32) -> $name {
                $name { value: self.value.$f(rhs) }
            }
        }

        impl $opr<&f32> for &$name {
            type Output = $name;
            fn $f(self, rhs: &f32) -> $name {
                $name { value: self.value.$f(rhs) }
            }
        }
    };
}

macro_rules! define_self_opr_mods {
    ($name:ident $opr:ident $f:ident) => {
        impl $opr<$name> for $name {
            type Output = $name;
            fn $f(self, rhs: $name) -> $name {
                $name { value: self.value.$f(rhs.value) }
            }
        }

        impl $opr<&$name> for $name {
            type Output = $name;
            fn $f(self, rhs: &$name) -> $name {
                $name { value: self.value.$f(rhs.value) }
            }
        }

        impl $opr<$name> for &$name {
            type Output = $name;
            fn $f(self, rhs: $name) -> $name {
                $name { value: self.value.$f(rhs.value) }
            }
        }

        impl $opr<&$name> for &$name {
            type Output = $name;
            fn $f(self, rhs: &$name) -> $name {
                $name { value: self.value.$f(rhs.value) }
            }
        }
    };
}

macro_rules! define_property {
    ($name:ident = $default:expr) => {
        /// SimulationDataCell property.
        #[derive(Debug, Clone, Copy, Into, From)]
        pub struct $name {
            /// Internal value of the $name.
            pub value: f32,
        }

        impl $name {
            /// Constructor.
            pub fn new() -> Self {
                default()
            }
        }

        impl Default for $name {
            fn default() -> Self {
                let value = $default;
                Self { value }
            }
        }

        define_self_opr_mods! {$name Add add}
        define_self_opr_mods! {$name Sub sub}
        define_f32_opr_mods! {$name Mul mul}
        define_f32_opr_mods_rhs! {$name Div div}
    };
}

define_property! { Drag   = 1500.0 }
define_property! { Spring = 20000.0 }
define_property! { Mass   = 30.0 }



// ==================
// === Thresholds ===
// ==================

/// Thresholds defining the values which define when simulation stops.
#[derive(Clone, Copy, Debug)]
#[allow(missing_docs)]
pub struct Thresholds {
    pub distance: f32,
    pub speed:    f32,
}

impl Default for Thresholds {
    fn default() -> Self {
        Self::new(0.1, 0.1)
    }
}

impl Thresholds {
    /// Constructor.
    pub fn new(distance: f32, speed: f32) -> Self {
        Self { distance, speed }
    }
}



// ======================
// === SimulationData ===
// ======================

/// A fixed step physics simulator data.
///
/// Please note that the simulator equations are not physically correct and were optimized for
/// performance and animation correctness (lack of numerical errors). In particular, the aerodynamic
/// drag force increases linearly with the velocity instead of with velocity ^ 2. Moreover, please
/// note that because of the simplified equation model the simulator behaves the same way regardless
/// the scale. Animation between 0.1 and 0.15 looks the same was as between 10 and 15. The only part
/// that needs to be tweaked are the snapping thresholds, which you can set with the `set_precision`
/// function.
///
/// The equations used here are:
/// ```text
///     spring_force = spring_stretch * self.spring;
///     drag_force   = - velocity * self.drag;
///     force        = spring_force + drag_force;
///     acceleration = force / self.mass;
///     new_velocity = velocity + acceleration * delta_time;
/// ```
#[derive(Clone, Copy, Debug, Default)]
pub struct SimulationData<T> {
    // We store the current value as an offset from the target rather than an absolute value. This
    // reduces numerical errors when animating floating point numbers: The offset will become very
    // small towards the end of the animation. Small floating point numbers offer higher precision
    // than large ones, because more digits can be used behind the point, for the fractional part
    // of the number. This higher precision helps us to avoid non-termination that could
    // otherwise happen due to rounding errors in our `step` function.
    //
    // For example: The precision of `f32` values is so low that we can only represent every second
    // integer above 16 777 216. If we simulate values that large and represented the simulation's
    // state by its current total value then this internal state would have to jump over those
    // gaps. The animation would either become to fast (if we rounded the steps up) or slow
    // down too early (if we rounded the steps down). Generally, it would be difficult to
    // handle the rounding errors gracefully. By representing the state as an offset, we
    // achieve the highest possible precision as the animation approaches its target. Large
    // rounding errors might only happen when the simulation is still far away from the target.
    // But in those situations, high precision is not as important.
    offset_from_target: T,
    target_value:       T,
    velocity:           T,
    mass:               Mass,
    spring:             Spring,
    drag:               Drag,
    thresholds:         Thresholds,
    active:             bool,
}

impl<T: Value> SimulationData<T> {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Runs a simulation step.
    fn step(&mut self, delta_seconds: Duration) {
        if self.active {
            let velocity = self.velocity.magnitude();
            let distance = self.offset_from_target.magnitude();
            let snap_velocity = velocity < self.thresholds.speed;
            let snap_distance = distance < self.thresholds.distance;
            let should_snap = snap_velocity && snap_distance;
            if should_snap || distance.is_nan() {
                self.offset_from_target = default();
                self.velocity = default();
                self.active = false;
            } else {
                let force = self.spring_force() + self.drag_force();
                let acceleration = force * (1.0 / self.mass.value);
                let delta_seconds = delta_seconds.unchecked_raw();
                self.velocity = self.velocity + acceleration * delta_seconds;
                self.offset_from_target = self.offset_from_target + self.velocity * delta_seconds;
            }
        }
    }

    /// Compute spring force.
    fn spring_force(&self) -> T {
        self.offset_from_target * -self.spring.value
    }

    /// Compute air drag force. Please note that this is physically incorrect. Read the docs of
    /// `SimulationData` to learn more.
    fn drag_force(&self) -> T {
        self.velocity * -1.0 * self.drag.value
    }

    /// Set the snapping thresholds of the animation. The rule of thumb is that you should set the
    /// precision to one order of magnitude smaller value than your unit value. For example, when
    /// moving things on the stage with a single pixel being your unit value, you should set the
    /// precision to 0.1. In case of a color alpha animation between 0.0 and 1.0, two digits after
    /// the dot are important, so your precision should be 0.001.
    fn set_precision(&mut self, precision: f32) {
        self.thresholds.speed = precision;
        self.thresholds.distance = precision;
    }
}


// === Getters ===

#[allow(missing_docs)]
impl<T: Value> SimulationData<T> {
    pub fn value(&self) -> T {
        self.target_value + self.offset_from_target
    }
    pub fn target_value(&self) -> T {
        self.target_value
    }
    pub fn velocity(&self) -> T {
        self.velocity
    }
    pub fn mass(&self) -> Mass {
        self.mass
    }
    pub fn spring(&self) -> Spring {
        self.spring
    }
    pub fn drag(&self) -> Drag {
        self.drag
    }
    pub fn thresholds(&self) -> Thresholds {
        self.thresholds
    }
    pub fn active(&self) -> bool {
        self.active
    }
}


// === Setters ===

#[allow(missing_docs)]
impl<T: Value> SimulationData<T> {
    pub fn set_velocity(&mut self, velocity: T) {
        self.velocity = velocity;
    }
    pub fn set_mass(&mut self, mass: Mass) {
        self.mass = mass;
    }
    pub fn set_spring(&mut self, spring: Spring) {
        self.spring = spring;
    }
    pub fn set_drag(&mut self, drag: Drag) {
        self.drag = drag;
    }
    pub fn set_thresholds(&mut self, thresholds: Thresholds) {
        self.thresholds = thresholds;
    }

    pub fn set_value(&mut self, value: T) {
        self.active = true;
        self.offset_from_target = value + self.target_value * -1.0;
    }

    pub fn set_target_value(&mut self, target_value: T) {
        self.active = true;
        let old_target_value = self.target_value;
        self.target_value = target_value;
        self.offset_from_target = old_target_value + self.offset_from_target + target_value * -1.0;
    }

    pub fn update_value<F: FnOnce(T) -> T>(&mut self, f: F) {
        self.set_value(f(self.value()));
    }

    pub fn update_target_value<F: FnOnce(T) -> T>(&mut self, f: F) {
        self.set_target_value(f(self.target_value()));
    }

    pub fn update_velocity<F: FnOnce(T) -> T>(&mut self, f: F) {
        self.set_velocity(f(self.velocity()));
    }

    pub fn update_mass<F: FnOnce(Mass) -> Mass>(&mut self, f: F) {
        self.set_mass(f(self.mass()));
    }

    pub fn update_spring<F: FnOnce(Spring) -> Spring>(&mut self, f: F) -> Spring {
        let value = f(self.spring());
        self.set_spring(value);
        value
    }

    pub fn update_drag<F: FnOnce(Drag) -> Drag>(&mut self, f: F) {
        self.set_drag(f(self.drag()));
    }

    pub fn update_thresholds<F: FnOnce(Thresholds) -> Thresholds>(&mut self, f: F) {
        self.set_thresholds(f(self.thresholds()));
    }

    /// Stop the animator and set it to the target value.
    pub fn skip(&mut self) {
        self.active = false;
        self.offset_from_target = default();
        self.velocity = default();
    }
}



// ==========================
// === SimulationDataCell ===
// ==========================

/// The main simulation engine. It allows running the simulation by explicitly calling the `step`
/// function. Refer to `Simulator` for a more automated solution.
#[derive(Derivative, Default)]
#[derivative(Debug(bound = "T:Copy+Debug"))]
pub struct SimulationDataCell<T> {
    data: Cell<SimulationData<T>>,
}

impl<T: Value> SimulationDataCell<T> {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Runs a simulation step.
    pub fn step(&self, delta_seconds: Duration) {
        let mut data = self.data.get();
        data.step(delta_seconds);
        self.data.set(data);
    }
}


// === Getters ===

#[allow(missing_docs)]
impl<T: Value> SimulationDataCell<T> {
    pub fn active(&self) -> bool {
        self.data.get().active()
    }

    pub fn value(&self) -> T {
        self.data.get().value()
    }

    pub fn target_value(&self) -> T {
        self.data.get().target_value()
    }

    pub fn drag(&self) -> Drag {
        self.data.get().drag()
    }

    pub fn spring(&self) -> Spring {
        self.data.get().spring()
    }

    pub fn mass(&self) -> Mass {
        self.data.get().mass()
    }
}


// === Setters ===

#[allow(missing_docs)]
impl<T: Value> SimulationDataCell<T> {
    pub fn set_drag(&self, drag: Drag) {
        self.data.update(|mut sim| {
            sim.set_drag(drag);
            sim
        });
    }

    pub fn update_drag<F: FnOnce(Drag) -> Drag>(&self, f: F) {
        self.data.update(|mut sim| {
            sim.update_drag(f);
            sim
        });
    }

    pub fn set_spring(&self, spring: Spring) {
        self.data.update(|mut sim| {
            sim.set_spring(spring);
            sim
        });
    }

    pub fn update_spring<F: FnOnce(Spring) -> Spring>(&self, f: F) {
        self.data.update(|mut sim| {
            sim.update_spring(f);
            sim
        });
    }

    pub fn set_mass(&self, mass: Mass) {
        self.data.update(|mut sim| {
            sim.set_mass(mass);
            sim
        });
    }

    pub fn update_mass<F: FnOnce(Mass) -> Mass>(&self, f: F) {
        self.data.update(|mut sim| {
            sim.update_mass(f);
            sim
        });
    }

    pub fn set_velocity(&self, velocity: T) {
        self.data.update(|mut sim| {
            sim.set_velocity(velocity);
            sim
        });
    }

    pub fn set_value(&self, value: T) {
        self.data.update(|mut sim| {
            sim.set_value(value);
            sim
        });
    }

    pub fn set_target_value(&self, target_value: T) {
        self.data.update(|mut sim| {
            sim.set_target_value(target_value);
            sim
        });
    }

    pub fn update_target_value<F: FnOnce(T) -> T>(&self, f: F) {
        self.data.update(|mut sim| {
            sim.update_target_value(f);
            sim
        });
    }

    pub fn set_precision(&self, precision: f32) {
        self.data.update(|mut sim| {
            sim.set_precision(precision);
            sim
        });
    }

    pub fn skip(&self) {
        self.data.update(|mut sim| {
            sim.skip();
            sim
        });
    }
}



// =================
// === Callbacks ===
// =================

/// Simulator callback.
pub trait Callback0 = 'static + Fn0;
pub trait Callback1<T> = 'static + Fn1<T>;



// =====================
// === SimulatorData ===
// =====================

/// Internal data of `Simulator`.
#[derive(Derivative, Default)]
#[derivative(Debug(bound = "T:Copy+Debug"))]
pub struct SimulatorData<T, OnStep, OnStart, OnEnd> {
    simulation: SimulationDataCell<T>,
    frame_rate: Cell<f32>,
    #[derivative(Debug = "ignore")]
    on_step:    OnStep,
    #[derivative(Debug = "ignore")]
    on_start:   OnStart,
    #[derivative(Debug = "ignore")]
    on_end:     OnEnd,
}

impl<T, OnStep, OnStart, OnEnd> Deref for SimulatorData<T, OnStep, OnStart, OnEnd> {
    type Target = SimulationDataCell<T>;
    fn deref(&self) -> &Self::Target {
        &self.simulation
    }
}

impl<T: Value, OnStep, OnStart, OnEnd> SimulatorData<T, OnStep, OnStart, OnEnd> {
    /// Constructor.
    pub fn new(on_step: OnStep, on_start: OnStart, on_end: OnEnd) -> Self {
        let simulation = SimulationDataCell::new();
        let frame_rate = Cell::new(60.0);
        Self { simulation, frame_rate, on_step, on_start, on_end }
    }
}

impl<T: Value, OnStep, OnStart, OnEnd> SimulatorData<T, OnStep, OnStart, OnEnd>
where
    OnStep: Callback1<T>,
    OnEnd: Callback1<EndStatus>,
{
    /// Proceed with the next simulation step for the given time delta.
    pub fn step(&self, delta_seconds: Duration) -> bool {
        let is_active = self.simulation.active();
        if is_active {
            self.simulation.step(delta_seconds);
            self.on_step.call(self.simulation.value());
        } else {
            self.on_end.call(EndStatus::Normal);
        }
        is_active
    }
}



// =================
// === Simulator ===
// =================

/// Handy alias for `Simulator` with a boxed closure callback.
pub type DynSimulator<T> = Simulator<T, Box<dyn Fn(T)>, (), Box<dyn Fn(EndStatus)>>;

/// The `SimulationDataCell` with an associated animation loop. The simulation is updated every
/// frame in an efficient way – when the simulation finishes, it automatically unregisters the
/// animation loop and registers back only when needed.
#[derive(CloneRef, Derivative)]
#[derivative(Clone(bound = ""))]
pub struct Simulator<T, OnStep, OnStart, OnEnd> {
    data:           Rc<SimulatorData<T, OnStep, OnStart, OnEnd>>,
    animation_loop: AnimationLoopSlot,
}

impl<T, OnStep, OnStart, OnEnd> Deref for Simulator<T, OnStep, OnStart, OnEnd> {
    type Target = Rc<SimulatorData<T, OnStep, OnStart, OnEnd>>;
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T, OnStep, OnStart, OnEnd> Simulator<T, OnStep, OnStart, OnEnd>
where
    T: Value,
    OnStep: Callback1<T>,
    OnStart: Callback0,
    OnEnd: Callback1<EndStatus>,
{
    /// Constructor.
    pub fn new(callback: OnStep, on_start: OnStart, on_end: OnEnd) -> Self {
        let data = Rc::new(SimulatorData::new(callback, on_start, on_end));
        let animation_loop = default();
        Self { data, animation_loop }.init()
    }
}

impl<T, OnStep, OnStart, OnEnd> Debug for Simulator<T, OnStep, OnStart, OnEnd> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Simulator")
    }
}


// === Setters ===

#[allow(missing_docs)]
impl<T, OnStep, OnStart, OnEnd> Simulator<T, OnStep, OnStart, OnEnd>
where
    T: Value,
    OnStep: Callback1<T>,
    OnStart: Callback0,
    OnEnd: Callback1<EndStatus>,
{
    pub fn set_value(&self, value: T) {
        self.simulation.set_value(value);
        self.start();
    }

    pub fn set_target_value(&self, target_value: T) {
        if target_value != self.target_value() {
            self.simulation.set_target_value(target_value);
            self.start();
        }
    }

    pub fn update_target_value<F: FnOnce(T) -> T>(&self, f: F) {
        self.set_target_value(f(self.target_value()))
    }
}


// === Private API ===

impl<T, OnStep, OnStart, OnEnd> Simulator<T, OnStep, OnStart, OnEnd>
where
    T: Value,
    OnStep: Callback1<T>,
    OnStart: Callback0,
    OnEnd: Callback1<EndStatus>,
{
    fn init(self) -> Self {
        self.start();
        self
    }

    /// Starts the simulation and attaches it to an animation loop.
    fn start(&self) {
        if self.animation_loop.get().is_none() {
            let frame_rate = self.frame_rate.get();
            let step = step(self);
            let on_too_many_frames_skipped = on_too_many_frames_skipped(self);
            let animation_loop = animation::Loop::new_with_fixed_frame_rate(
                frame_rate,
                step,
                on_too_many_frames_skipped,
            );
            self.animation_loop.set(Some(animation_loop));
            self.on_start.call();
        }
    }

    /// Skip the simulation and set the target value as its result.
    pub fn skip(&self) {
        self.simulation.skip();
        self.on_step.call(self.simulation.value());
        self.stop();
    }

    /// Stops the simulation and detaches it from animation loop.
    fn stop(&self) {
        self.animation_loop.set(None);
        self.on_end.call(EndStatus::Forced);
    }
}

impl<T, OnStep, OnStart, OnEnd> Simulator<T, OnStep, OnStart, OnEnd> {
    /// Downgrade to a weak reference.
    pub fn downgrade(&self) -> WeakSimulator<T, OnStep, OnStart, OnEnd> {
        let data = self.data.clone_ref();
        let animation_loop = self.animation_loop.downgrade();
        WeakSimulator { data, animation_loop }
    }
}


// =====================
// === WeakSimulator ===
// =====================

/// Weak version of [`Simulator`].
#[derive(CloneRef, Derivative)]
#[derivative(Clone(bound = ""))]
pub struct WeakSimulator<T, OnStep, OnStart, OnEnd> {
    data:           Rc<SimulatorData<T, OnStep, OnStart, OnEnd>>,
    animation_loop: WeakAnimationLoopSlot,
}

impl<T, OnStep, OnStart, OnEnd> WeakSimulator<T, OnStep, OnStart, OnEnd> {
    /// Try upgrading to a string reference.
    pub fn upgrade(&self) -> Option<Simulator<T, OnStep, OnStart, OnEnd>> {
        let data = self.data.clone_ref();
        self.animation_loop.upgrade().map(|animation_loop| Simulator { data, animation_loop })
    }
}

impl<T, OnStep, OnStart, OnEnd> Debug for WeakSimulator<T, OnStep, OnStart, OnEnd> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "WeakSimulator")
    }
}



// =========================
// === AnimationLoopSlot ===
// =========================

/// A slot for an animation loop. It will be empty if the animation is not active – either if it was
/// not startd yet or it was already finished.
#[derive(CloneRef, Derivative)]
#[derivative(Clone(bound = ""))]
#[derivative(Default(bound = ""))]
#[allow(clippy::type_complexity)]
#[allow(missing_debug_implementations)]
pub struct AnimationLoopSlot {
    animation_loop: Rc<CloneCell<Option<animation::Loop>>>,
}

#[allow(clippy::type_complexity)]
impl Deref for AnimationLoopSlot {
    type Target = Rc<CloneCell<Option<animation::Loop>>>;
    fn deref(&self) -> &Self::Target {
        &self.animation_loop
    }
}

impl AnimationLoopSlot {
    /// Downgrade to a week reference.
    pub fn downgrade(&self) -> WeakAnimationLoopSlot {
        let animation_loop = Rc::downgrade(&self.animation_loop);
        WeakAnimationLoopSlot { animation_loop }
    }
}


// =============================
// === WeakAnimationLoopSlot ===
// =============================

/// A weak version of [`AnimationLoopSlot`].
#[allow(clippy::type_complexity)]
#[allow(missing_debug_implementations)]
#[derive(CloneRef, Derivative)]
#[derivative(Clone(bound = ""))]
pub struct WeakAnimationLoopSlot {
    animation_loop: Weak<CloneCell<Option<animation::Loop>>>,
}

impl WeakAnimationLoopSlot {
    /// Upgrade the weak reference.
    pub fn upgrade(&self) -> Option<AnimationLoopSlot> {
        self.animation_loop.upgrade().map(|animation_loop| AnimationLoopSlot { animation_loop })
    }
}


// ==========================
// === FixedFrameRateLoop ===
// ==========================

/// Callback for an animation step.
pub type Step<T, OnStep, OnStart, OnEnd> = impl Fn(animation::TimeInfo);

fn step<T, OnStep, OnStart, OnEnd>(
    simulator: &Simulator<T, OnStep, OnStart, OnEnd>,
) -> Step<T, OnStep, OnStart, OnEnd>
where
    T: Value,
    OnStep: Callback1<T>,
    OnStart: Callback0,
    OnEnd: Callback1<EndStatus>, {
    let weak_simulator = simulator.downgrade();
    move |time: animation::TimeInfo| {
        if let Some(simulator) = weak_simulator.upgrade() {
            let delta_seconds = time.previous_frame / 1000.0;
            if !simulator.step(delta_seconds) {
                simulator.animation_loop.set(None)
            }
        }
    }
}

/// Callback for an animation step.
pub type OnTooManyFramesSkipped<T, OnStep, OnStart, OnEnd> = impl Fn();

fn on_too_many_frames_skipped<T, OnStep, OnStart, OnEnd>(
    simulator: &Simulator<T, OnStep, OnStart, OnEnd>,
) -> OnTooManyFramesSkipped<T, OnStep, OnStart, OnEnd>
where
    T: Value,
    OnStep: Callback1<T>,
    OnStart: Callback0,
    OnEnd: Callback1<EndStatus>, {
    let weak_simulator = simulator.downgrade();
    move || {
        if let Some(simulator) = weak_simulator.upgrade() {
            simulator.skip()
        }
    }
}



// =================
// === EndStatus ===
// =================

/// Status of the simulation end. It is either normal, which happens when the animation finishes, or
/// forced, which means that it was forced by the user (for example by using the `stop` function).
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
#[allow(missing_docs)]
pub enum EndStatus {
    Normal,
    Forced,
}

#[allow(missing_docs)]
impl EndStatus {
    pub fn is_normal(self) -> bool {
        self == Self::Normal
    }
    pub fn is_forced(self) -> bool {
        self == Self::Forced
    }
}

impl Default for EndStatus {
    fn default() -> Self {
        Self::Normal
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    /// We test that simulations with target value `f32::NaN` terminate and that their final value
    /// is in fact NaN.
    #[test]
    fn animation_to_nan() {
        let mut data = SimulationData::<f32>::new();
        data.set_value(0.0);
        data.set_target_value(f32::NAN);
        data.step(1.0.ms());
        assert!(data.value().is_nan());
        assert!(!data.active);
    }

    /// We test that simulations with start value `f32::NaN` terminate and reach their target.
    #[test]
    fn animation_from_nan() {
        let mut data = SimulationData::<f32>::new();
        data.set_value(f32::NAN);
        data.set_target_value(0.0);
        data.step(1.0.ms());
        assert_eq!(data.value(), 0.0);
        assert!(!data.active);
    }
}
