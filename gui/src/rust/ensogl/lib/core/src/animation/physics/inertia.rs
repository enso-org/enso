//! This module implements physics components to simulate a rubber band dynamics.
//! The components has the potential to be further developed and extended in the future into a
//! more sophisticated physics simulator.

use crate::prelude::*;

use crate::animation;



// =============
// === Value ===
// =============

/// The type of the value of the simulation. In particular, the Value could be `f32`
/// (1-dimensional simulation), or `Vector3<f32>` (3-dimensional simulation).
pub trait Value
    = 'static + Copy + Default + Debug + Normalize + PartialEq
    + Magnitude <Output=f32>
    + Neg<       Output=Self>
    + Sub<Self , Output=Self>
    + Add<Self , Output=Self>
    + Div<f32  , Output=Self>
    + Mul<f32  , Output=Self>;



// ==================
// === Properties ===
// ==================

macro_rules! define_f32_opr_mods {
    ($name:ident $opr:ident $f:ident) => {
        define_f32_opr_mods_lhs! {$name $opr $f}
        define_f32_opr_mods_rhs! {$name $opr $f}
    }
}

macro_rules! define_f32_opr_mods_lhs {
    ($name:ident $opr:ident $f:ident) => {
        impl $opr<$name> for f32 {
            type Output = $name;
            fn $f(self, rhs:$name) -> $name {
                $name{value : self.$f(rhs.value)}
            }
        }

        impl $opr<&$name> for f32 {
            type Output = $name;
            fn $f(self, rhs:&$name) -> $name {
                $name{value : self.$f(rhs.value)}
            }
        }

        impl $opr<$name> for &f32 {
            type Output = $name;
            fn $f(self, rhs:$name) -> $name {
                $name{value : self.$f(rhs.value)}
            }
        }

        impl $opr<&$name> for &f32 {
            type Output = $name;
            fn $f(self, rhs:&$name) -> $name {
                $name{value : self.$f(rhs.value)}
            }
        }
    }
}

macro_rules! define_f32_opr_mods_rhs {
    ($name:ident $opr:ident $f:ident) => {
        impl $opr<f32> for $name {
            type Output = $name;
            fn $f(self, rhs:f32) -> $name {
                $name{value : self.value.$f(rhs)}
            }
        }

        impl $opr<&f32> for $name {
            type Output = $name;
            fn $f(self, rhs:&f32) -> $name {
                $name{value : self.value.$f(rhs)}
            }
        }

        impl $opr<f32> for &$name {
            type Output = $name;
            fn $f(self, rhs:f32) -> $name {
                $name{value : self.value.$f(rhs)}
            }
        }

        impl $opr<&f32> for &$name {
            type Output = $name;
            fn $f(self, rhs:&f32) -> $name {
                $name{value : self.value.$f(rhs)}
            }
        }
    }
}

macro_rules! define_self_opr_mods {
    ($name:ident $opr:ident $f:ident) => {
        impl $opr<$name> for $name {
            type Output = $name;
            fn $f(self, rhs:$name) -> $name {
                $name{value : self.value.$f(rhs.value)}
            }
        }

        impl $opr<&$name> for $name {
            type Output = $name;
            fn $f(self, rhs:&$name) -> $name {
                $name{value : self.value.$f(rhs.value)}
            }
        }

        impl $opr<$name> for &$name {
            type Output = $name;
            fn $f(self, rhs:$name) -> $name {
                $name{value : self.value.$f(rhs.value)}
            }
        }

        impl $opr<&$name> for &$name {
            type Output = $name;
            fn $f(self, rhs:&$name) -> $name {
                $name{value : self.value.$f(rhs.value)}
            }
        }
    }
}

macro_rules! define_property {
    ($name:ident = $default:expr) => {
        /// Simulation property.
        #[derive(Debug,Clone,Copy,Into,From)]
        pub struct $name {
            /// Internal value of the $name.
            pub value : f32,
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
                Self {value}
            }
        }

        define_self_opr_mods!    {$name Add add}
        define_self_opr_mods!    {$name Sub sub}
        define_f32_opr_mods!     {$name Mul mul}
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
#[derive(Clone,Copy,Debug)]
#[allow(missing_docs)]
pub struct Thresholds {
    pub distance : f32,
    pub speed    : f32
}

impl Default for Thresholds {
    fn default() -> Self {
        Self::new(0.1,0.1)
    }
}

impl Thresholds {
    /// Constructor.
    pub fn new(distance:f32, speed:f32) -> Self {
        Self {distance,speed}
    }
}



// ======================
// === SimulationData ===
// ======================

/// A fixed step physics simulator used to simulate `PhysicsState`.
#[derive(Clone,Copy,Debug,Default)]
pub struct SimulationData<T> {
    value        : T,
    target_value : T,
    velocity     : T,
    mass         : Mass,
    spring       : Spring,
    drag         : Drag,
    thresholds   : Thresholds,
    active       : bool,
}

impl<T:Value> SimulationData<T> {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Runs a simulation step.
    fn step(&mut self, delta_seconds:f32) {
        if self.active {
            let velocity      = self.velocity.magnitude();
            let distance      = (self.value - self.target_value).magnitude();
            let snap_velocity = velocity < self.thresholds.speed;
            let snap_distance = distance < self.thresholds.distance;
            let should_snap   = snap_velocity && snap_distance;
            if should_snap {
                self.value    = self.target_value;
                self.velocity = default();
                self.active   = false;
            } else {
                let force        = self.spring_force() + self.drag_force();
                let acceleration = force / self.mass.value;
                self.velocity    = self.velocity + acceleration * delta_seconds;
                self.value       = self.value + self.velocity * delta_seconds;
            }
        }
    }

    /// Compute spring force.
    fn spring_force(&self) -> T {
        let value_delta = self.target_value - self.value;
        let distance    = value_delta.magnitude();
        if distance > 0.0 {
            let coefficient = distance * self.spring.value;
            value_delta.normalize() * coefficient
        } else {
            default()
        }
    }

    /// Compute air drag force.
    fn drag_force(&self) -> T {
        -self.velocity * self.drag.value
    }
}


// === Getters ===

#[allow(missing_docs)]
impl<T:Value> SimulationData<T> {
    pub fn value        (&self) -> T          { self.value }
    pub fn target_value (&self) -> T          { self.target_value }
    pub fn velocity     (&self) -> T          { self.velocity }
    pub fn mass         (&self) -> Mass       { self.mass }
    pub fn spring       (&self) -> Spring     { self.spring }
    pub fn drag         (&self) -> Drag       { self.drag }
    pub fn thresholds   (&self) -> Thresholds { self.thresholds }
    pub fn active       (&self) -> bool       { self.active }
}


// === Setters ===

#[allow(missing_docs)]
impl<T:Value> SimulationData<T> {
    pub fn set_velocity   (&mut self, velocity:T)            { self.velocity   = velocity; }
    pub fn set_mass       (&mut self, mass:Mass)             { self.mass       = mass; }
    pub fn set_spring     (&mut self, spring:Spring)         { self.spring     = spring; }
    pub fn set_drag       (&mut self, drag:Drag)             { self.drag       = drag; }
    pub fn set_thresholds (&mut self, thresholds:Thresholds) { self.thresholds = thresholds; }

    pub fn set_value(&mut self, value:T) {
        self.active = true;
        self.value = value;
    }

    pub fn set_target_value(&mut self, target_value:T) {
        self.active = true;
        self.target_value = target_value;
    }

    pub fn update_value<F:FnOnce(T)->T>(&mut self, f:F) {
        self.set_value(f(self.value()));
    }

    pub fn update_target_value<F:FnOnce(T)->T>(&mut self, f:F) {
        self.set_target_value(f(self.target_value()));
    }

    pub fn update_velocity<F:FnOnce(T)->T>(&mut self, f:F) {
        self.set_velocity(f(self.velocity()));
    }

    pub fn update_mass<F:FnOnce(Mass)->Mass>(&mut self, f:F) {
        self.set_mass(f(self.mass()));
    }

    pub fn update_spring<F:FnOnce(Spring)->Spring>(&mut self, f:F) -> Spring {
        let value = f(self.spring());
        self.set_spring(value);
        value
    }

    pub fn update_drag<F:FnOnce(Drag)->Drag>(&mut self, f:F) {
        self.set_drag(f(self.drag()));
    }

    pub fn update_thresholds<F:FnOnce(Thresholds)->Thresholds>(&mut self, f:F) {
        self.set_thresholds(f(self.thresholds()));
    }

    /// Stop the animator and set it to the target value.
    pub fn skip(&mut self) {
        self.active   = false;
        self.value    = self.target_value;
        self.velocity = default();
    }
}



// ==================
// === Simulation ===
// ==================

/// The main simulation engine. It allows running the simulation by explicitly calling the `step`
/// function. Refer to `Simulator` for a more automated solution.
#[derive(Derivative,CloneRef,Default)]
#[derivative(Clone(bound=""))]
#[derivative(Debug(bound="T:Copy+Debug"))]
pub struct Simulation<T> {
    data : Rc<Cell<SimulationData<T>>>
}

impl<T:Value> Simulation<T> {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Runs a simulation step.
    pub fn step(&self, delta_seconds:f32) {
        let mut data = self.data.get();
        data.step(delta_seconds);
        self.data.set(data);
    }
}


// === Getters ===

#[allow(missing_docs)]
impl<T:Value> Simulation<T> {
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
impl<T:Value> Simulation<T> {
    pub fn set_drag(&self, drag:Drag) {
        self.data.update(|mut sim| {sim.set_drag(drag); sim});
    }

    pub fn update_drag<F:FnOnce(Drag)->Drag>(&self, f:F) {
        self.data.update(|mut sim| {sim.update_drag(f); sim});
    }

    pub fn set_spring(&self, spring:Spring) {
        self.data.update(|mut sim| {sim.set_spring(spring); sim});
    }

    pub fn update_spring<F:FnOnce(Spring)->Spring>(&self, f:F) {
        self.data.update(|mut sim| {sim.update_spring(f); sim});
    }

    pub fn set_mass(&self, mass:Mass) {
        self.data.update(|mut sim| {sim.set_mass(mass); sim});
    }

    pub fn update_mass<F:FnOnce(Mass)->Mass>(&self, f:F) {
        self.data.update(|mut sim| {sim.update_mass(f); sim});
    }

    pub fn set_velocity(&self, velocity:T) {
        self.data.update(|mut sim| {sim.set_velocity(velocity); sim});
    }

    pub fn set_value(&self, value:T) {
        self.data.update(|mut sim| {sim.set_value(value); sim});
    }

    pub fn set_target_value(&self, target_value:T) {
        self.data.update(|mut sim| {sim.set_target_value(target_value); sim});
    }

    pub fn update_target_value<F:FnOnce(T)->T>(&self, f:F) {
        self.data.update(|mut sim| {sim.update_target_value(f); sim});
    }

    pub fn skip(&self) {
        self.data.update(|mut sim| {sim.skip(); sim});
    }
}



// =================
// === Simulator ===
// =================

/// Simulator callback.
pub trait Callback<T> = Fn(T)+'static;

/// Handy alias for `Simulator` with a boxed closure callback.
pub type DynSimulator<T> = Simulator<T,Box<dyn Fn(T)>>;

/// The `Simulation` with an associated animation loop. The simulation is updated every frame in an
/// efficient way – when the simulation finishes, it automatically unregisters in the animation loop
/// and registers back only when needed.
#[derive(CloneRef,Derivative,Shrinkwrap)]
#[derivative(Clone(bound=""))]
pub struct Simulator<T,Cb> {
    #[shrinkwrap(main_field)]
    simulation     : Simulation<T>,
    animation_loop : Rc<CloneCell<Option<FixedFrameRateAnimationStep<T,Cb>>>>,
    frame_rate     : Rc<Cell<f32>>,
    callback       : Rc<Cb>,
}

impl<T:Value,Cb> Simulator<T,Cb>
where Cb : Callback<T> {
    /// Constructor.
    pub fn new(callback:Cb) -> Self {
        let frame_rate     = Rc::new(Cell::new(60.0));
        let callback       = Rc::new(callback);
        let simulation     = Simulation::new();
        let animation_loop = default();
        Self {simulation,animation_loop,frame_rate,callback} . init()
    }
}

impl<T,Cb> Debug for Simulator<T,Cb> {
    fn fmt(&self, f:&mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"Simulator")
    }
}


// === Setters ===

#[allow(missing_docs)]
impl<T:Value,Cb> Simulator<T,Cb>
where Cb : Callback<T> {
    pub fn set_callback(&mut self, callback:Cb) {
        let callback = Rc::new(callback);
        self.callback = callback;
        self.stop();
        self.start();
    }

    pub fn set_value(&self, value:T) {
        self.simulation.set_value(value);
        self.start();
    }

    pub fn set_target_value(&self, target_value:T) {
        if target_value != self.target_value() {
            self.simulation.set_target_value(target_value);
            self.start();
        }
    }

    pub fn update_target_value<F:FnOnce(T)->T>(&self, f:F) {
        self.set_target_value(f(self.target_value()))
    }

    pub fn skip(&self) {
        self.simulation.skip();
        self.stop();
        (self.callback)(self.simulation.value());
    }
}


// === Private API ===

impl<T:Value,Cb> Simulator<T,Cb>
where Cb : Callback<T> {
    fn init(self) -> Self {
        self.start();
        self
    }

    /// Starts the simulation and attaches it to an animation loop.
    fn start(&self) {
        if self.animation_loop.get().is_none() {
            let frame_rate     = self.frame_rate.get();
            let step           = step(&self);
            let animation_loop = animation::Loop::new_with_fixed_frame_rate(frame_rate,step);
            self.animation_loop.set(Some(animation_loop));
        }
    }

    /// Stops the simulation and detaches it from animation loop.
    fn stop(&self) {
        self.animation_loop.set(None);
    }
}

/// Alias for `FixedFrameRateLoop` with specified step callback.
pub type FixedFrameRateAnimationStep<T,Cb> = animation::FixedFrameRateLoop<Step<T,Cb>>;
pub type Step<T,Cb> = impl Fn(animation::TimeInfo);
fn step<T:Value,Cb>(simulator:&Simulator<T,Cb>) -> Step<T,Cb>
where Cb : Callback<T> {
    let this = simulator.clone_ref();
    move |time:animation::TimeInfo| {
        let delta_seconds = time.frame / 1000.0;
        if this.simulation.active() {
            this.simulation.step(delta_seconds);
            (this.callback)(this.simulation.value());
        } else {
            this.stop();
        }
    }
}
