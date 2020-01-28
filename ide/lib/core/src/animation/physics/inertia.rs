//! This module implements physics components to simulate a rubber band dynamics.
//! The components has the potential to be further developed and extended in the future into a
//! more sophisticated physics simulator.

use crate::prelude::*;

use crate::animation::animator::Animator;
use crate::animation::animator::fixed_step::IntervalCounter;
use crate::animation::linear_interpolation;

use nalgebra::Vector3;
use nalgebra::zero;



// ====================
// === PhysicsForce ===
// ====================

/// A trait for implementing 3 dimensional forces.
pub trait PhysicsForce {
    /// Gets the calculated force.
    fn force(&self, kinematics:&KinematicsProperties) -> Vector3<f32>;
}



// ======================
// === DragProperties ===
// ======================

/// This structure contains air dragging properties.
#[derive(Default, Clone, Copy)]
pub struct DragProperties {
    /// Drag`s coefficient.
    pub coefficient: f32
}

impl DragProperties {
    /// Creates `DragProperties` with drag's `coefficient`.
    pub fn new(coefficient:f32) -> Self {
        Self { coefficient }
    }
}

impl PhysicsForce for DragProperties {
    fn force(&self, kinematics:&KinematicsProperties) -> Vector3<f32> {
        -kinematics.velocity * self.coefficient
    }
}



// ========================
// === SpringProperties ===
// ========================

/// This structure contains spring physics properties.
#[derive(Debug, Clone, Copy)]
pub struct SpringProperties {
    /// Spring's coefficient.
    pub coefficient : f32,
    /// Spring's fixed point.
    pub fixed_point : Vector3<f32>
}

impl Default for SpringProperties {
    fn default() -> Self {
        Self::new(zero(),zero())
    }
}

impl SpringProperties {
    /// Creates `SpringProperties` with spring's `coefficient` and `fixed_point`.
    pub fn new(coefficient:f32, fixed_point:Vector3<f32>) -> Self {
        Self { coefficient,fixed_point }
    }
}

impl PhysicsForce for SpringProperties {
    fn force(&self, kinematics:&KinematicsProperties) -> Vector3<f32> {
        let delta     = self.fixed_point - kinematics.position;
        let delta_len = delta.magnitude();
        if delta_len > 0.0 {
            let force_val = delta_len * self.coefficient;
            delta.normalize() * force_val
        } else {
            zero()
        }
    }
}



// ============================
// === KinematicProperties ===
// ============================

/// This structure contains kinematics properties.
#[derive(Debug,Clone,Copy)]
pub struct KinematicsProperties {
    position     : Vector3<f32>,
    velocity     : Vector3<f32>,
    acceleration : Vector3<f32>,
    mass         : f32
}

impl Default for KinematicsProperties {
    fn default() -> Self {
        Self::new(zero(),zero(),zero(),zero())
    }
}

impl KinematicsProperties {
    /// Creates `KinematicsProperties` with `position`, `velocity`, `acceleration` and `mass`.
    pub fn new
    (position:Vector3<f32>, velocity:Vector3<f32>, acceleration:Vector3<f32>, mass:f32) -> Self {
        Self { position,velocity,acceleration,mass }
    }
}


// === Getters ===

impl KinematicsProperties {
    /// `Position` getter.
    pub fn position(&self) -> Vector3<f32> {
        self.position
    }

    /// `Velocity` getter.
    pub fn velocity(&self) -> Vector3<f32> {
        self.velocity
    }

    /// `Acceleration` getter.
    pub fn acceleration(&self) -> Vector3<f32> {
        self.acceleration
    }

    /// `Mass` getter.
    pub fn mass(&self) -> f32 {
        self.mass
    }
}


// === Setters ===

impl KinematicsProperties {
    /// `Position` setter.
    pub fn set_position(&mut self, position:Vector3<f32>) {
        self.position = position
    }

    /// `Velocity` setter.
    pub fn set_velocity(&mut self, velocity:Vector3<f32>) {
        self.velocity = velocity
    }

    /// `Acceleration` setter.
    pub fn set_acceleration(&mut self, acceleration:Vector3<f32>) {
        self.acceleration = acceleration
    }

    /// `Mass` setter.
    pub fn set_mass(&mut self, mass:f32) {
        self.mass = mass
    }
}



// =============================
// === PhysicsPropertiesData ===
// =============================

struct PhysicsPropertiesData {
    kinematics : KinematicsProperties,
    spring     : SpringProperties,
    drag       : DragProperties
}

impl PhysicsPropertiesData {
    pub fn new
    (kinematics: KinematicsProperties, spring:SpringProperties, drag:DragProperties) -> Self {
        Self { kinematics,spring,drag }
    }
}



// =========================
// === PhysicsProperties ===
// =========================

/// A structure including kinematics, drag and spring properties.
#[derive(Clone)]
pub struct PhysicsProperties {
    data : Rc<RefCell<PhysicsPropertiesData>>
}

impl PhysicsProperties {
    /// Creates  `PhysicsProperties` with `kinematics`, `spring` and `drag`.
    pub fn new
    (kinematics: KinematicsProperties, spring:SpringProperties, drag:DragProperties) -> Self {
        let data = Rc::new(RefCell::new(PhysicsPropertiesData::new(kinematics,spring,drag)));
        Self { data }
    }
}


// === Getters ===

impl PhysicsProperties {
    /// `KinematicsProperties` getter.
    pub fn kinematics(&self) -> KinematicsProperties { self.data.borrow().kinematics }
    /// `SpringProperties` getter.
    pub fn spring    (&self) -> SpringProperties     { self.data.borrow().spring }
    /// `DragProperties` getter.
    pub fn drag      (&self) -> DragProperties       { self.data.borrow().drag }
}


// === Setters ===

impl PhysicsProperties {
    /// Safe accessor to modify `KinematicsProperties`.
    pub fn mod_kinematics<F:FnOnce(&mut KinematicsProperties)>(&mut self, f:F) {
        let mut kinematics = self.kinematics();
        f(&mut kinematics);
        self.set_kinematics(kinematics);
    }

    /// `KinematicsProperties` setter.
    pub fn set_kinematics(&mut self, kinematics:KinematicsProperties) {
        self.data.borrow_mut().kinematics = kinematics;
    }

    /// Safe accessor to modify `SpringProperties`.
    pub fn mod_spring<F:FnOnce(&mut SpringProperties)>(&mut self, f:F) {
        let mut spring = self.spring();
        f(&mut spring);
        self.set_spring(spring);
    }

    /// `SpringProperties` setter.
    pub fn set_spring(&mut self, spring:SpringProperties) {
        self.data.borrow_mut().spring = spring;
    }

    /// Safe accessor to modify `DragProperties`.
    pub fn mod_drag<F:FnOnce(&mut DragProperties)>(&mut self, f:F) {
        let mut drag = self.drag();
        f(&mut drag);
        self.set_drag(drag);
    }

    /// `DragProperties` setter.
    pub fn set_drag(&mut self, drag:DragProperties) {
        self.data.borrow_mut().drag = drag;
    }
}



// ========================
// === PhysicsSimulator ===
// ========================

/// A callback used by PhysicsSimulator.
pub trait PhysicsCallback = FnMut(Vector3<f32>) + 'static;

/// A fixed step physics simulator used to simulate `PhysicsProperties`.
pub struct PhysicsSimulator {
    _animator : Animator
}

impl PhysicsSimulator {
    /// Simulates `Properties` and inputs `Kinematics`' position in `PhysicsCallback`.
    pub fn new<F:PhysicsCallback>
    ( steps_per_second:f64
    , mut properties:PhysicsProperties
    , mut callback:F) -> Self {
        let step_ms              = 1000.0 / steps_per_second;
        let mut current_position = properties.kinematics().position();
        let mut next_position    = simulate(&mut properties, step_ms);
        let mut interval_counter = IntervalCounter::new(step_ms);
        let _animator            = Animator::new(move |delta_ms| {
            let intervals = interval_counter.add_time(delta_ms);
            for _ in 0..intervals {
                current_position = next_position;
                next_position    = simulate(&mut properties,step_ms);
            }

            let transition = interval_counter.accumulated_time / interval_counter.interval_duration;
            let position   = linear_interpolation(current_position,next_position,transition as f32);
            callback(position);
        });

        Self { _animator }
    }
}

/// Simulate the `KinematicProperties`.
fn simulate_kinematics(kinematics:&mut KinematicsProperties, force:&Vector3<f32>, dt:f64) {
    let dt = dt as f32;
    kinematics.set_acceleration(force / kinematics.mass);
    kinematics.set_velocity(kinematics.velocity() + kinematics.acceleration() * dt);
    kinematics.set_position(kinematics.position() + kinematics.velocity()     * dt);
}

/// Runs a simulation step.
fn simulate(properties:&mut PhysicsProperties, delta_ms:f64) -> Vector3<f32> {
    let spring        = properties.spring();
    let drag          = properties.drag();
    let mut net_force = zero();
    properties.mod_kinematics(|mut kinematics| {
        net_force += spring.force(&kinematics);
        net_force += drag.force(&kinematics);
        let delta_seconds = delta_ms / 1000.0;
        simulate_kinematics(&mut kinematics, &net_force, delta_seconds);
    });
    properties.kinematics().position()
}
