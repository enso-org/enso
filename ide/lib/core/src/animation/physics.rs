#![allow(missing_docs)]

use crate::prelude::*;

use crate::animation::{Animator, IntervalCounter, linear_interpolation};
use crate::animation::position::HasPosition;

use nalgebra::Vector3;
use nalgebra::zero;

// ====================
// === PhysicsForce ===
// ====================

pub trait PhysicsForce {
    fn force(&self, kinematics:&KinematicsProperties) -> Vector3<f32>;
}

// ======================
// === DragProperties ===
// ======================

/// This structure contains air dragging properties.
#[derive(Default,Debug,Clone,Copy)]
pub struct DragProperties {
    coefficient: f32
}

impl DragProperties {
    pub fn new(amount:f32) -> Self {
        Self { coefficient: amount }
    }
}

impl PhysicsForce for DragProperties {
    fn force(&self, kinematics:&KinematicsProperties) -> Vector3<f32> {
        let velocity  = kinematics.velocity;
        let speed     = velocity.norm();
        if speed > 0.0 {
            velocity.normalize() * speed * speed * -0.5 * self.coefficient
        } else {
            zero()
        }
    }
}

// === Getters ===

impl DragProperties {
    pub fn coefficient(self) -> f32 {
        self.coefficient
    }
}


// === Setters ===

impl DragProperties {
    pub fn set_coefficient(&mut self, coefficient:f32) {
        self.coefficient = coefficient
    }
}



// ========================
// === SpringProperties ===
// ========================

/// This structure contains spring physics properties.
#[derive(Debug, Clone, Copy)]
pub struct SpringProperties {
    coefficient : f32,
    fixed_point : Vector3<f32>
}

impl Default for SpringProperties {
    fn default() -> Self {
        Self::new(zero(),zero())
    }
}

impl SpringProperties {
    pub fn new(coefficient:f32, fixed_point:Vector3<f32>) -> Self {
        Self { coefficient,fixed_point }
    }
}


// === Getters ===

impl SpringProperties {
    pub fn coefficient(&self) -> f32          { self.coefficient }
    pub fn fixed_point(&self) -> Vector3<f32> { self.fixed_point }
}

impl PhysicsForce for SpringProperties {
    fn force(&self, kinematics:&KinematicsProperties) -> Vector3<f32> {
        let delta     = self.fixed_point() - kinematics.position();
        let delta_len = delta.magnitude();
        if delta_len > 0.0 {
            let force_val = delta_len * self.coefficient();
            delta.normalize() * force_val
        } else {
            zero()
        }
    }
}

// === Setters ===

impl SpringProperties {
    pub fn set_coefficient(&mut self, coefficient:f32)          { self.coefficient = coefficient }
    pub fn set_fixed_point(&mut self, fixed_point:Vector3<f32>) { self.fixed_point = fixed_point }
}



// ============================
// === KinematicProperties ===
// ============================

/// This structure contains kinematics properties.
#[derive(Debug, Clone, Copy)]
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
    pub fn new
    (position:Vector3<f32>, velocity:Vector3<f32>, acceleration:Vector3<f32>, mass:f32) -> Self {
        Self { position,velocity,acceleration,mass }
    }
}


// === Getters ===

impl KinematicsProperties {
    pub fn velocity    (&self) -> Vector3<f32> { self.velocity }
    pub fn acceleration(&self) -> Vector3<f32> { self.acceleration }
    pub fn mass        (&self) -> f32          { self.mass }
}


// === Setters ===

impl KinematicsProperties {
    pub fn set_velocity(&mut self, velocity:Vector3<f32>) {
        self.velocity = velocity
    }

    pub fn set_acceleration(&mut self, acceleration:Vector3<f32>) {
        self.acceleration = acceleration
    }

    pub fn set_mass(&mut self, mass:f32) {
        self.mass = mass
    }
}

impl HasPosition for KinematicsProperties {
    fn position    (&self) -> Vector3<f32>            { self.position }
    fn set_position(&mut self, position:Vector3<f32>) { self.position = position }
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
    pub fn new
    (kinematics: KinematicsProperties, spring:SpringProperties, drag:DragProperties) -> Self {
        let data = Rc::new(RefCell::new(PhysicsPropertiesData::new(kinematics, spring, drag)));
        Self { data }
    }
}


// === Getters ===

impl PhysicsProperties {
    pub fn kinematics(&self) -> KinematicsProperties { self.data.borrow().kinematics }
    pub fn spring    (&self) -> SpringProperties     { self.data.borrow().spring }
    pub fn drag      (&self) -> DragProperties       { self.data.borrow().drag }
}


// === Setters ===

impl PhysicsProperties {
    pub fn mod_kinematics<F:FnOnce(&mut KinematicsProperties)>(&mut self, f:F) {
        let mut kinematics = self.kinematics();
        f(&mut kinematics);
        self.set_kinematics(kinematics);
    }

    pub fn set_kinematics(&mut self, kinematics:KinematicsProperties) {
        self.data.borrow_mut().kinematics = kinematics;
    }

    pub fn mod_spring<F:FnOnce(&mut SpringProperties)>(&mut self, f:F) {
        let mut spring = self.spring();
        f(&mut spring);
        self.set_spring(spring);
    }

    pub fn set_spring(&mut self, spring:SpringProperties) {
        self.data.borrow_mut().spring = spring;
    }

    pub fn mod_drag<F:FnOnce(&mut DragProperties)>(&mut self, f:F) {
        let mut drag = self.drag();
        f(&mut drag);
        self.set_drag(drag);
    }

    pub fn set_drag(&mut self, drag:DragProperties) {
        self.data.borrow_mut().drag = drag;
    }
}



// ========================
// === SimulationObject ===
// ========================

pub trait SimulationObject = HasPosition + 'static;



// ========================
// === PhysicsSimulator ===
// ========================

/// A 60 steps per second physics simulator used to simulate `Properties`.
pub struct PhysicsSimulator {
    _animator : Animator
}

impl PhysicsSimulator {
    /// Simulates `Properties` on `object`.
    pub fn new<T:SimulationObject>(mut object:T, mut properties:PhysicsProperties) -> Self {
        properties.mod_kinematics(|kinematics| { kinematics.set_position(object.position()); });
        let steps_per_second     = 60.0;
        let step_ms              = 1000.0 / steps_per_second;
        let mut current_position = object.position();
        let mut next_position    = simulate(&mut properties, step_ms);
        let mut interval_counter = IntervalCounter::new(step_ms);
        let _animator            = Animator::new(move |delta_ms| {
            let intervals = interval_counter.add_time(delta_ms);
            for _ in 0..intervals {
                current_position = next_position;
                next_position    = simulate(&mut properties, step_ms);
            }

            let transition = interval_counter.accumulated_time / interval_counter.interval_duration;
            let position   = linear_interpolation(current_position, next_position, transition);
            object.set_position(position);
        });

        Self { _animator }
    }
}

/// Simulate the `KinematicProperties`.
fn simulate_kinematics(kinematics:&mut KinematicsProperties, force:&Vector3<f32>, dt:f32) {
    kinematics.set_acceleration(force / kinematics.mass);
    kinematics.set_velocity(kinematics.velocity() + kinematics.acceleration() * dt);
    kinematics.set_position(kinematics.position() + kinematics.velocity()     * dt);
}

/// Runs a simulation step.
fn simulate(properties:&mut PhysicsProperties, delta_ms:f32) -> Vector3<f32> {
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
