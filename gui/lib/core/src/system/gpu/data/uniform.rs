#![allow(missing_docs)]

use crate::prelude::*;

use enum_dispatch::*;
use nalgebra::Matrix4;
use nalgebra::Vector3;
use shapely::shared;
use web_sys::WebGlUniformLocation;

use crate::display::render::webgl::Context;
use crate::system::gpu::data::ContextUniformOps;
use crate::system::gpu::data::GpuData;
use crate::system::web::Logger;



// =============
// === Types ===
// =============

/// A set of constraints that every uniform has to met.
pub trait UniformValue = GpuData where
    AnyUniform : From<Uniform<Self>>,
    Context    : ContextUniformOps<Self>;



// ====================
// === UniformScope ===
// ====================

shared! { UniformScope

/// A scope containing set of uniform values.
#[derive(Clone,Debug)]
pub struct UniformScopeData {
    map    : HashMap<String,AnyUniform>,
    logger : Logger,
}

impl {
    /// Constructor.
    pub fn new(logger: Logger) -> Self {
        let map = default();
        Self {map,logger}
    }

    /// Look up uniform by name.
    pub fn get<Name:Str>(&self, name:Name) -> Option<AnyUniform> {
        self.map.get(name.as_ref()).cloned()
    }

    /// Checks if uniform of a given name was defined in this scope.
    pub fn contains<Name:Str>(&self, name:Name) -> bool {
        self.map.contains_key(name.as_ref())
    }

    /// Add a new uniform with a given name and initial value. Returns `None` if the name is in use.
    pub fn add<Name:Str, Value:UniformValue>
    (&mut self, name:Name, value:Value) -> Option<Uniform<Value>> {
        self.add_or_else(name,value,Some,|_|None)
    }

    /// Add a new uniform with a given name and initial value. Panics if the name is in use.
    pub fn add_or_panic<Name:Str, Value:UniformValue>
    (&mut self, name:Name, value:Value) -> Uniform<Value> {
        self.add_or_else(name,value,|t|{t},|name| {
            panic!("Trying to override uniform '{}'.", name.as_ref())
        })
    }
}}

impl UniformScopeData {
    /// Adds a new uniform with a given name and initial value. In case the name was already in use,
    /// it fires the `fail` function. Otherwise, it fires the `ok` function on the newly created
    /// uniform.
    pub fn add_or_else<Name:Str, Value:UniformValue, Ok:Fn(Uniform<Value>)->T, Fail:Fn(Name)->T, T>
    (&mut self, name:Name, value:Value, ok:Ok, fail:Fail) -> T {
        if self.map.contains_key(name.as_ref()) { fail(name) } else {
            let uniform     = Uniform::new(value);
            let any_uniform = uniform.clone().into();
            self.map.insert(name.into(),any_uniform);
            ok(uniform)
        }
    }
}



// ===================
// === UniformData ===
// ===================

shared! { Uniform

/// An uniform value.
#[derive(Clone,Debug)]
pub struct UniformData<Value> {
    value: Value,
    dirty: bool,
}

impl<Value:UniformValue> {
    /// Constructor.
    pub fn new(value:Value) -> Self {
        let dirty = false;
        Self {value,dirty}
    }

    /// Sets the value of this uniform.
    pub fn set(&mut self, value:Value) {
        self.set_dirty();
        self.value = value;
    }

    /// Modifies the value stored by the uniform.
    pub fn modify<F:FnOnce(&mut Value)>(&mut self, f:F) {
        self.set_dirty();
        f(&mut self.value);
    }

    /// Checks whether the uniform was changed and not yet updated.
    pub fn check_dirty(&self) -> bool {
        self.dirty
    }

    /// Sets the dirty flag.
    pub fn set_dirty(&mut self) {
        self.dirty = true;
    }

    /// Clears the dirty flag.
    pub fn unset_dirty(&mut self) {
        self.dirty = false;
    }

    /// Uploads the uniform data to the provided location of the currently bound shader program.
    pub fn upload(&self, context:&Context, location:&WebGlUniformLocation) {
        context.set_uniform(location,&self.value);
    }
}}



// ==================
// === AnyUniform ===
// ==================

/// Existentially typed uniform value.
#[allow(non_camel_case_types)]
#[enum_dispatch(AnyUniformOps)]
#[derive(Clone,Debug)]
pub enum AnyUniform {
    Variant_i32           (Uniform<i32>),
    Variant_f32           (Uniform<f32>),
    Variant_Vector3_of_f32(Uniform<Vector3<f32>>),
    Variant_Matrix4_of_f32(Uniform<Matrix4<f32>>)
}

/// Set of operations exposed by the `AnyUniform` value.
#[enum_dispatch]
pub trait AnyUniformOps {
    fn upload(&self, context:&Context, location:&WebGlUniformLocation);
}
