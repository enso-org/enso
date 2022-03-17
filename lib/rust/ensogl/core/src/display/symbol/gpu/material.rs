//! This module defines `Material`, an abstraction for look and feel of a `Symbol`.

use crate::prelude::*;
use crate::system::gpu::types::*;

use crate::display::symbol::shader::builder::CodeTemplate;



// ===============
// === VarDecl ===
// ===============

/// Material's variable declaration. Please note that variables in material do not define whether
/// they are stored as attributes or uniforms. They are bound to the underlying implementation
/// automatically by their name. In case of unsuccessful binding, the default value is used.
#[derive(Clone, Debug)]
pub struct VarDecl {
    /// The GLSL type of the variable.
    pub tp: glsl::PrimType,

    /// Default value of the variable used in case it was not bound to a real value. It's `None` in
    /// case the value does not have glsl representation (for now, only samplers are such
    /// variables).
    pub default: Option<Glsl>,
}

impl VarDecl {
    /// Constructor.
    pub fn new(tp: glsl::PrimType, default: Option<Glsl>) -> Self {
        Self { tp, default }
    }
}

impl<T: PhantomInto<glsl::PrimType> + TryInto<Glsl>> From<T> for VarDecl {
    fn from(t: T) -> Self {
        Self::new(<T>::glsl_prim_type(), t.try_into().ok())
    }
}



// ================
// === Material ===
// ================

/// Abstraction for look and feel of a `Symbol`. Under the hood, material defines a GLSL shader.
#[derive(Clone, Debug, Default, Shrinkwrap)]
#[shrinkwrap(mutable)]
#[shrinkwrap(unsafe_ignore_visibility)]
pub struct Material {
    #[shrinkwrap(main_field)]
    code:    CodeTemplate,
    inputs:  BTreeMap<String, VarDecl>,
    outputs: BTreeMap<String, VarDecl>,
}

/// Bounds for the material inputs.
pub trait Input = Into<VarDecl> + GpuDefault;

impl Material {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Adds a new input variable.
    pub fn add_input<T: Input>(&mut self, name: &str, t: T) {
        self.inputs.insert(name.into(), t.into());
    }

    /// Adds a new output variable.
    pub fn add_output<T: Input>(&mut self, name: &str, t: T) {
        self.outputs.insert(name.into(), t.into());
    }

    /// Adds a new input variable.
    pub fn add_input_def<T: Input>(&mut self, name: &str) {
        self.inputs.insert(name.into(), <T>::gpu_default().into());
    }

    /// Adds a new output variable.
    pub fn add_output_def<T: Input>(&mut self, name: &str) {
        self.outputs.insert(name.into(), <T>::gpu_default().into());
    }
}

impl From<&Material> for Material {
    fn from(t: &Material) -> Self {
        t.clone()
    }
}


// === Getters ===

impl Material {
    /// Gets the GLSL code of this material.
    pub fn code(&self) -> &CodeTemplate {
        &self.code
    }

    /// Gets all registered inputs as a map.
    pub fn inputs(&self) -> &BTreeMap<String, VarDecl> {
        &self.inputs
    }

    /// Gets all registered outputs as a map.
    pub fn outputs(&self) -> &BTreeMap<String, VarDecl> {
        &self.outputs
    }
}


// === Setters ===

impl Material {
    /// Sets the GLSL code of this material. This is a very primitive method. Use it only when
    /// defining primitive materials.
    pub fn set_code<T: Into<CodeTemplate>>(&mut self, code: T) {
        self.code = code.into();
    }
}
