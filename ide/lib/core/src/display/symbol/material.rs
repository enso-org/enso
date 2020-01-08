//! This module defines `Material`, an abstraction for look and feel of a `Symbol`.

use crate::prelude::*;

use crate::system::gpu::data::GpuData;
use crate::display::render::webgl::glsl;
use crate::display::symbol::shader::builder::CodeTemplete;



// ===============
// === VarDecl ===
// ===============

/// Material's variable declaration. Please note that variables in material do not define whether
/// they are stored as attributes or uniforms. They are bound to the underlying implementation
/// automatically by their name. In case of unsuccessful binding, the default value is used.
#[derive(Clone,Debug)]
pub struct VarDecl {
    /// The GLSL type of the variable.
    pub tp: glsl::PrimType,

    /// Default value of the variable used in case it was not bound to a real value.
    pub default : String,
}

impl VarDecl {
    /// Constructor.
    pub fn new(tp:glsl::PrimType, default:String) -> Self {
        Self {tp,default}
    }
}



// ================
// === Material ===
// ================

/// Abstraction for look and feel of a `Symbol`. Under the hood, material defines a GLSL shader.
#[derive(Clone,Debug,Default,Shrinkwrap)]
#[shrinkwrap(mutable)]
#[shrinkwrap(unsafe_ignore_visibility)]
pub struct Material {
    #[shrinkwrap(main_field)]
    code    : CodeTemplete,
    inputs  : BTreeMap<String,VarDecl>,
    outputs : BTreeMap<String,VarDecl>,
}

impl Material {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Adds a new input variable.
    pub fn add_input<Name:Str,T:GpuData>(&mut self, name:Name, t:T) {
        self.inputs.insert(name.into(),Self::make_var_decl(t));
    }

    /// Adds a new output variable.
    pub fn add_output<Name:Str,T:GpuData>(&mut self, name:Name, t:T) {
        self.outputs.insert(name.into(),Self::make_var_decl(t));
    }

    fn make_var_decl<T:GpuData>(t:T) -> VarDecl {
        VarDecl::new(<T as GpuData>::glsl_type(), t.to_glsl())
    }
}

impl From<&Material> for Material {
    fn from(t:&Material) -> Self {
        t.clone()
    }
}


// === Getters ===

impl Material {
    /// Gets the GLSL code of this material.
    pub fn code(&self) -> &CodeTemplete {
        &self.code
    }

    /// Gets all registered inputs as a map.
    pub fn inputs(&self) -> &BTreeMap<String,VarDecl> {
        &self.inputs
    }

    /// Gets all registered outputs as a map.
    pub fn outputs(&self) -> &BTreeMap<String,VarDecl> {
        &self.outputs
    }
}


// === Setters ===

impl Material {
    /// Sets the GLSL code of this material. This is a very primitive method. Use it only when
    /// defining primitive materials.
    pub fn set_code<T:Into<CodeTemplete>>(&mut self, code:T) {
        self.code = code.into();
    }
}
