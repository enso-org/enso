#![allow(missing_docs)]

pub mod upload;

use crate::prelude::*;

use enum_dispatch::*;
use shapely::shared;
use upload::UniformUpload;
use web_sys::WebGlUniformLocation;

use crate::system::gpu::shader::Context;
use crate::system::gpu::data::texture::*;
use crate::system::gpu::data::prim::*;



// ====================
// === UniformValue ===
// ====================

/// Describes every value which can be kept inside an Uniform.
pub trait UniformValue = UniformUpload;

/// Some values need to be initialized before they can be used as uniforms. Textures, for example,
/// need to allocate memory on GPU and if used with remote source, need to download images.
/// For primitive types, like numbers or matrices, the binding operation does nothing.
pub trait IntoUniformValue = IntoUniformValueImpl where
    Uniform<AsUniformValue<Self>>: Into<AnyUniform>;

/// Internal helper for `IntoUniformValue`.
pub trait IntoUniformValueImpl {
    type Result;
    fn into_uniform_value(self, context:&Context) -> Self::Result;
}

/// Result of the binding operation.
pub type AsUniformValue<T> = <T as IntoUniformValueImpl>::Result;


// === Instances ===

macro_rules! define_identity_uniform_value_impl {
    ( [] [$([$t1:ident $t2:ident])*] ) => {$(
        impl IntoUniformValueImpl for $t1<$t2> {
            type Result = $t1<$t2>;
            fn into_uniform_value(self, _context:&Context) -> Self::Result {
                self
            }
        }
    )*}
}
crate::with_all_prim_types!([[define_identity_uniform_value_impl][]]);

impl<Provider:TextureProvider> IntoUniformValueImpl for Provider {
    type Result = Texture<Provider>;
    fn into_uniform_value(self, context:&Context) -> Self::Result {
        self.new_texture(context)
    }
}



// ====================
// === UniformScope ===
// ====================

shared! { UniformScope

/// A scope containing set of uniform values.
#[derive(Debug)]
pub struct UniformScopeData {
    map     : HashMap<String,AnyUniform>,
    logger  : Logger,
    context : Context,
}

impl {
    /// Constructor.
    pub fn new(logger:Logger, context:&Context) -> Self {
        let map     = default();
        let context = context.clone();
        Self {map,logger,context}
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
    /// Please note that the value will be bound to the context before it becomes the uniform.
    /// Refer to the docs of `IntoUniformValue` to learn more.
    pub fn add<Name:Str, Value:IntoUniformValue>
    (&mut self, name:Name, value:Value) -> Option<Uniform<AsUniformValue<Value>>> {
        self.add_or_else(name,value,Some,|_|None)
    }

    /// Add a new uniform with a given name and initial value. Panics if the name is in use.
    /// Please note that the value will be bound to the context before it becomes the uniform.
    /// Refer to the docs of `IntoUniformValue` to learn more.
    pub fn add_or_panic<Name:Str, Value:IntoUniformValue>
    (&mut self, name:Name, value:Value) -> Uniform<AsUniformValue<Value>> {
        self.add_or_else(name,value,|t|{t},|name| {
            panic!("Trying to override uniform '{}'.", name.as_ref())
        })
    }
}}

impl UniformScopeData {
    /// Adds a new uniform with a given name and initial value. In case the name was already in use,
    /// it fires the `fail` function. Otherwise, it fires the `ok` function on the newly created
    /// uniform.
    pub fn add_or_else<Name:Str,Value:IntoUniformValue,Ok,Fail,T>
    (&mut self, name:Name, value:Value, ok:Ok, fail:Fail) -> T
    where Ok   : Fn(Uniform<AsUniformValue<Value>>)->T,
          Fail : Fn(Name)->T {
        if self.map.contains_key(name.as_ref()) { fail(name) } else {
            let bound_value = value.into_uniform_value(&self.context);
            let uniform     = Uniform::new(bound_value);
            let any_uniform = uniform.clone().into();
            self.map.insert(name.into(),any_uniform);
            ok(uniform)
        }
    }
}



// ===============
// === Uniform ===
// ===============

shared! { Uniform

/// An uniform value.
#[derive(Debug)]
pub struct UniformData<Value> {
    value: Value,
    dirty: bool,
}

impl<Value> {
    /// Constructor.
    pub fn new(value:Value) -> Self {
        let dirty = true;
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
}}

impl<Value:UniformValue> UniformData<Value> {
    /// Uploads the uniform data to the provided location of the currently bound shader program.
    pub fn upload(&self, context:&Context, location:&WebGlUniformLocation) {
        self.value.upload_uniform(context,location);
    }
}

impl<Value:UniformValue> Uniform<Value> {
    /// Uploads the uniform data to the provided location of the currently bound shader program.
    pub fn upload(&self, context:&Context, location:&WebGlUniformLocation) {
        self.rc.borrow().upload(context,location)
    }
}

impl<Value> Uniform<Value> where Context : ContextTextureOps<Value,Guard=TextureBindGuard> {
    /// Bind texture in this WebGl context.
    pub fn bind_texture_unit(&self, context:&Context, unit:u32) -> TextureBindGuard {
        let value = &self.rc.borrow().value;
        context.bind_texture_unit(value,unit)
    }
}



// ======================
// === AnyPrimUniform ===
// ======================

macro_rules! define_any_prim_uniform {
    ( [] [$([$t1:ident $t2:ident])*] ) => { paste::item! {
        /// Existentially typed uniform value.
        #[allow(non_camel_case_types)]
        #[enum_dispatch(AnyPrimUniformOps)]
        #[derive(Clone,Debug)]
        pub enum AnyPrimUniform {
            $([<Variant_ $t1 _ $t2>](Uniform<$t1<$t2>>)),*
        }
    }}
}
crate::with_all_prim_types!([[define_any_prim_uniform][]]);

/// Set of operations exposed by the `AnyPrimUniform` value.
#[enum_dispatch]
pub trait AnyPrimUniformOps {
    fn upload(&self, context:&Context, location:&WebGlUniformLocation);
}



// =========================
// === AnyTextureUniform ===
// =========================

macro_rules! gen_any_texture_uniform {
    ( $([$internal_format:tt $type:tt])* ) => { paste::item! {
        #[allow(missing_docs)]
        #[allow(non_camel_case_types)]
        #[enum_dispatch(AnyTextureUniformOps)]
        #[derive(Clone,Debug)]
        pub enum AnyTextureUniform {
            $( [< $internal_format _ $type >] (Uniform<Texture<TextureData<$internal_format,$type>>>) ),*
        }
    }}
}

macro_rules! gen_prim_conversions {
    ( [] [$([$t1:ident $t2:ident])*] ) => {$(
        impl From<Uniform<$t1<$t2>>> for AnyUniform {
            fn from(t:Uniform<$t1<$t2>>) -> Self {
                Self::Prim(t.into())
            }
        }
    )*}
}

macro_rules! gen_texture_conversions {
    ( $([$internal_format:tt $type:tt])* ) => {$(
        impl From<Uniform<Texture<TextureData<$internal_format,$type>>>> for AnyUniform {
            fn from(t:Uniform<Texture<TextureData<$internal_format,$type>>>) -> Self {
                Self::Texture(t.into())
            }
        }
    )*}
}

crate::with_all_texture_types!(gen_any_texture_uniform);


#[enum_dispatch]
pub trait AnyTextureUniformOps {
    fn bind_texture_unit(&self, context:&Context, unit:u32) -> TextureBindGuard;
}



// ==================
// === AnyUniform ===
// ==================

#[derive(Clone,Debug)]
pub enum AnyUniform {
    Prim(AnyPrimUniform),
    Texture(AnyTextureUniform)
}

crate::with_all_prim_types!([[gen_prim_conversions][]]);
crate::with_all_texture_types!(gen_texture_conversions);
