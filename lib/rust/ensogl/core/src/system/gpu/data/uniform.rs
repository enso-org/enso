// === Non-Standard Linter Configuration ===
#![allow(missing_docs)]

use crate::prelude::*;
use crate::system::gpu::data::prim::*;
use crate::system::gpu::data::texture::*;
use enum_dispatch::*;

use crate::system::gpu::Context;

use upload::UniformUpload;
use web_sys::WebGlUniformLocation;


// ==============
// === Export ===
// ==============

pub mod upload;



// ====================
// === UniformValue ===
// ====================

/// Describes every value which can be stored inside of an uniform.
pub trait UniformValue = Sized where Uniform<Self>: Into<AnyUniform>;



// ====================
// === UniformScope ===
// ====================

/// A scope containing set of uniform values.
#[derive(Debug, Default)]
pub struct UniformScope {
    map: HashMap<String, AnyUniform>,
}

impl UniformScope {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Look up uniform by name.
    pub fn get<Name: Str>(&self, name: Name) -> Option<AnyUniform> {
        self.map.get(name.as_ref()).cloned()
    }

    /// Checks if uniform of a given name was defined in this scope.
    pub fn contains<Name: Str>(&self, name: Name) -> bool {
        self.map.contains_key(name.as_ref())
    }

    /// Add a new uniform with a given name and initial value. Returns `None` if the name is in use.
    pub fn add<Name, Value, Input>(&mut self, name: Name, input: Input) -> Option<Uniform<Value>>
    where
        Name: Str,
        Input: Into<Uniform<Value>>,
        Value: UniformValue, {
        self.add_or_else(name, input, Some, |_, _, _| None)
    }

    /// Add a new uniform with a given name and initial value. Panics if the name is in use.
    pub fn add_or_panic<Name, Value, Input>(&mut self, name: Name, input: Input) -> Uniform<Value>
    where
        Name: Str,
        Input: Into<Uniform<Value>>,
        Value: UniformValue, {
        self.add_or_else(
            name,
            input,
            |t| t,
            |name, _, _| panic!("Trying to override uniform '{}'.", name.as_ref()),
        )
    }

    pub fn add_uniform<Name, Value>(&mut self, name: Name, uniform: &Uniform<Value>)
    where
        Name: Str,
        Value: UniformValue, {
        self.add::<Name, Value, _>(name, uniform);
    }

    pub fn add_uniform_or_panic<Name, Value>(&mut self, name: Name, uniform: &Uniform<Value>)
    where
        Name: Str,
        Value: UniformValue, {
        self.add_or_panic::<Name, Value, _>(name, uniform);
    }

    /// Adds a new uniform with a given name and initial value. In case the name was already in use,
    /// it fires the `on_exist` function. Otherwise, it fires the `on_fresh` function on the newly
    /// created uniform.
    pub fn add_or_else<Name, Value, OnFresh, OnExist, Input, T>(
        &mut self,
        name: Name,
        input: Input,
        on_fresh: OnFresh,
        on_exist: OnExist,
    ) -> T
    where
        Name: Str,
        Input: Into<Uniform<Value>>,
        Value: UniformValue,
        OnFresh: FnOnce(Uniform<Value>) -> T,
        OnExist: FnOnce(Name, Input, &AnyUniform) -> T,
    {
        match self.map.get(name.as_ref()) {
            Some(v) => on_exist(name, input, v),
            None => {
                let uniform = input.into();
                let any_uniform = uniform.clone().into();
                self.map.insert(name.into(), any_uniform);
                on_fresh(uniform)
            }
        }
    }

    /// Gets an existing uniform or adds a new one in case it was missing. Returns `None` if the
    /// uniform exists but its type does not match the requested one.
    pub fn set<Name, Value>(&mut self, name: Name, value: Value) -> Option<Uniform<Value>>
    where
        Name: Str,
        Value: UniformValue,
        for<'t> &'t Uniform<Value>: TryFrom<&'t AnyUniform>, {
        self.add_or_else(name, value, Some, move |_, value, uniform| {
            let out: Option<&Uniform<Value>> = uniform.try_into().ok();
            let out = out.cloned();
            if let Some(t) = &out {
                t.set(value)
            }
            out
        })
    }
}



// ===============
// === Uniform ===
// ===============

// TODO: Finish the dirty implementation. After uniform is changed it should be marked dirty. After
//       each frame, all uniforms should be marked as non-dirty. When rendering an object, it should
//       check for dirty unforms in its bindigs. There are few interesting cases - when object is
//       hidden and is shown, its obsolete uniforms need to be updated. This may be realized by
//       using an int or frame number instead of bool to mark dirty state and checking if the
//       object uniform was uploaded with the newest version.
//
//       Please note that currently a special uniform 'zoom' is modified in the render loop. See
//       the `scene::View` implementation to learn more.

/// An uniform value.
#[derive(Debug, CloneRef, Derivative)]
#[derivative(Clone(bound = ""))]
pub struct Uniform<Value> {
    value: Rc<RefCell<Value>>,
}

impl<Value> Uniform<Value> {
    /// Constructor.
    pub fn new(value: Value) -> Self {
        let value = Rc::new(RefCell::new(value));
        // let dirty = true;
        Self { value }
    }

    /// Sets the value of this uniform.
    pub fn set(&self, value: Value) {
        // self.set_dirty();
        *self.value.borrow_mut() = value;
    }

    /// Modifies the value of this uniform.
    pub fn modify(&self, f: impl FnOnce(&mut Value)) {
        f(&mut *self.value.borrow_mut());
    }
}

impl<Value: Clone> Uniform<Value> {
    /// Reads the value of this uniform.
    pub fn get(&self) -> Value {
        self.value.borrow().clone()
    }
}

impl<Value> Uniform<Value> {
    pub fn swap(&self, that: &Self) {
        if !Rc::ptr_eq(&self.value, &that.value) {
            mem::swap(&mut *self.value.borrow_mut(), &mut *that.value.borrow_mut())
        }
    }
}

impl<T> From<T> for Uniform<T> {
    fn from(t: T) -> Self {
        Self::new(t)
    }
}



// ========================
// === Texture Uniforms ===
// ========================

impl<T> HasItem for Uniform<T> {
    type Item = T;
}

impl<T> WithItemRef for Uniform<T> {
    fn with_item<R>(&self, f: impl FnOnce(&Self::Item) -> R) -> R {
        f(&self.value.borrow())
    }
}



// ======================
// === AnyPrimUniform ===
// ======================

#[derive(Clone, Copy, Debug)]
pub struct TypeMismatch;

macro_rules! define_any_prim_uniform {
    ( [] [$([$t1:ident $t2:ident])*] ) => { paste! {
        /// Existentially typed uniform value.
        #[allow(non_camel_case_types)]
        #[enum_dispatch(AnyPrimUniformOps)]
        #[derive(Clone,Debug)]
        pub enum AnyPrimUniform {
            $([<Variant_ $t1 _ $t2>](Uniform<$t1<$t2>>)),*
        }

        $(impl<'t> TryFrom<&'t AnyPrimUniform> for &'t Uniform<$t1<$t2>> {
            type Error = TypeMismatch;
            fn try_from(value:&'t AnyPrimUniform) -> Result<Self,Self::Error> {
                match value {
                    AnyPrimUniform::[<Variant_ $t1 _ $t2>](t) => Ok(t),
                    _ => Err(TypeMismatch)
                }
            }
        })*
    }}
}
crate::with_all_prim_types!([[define_any_prim_uniform][]]);

/// Set of operations exposed by the `AnyPrimUniform` value.
#[enum_dispatch]
pub trait AnyPrimUniformOps {
    /// Uploads the uniform data to the provided location of the currently bound shader program.
    fn upload(&self, context: &Context, location: &WebGlUniformLocation);
}

impl<Value: UniformUpload> AnyPrimUniformOps for Uniform<Value> {
    fn upload(&self, context: &Context, location: &WebGlUniformLocation) {
        self.value.borrow().upload_uniform(context, location)
    }
}



// =========================
// === AnyTextureUniform ===
// =========================

#[allow(non_camel_case_types)]
#[derive(Clone, CloneRef, Debug)]
pub struct AnyTextureUniform {
    texture: Uniform<Option<Texture>>,
}

impl AnyTextureUniform {
    pub fn texture(&self) -> Option<Ref<Texture>> {
        Ref::filter_map(self.texture.value.borrow(), |texture| texture.as_ref()).ok()
    }
}

impl From<Uniform<Option<Texture>>> for AnyTextureUniform {
    fn from(texture: Uniform<Option<Texture>>) -> Self {
        Self { texture }
    }
}

impl<'t> TryFrom<&'t AnyTextureUniform> for &'t Uniform<Option<Texture>> {
    type Error = TypeMismatch;
    fn try_from(value: &'t AnyTextureUniform) -> Result<Self, Self::Error> {
        Ok(&value.texture)
    }
}



// ==================
// === AnyUniform ===
// ==================

#[derive(Clone, Debug)]
pub enum AnyUniform {
    Prim(AnyPrimUniform),
    Texture(AnyTextureUniform),
}


// === Conversions ===

impl<T> From<Uniform<T>> for AnyUniform
where Uniform<T>: IntoAnyUniform
{
    fn from(t: Uniform<T>) -> Self {
        t.into_any_uniform()
    }
}

pub trait IntoAnyUniform: Sized {
    fn into_any_uniform(self) -> AnyUniform;
}

impl<T: Into<AnyPrimUniform>> IntoAnyUniform for T {
    default fn into_any_uniform(self) -> AnyUniform {
        AnyUniform::Prim(self.into())
    }
}

impl IntoAnyUniform for Uniform<Option<Texture>> {
    fn into_any_uniform(self) -> AnyUniform {
        AnyUniform::Texture(self.into())
    }
}

macro_rules! generate_prim_type_downcasts {
    ( [] [$([$t1:ident $t2:ident])*] ) => {
        $(impl<'t> TryFrom<&'t AnyUniform> for &'t Uniform<$t1<$t2>> {
            type Error = TypeMismatch;
            fn try_from(value:&'t AnyUniform) -> Result<Self,Self::Error> {
                match value {
                    AnyUniform::Prim(t) => t.try_into(),
                    _ => Err(TypeMismatch)
                }
            }
        })*
    }
}
crate::with_all_prim_types!([[generate_prim_type_downcasts][]]);


impl<'t> TryFrom<&'t AnyUniform> for &'t Uniform<Option<Texture>> {
    type Error = TypeMismatch;
    fn try_from(value: &'t AnyUniform) -> Result<Self, Self::Error> {
        match value {
            AnyUniform::Texture(t) => t.try_into(),
            _ => Err(TypeMismatch),
        }
    }
}
