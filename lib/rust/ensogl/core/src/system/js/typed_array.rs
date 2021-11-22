//! Implements safe Rust-style bindings to JavaScript typed arrays.

use crate::prelude::*;

use std::fmt::Error;
use std::fmt::Formatter;



// =======================
// === JsTypedArrayOps ===
// =======================

/// Abstraction for every type which can be an item of typed array in JavaScript.
pub trait JsTypedArrayItem: Debug + Clone + Sized + 'static {
    /// Internal JavaScript array type.
    type JsArrayType: JsTypedArrayOps<Item = Self>;
}

/// Abstraction for all operations on raw JavaScript typed arrays.
pub trait JsTypedArrayOps: Debug + Clone {
    /// Rust item type for a given JavaScript array type.
    type Item;
    /// Constructs a new array with a given length.
    fn new_with_length(len: u32) -> Self;
    /// Returns a JavaScripts `ArrayBuffer` view for this array.
    fn buffer(&self) -> js_sys::ArrayBuffer;
    /// Converts the array to a vector.
    fn to_vec(&self) -> Vec<Self::Item>;
    /// Views the array as JavaScript `Object`.
    fn to_object(&self) -> &js_sys::Object;
}

macro_rules! define_js_typed_array_bindings {
    ($($name:ident($type:ident)),* $(,)?) => {$(

        impl JsTypedArrayItem for $type {
            type JsArrayType = js_sys::$name;
        }

        impl JsTypedArrayOps for js_sys::$name {
            type Item = $type;

            fn new_with_length(len:u32) -> Self {
                js_sys::$name::new_with_length(len)
            }

            fn buffer(&self) -> js_sys::ArrayBuffer {
                js_sys::$name::buffer(self)
            }

            fn to_vec(&self) -> Vec<Self::Item> {
                js_sys::$name::to_vec(self)
            }

            fn to_object(&self) -> &js_sys::Object {
                self
            }
        }

    )*}
}

define_js_typed_array_bindings! {
    Float32Array (f32),
    Int8Array    (i8),
    Int16Array   (i16),
    Int32Array   (i32),
    Uint8Array   (u8),
    Uint16Array  (u16),
    Uint32Array  (u32),
}



// ====================
// === JsTypedArray ===
// ====================

/// Rust-style wrapper for typed array living in JavaScript scope.
pub struct JsTypedArray<T: JsTypedArrayItem> {
    raw: <T as JsTypedArrayItem>::JsArrayType,
}


// === API ===

impl<T: JsTypedArrayItem> JsTypedArray<T> {
    /// Constructor.
    pub fn new_with_length(len: u32) -> Self {
        let raw = <T as JsTypedArrayItem>::JsArrayType::new_with_length(len);
        Self { raw }
    }

    /// Accessor of the underlying raw `JsArrayType`.
    pub fn raw(&self) -> &<T as JsTypedArrayItem>::JsArrayType {
        &self.raw
    }
}


// === Instances ===

impl<T: JsTypedArrayItem> Clone for JsTypedArray<T> {
    fn clone(&self) -> Self {
        let raw = self.raw.clone();
        Self { raw }
    }
}

impl<T: JsTypedArrayItem> Debug for JsTypedArray<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        Debug::fmt(&self.raw, f)
    }
}

impl<T: JsTypedArrayItem> Deref for JsTypedArray<T> {
    type Target = <T as JsTypedArrayItem>::JsArrayType;
    fn deref(&self) -> &Self::Target {
        &self.raw
    }
}
