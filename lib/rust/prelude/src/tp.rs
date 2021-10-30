//! Type related utilities.

use super::std_reexports::*;


// ================
// === Anything ===
// ================

/// Placeholder type used to represent any value type. It is useful to define type-level relations
/// like defining an unit with any quantity, let it be distance or mass.
#[derive(Clone,Copy,Debug,PartialEq)]
pub struct Anything {}


// ===================
// === TypeDisplay ===
// ===================

/// Like `Display` trait but for types. However, unlike `Display` it defaults to
/// `impl::any::type_name` if not provided with explicit implementation.
pub trait TypeDisplay {
    fn type_display() -> String;
}

impl<T> TypeDisplay for T {
    default fn type_display() -> String {
        type_name::<Self>().to_string()
    }
}

/// Formats the type for the user-facing output.
pub fn type_display<T:TypeDisplay>() -> String {
    <T as TypeDisplay>::type_display()
}


// =============
// === Value ===
// =============

/// Defines relation between types and values, like between `True` and `true`.
pub trait KnownTypeValue {

    /// The value-level counterpart of this type-value.
    type Value;

    /// The value of this type-value.
    fn value() -> Self::Value;
}

pub type TypeValue<T> = <T as KnownTypeValue>::Value;



// =======================
// === Type-level Bool ===
// =======================

/// Type level `true` value.
#[derive(Clone,Copy,Debug)]
pub struct True {}

/// Type level `false` value.
#[derive(Clone,Copy,Debug)]
pub struct False {}

impl KnownTypeValue for True {
    type Value = bool;
    fn value() -> Self::Value {
        true
    }
}

impl KnownTypeValue for False {
    type Value = bool;
    fn value() -> Self::Value {
        false
    }
}



// ========================
// === Type Conversions ===
// ========================

/// This is used to make type inference better at use sites - without it,
/// Rust would force one to write the `where` clause at every use site.
pub trait IntoSelfFrom<T> = Sized where T:Into<Self>;

/// Can be transformed from and into.
pub trait BiInto<T> = Sized + Into<T> + IntoSelfFrom<T>;
