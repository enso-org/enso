//! Zero-sized type representation. This module is similar to the `zst` crate with the following
//! differences:
//! - All impls are derived manually to not include the type parameter in bounds.
//! - It implements the [`Zeroable`] trait.
//! - It implements the [`serde::Serialize`] and [`serde::Deserialize`] traits.
//! - Its internal `PhantomData` field is defined as public. Otherwise, the compiler does not allow
//!   its usage in `repr(transparent)` types because there is no guarantee that in the future the
//!   private fields might be changed and become non-zero-sized.

use crate::Zeroable;
use core::fmt::Debug;
use core::marker::PhantomData;



// ===========
// === ZST ===
// ===========

mod tp {
    use super::*;

    /// Zero-sized type representation. Please note that it is better to use this type instead of
    /// `PhantomData<T>` assumes that the type `T` is owned, and requires the drop checker to fire.
    /// To learn more, see: https://doc.rust-lang.org/nomicon/phantom-data.html
    #[repr(align(1))]
    pub struct ZST<T: ?Sized>(pub PhantomData<*const T>);

    impl<T: ?Sized> ZST<T> {
        #[inline(always)]
        pub(crate) const fn new() -> ZST<T> {
            ZST(PhantomData)
        }
    }
}
pub use tp::*;

/// ZST constructor.
#[allow(non_snake_case)]
#[inline(always)]
pub const fn ZST<T: ?Sized>() -> ZST<T> {
    ZST::new()
}

#[allow(unsafe_code)]
unsafe impl<T: ?Sized> Zeroable for ZST<T> {}

impl<T: ?Sized> Debug for ZST<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "ZST")
    }
}

impl<T: ?Sized> Default for ZST<T> {
    #[inline(always)]
    fn default() -> Self {
        Self::new()
    }
}

impl<T: ?Sized> Copy for ZST<T> {}
impl<T: ?Sized> Clone for ZST<T> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self::new()
    }
}

impl<T: ?Sized> Eq for ZST<T> {}
impl<T: ?Sized> PartialEq for ZST<T> {
    #[inline(always)]
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl<T: ?Sized> Ord for ZST<T> {
    #[inline(always)]
    fn cmp(&self, _: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
    }
}

impl<T: ?Sized> PartialOrd for ZST<T> {
    #[inline(always)]
    fn partial_cmp(&self, _: &Self) -> Option<std::cmp::Ordering> {
        Some(std::cmp::Ordering::Equal)
    }
}

// === serde::Serialize ===

impl<T: ?Sized> serde::Serialize for ZST<T> {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: serde::Serializer {
        serializer.serialize_unit_struct("ZST")
    }
}


// === serde::Deserialize ===

struct ZSTDataVisitor<T: ?Sized> {
    _marker: ZST<T>,
}

impl<'de, T: ?Sized> serde::de::Visitor<'de> for ZSTDataVisitor<T> {
    type Value = ZST<T>;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("unit")
    }

    #[inline]
    fn visit_unit<E>(self) -> Result<Self::Value, E>
    where E: serde::de::Error {
        Ok(ZST())
    }
}


impl<'de, T: ?Sized> serde::Deserialize<'de> for ZST<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where D: serde::Deserializer<'de> {
        let visitor = ZSTDataVisitor { _marker: ZST() };
        deserializer.deserialize_unit_struct("ZST", visitor)
    }
}
