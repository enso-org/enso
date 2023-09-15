//! Zero-sized type representation. This module is similar to the `zst` crate with the following
//! differences:
//! - All impls are derived manually to not include the type parameter in bounds.
//! - It implements the [`Zeroable`], [`serde::Serialize`], and [`serde::Deserialize`].
//! - Its internal `PhantomData` field is defined as public. Otherwise, the compiler does not allow
//!   its usage in `repr(transparent)` types because there is no guarantee that in the future the
//!   private fields might be changed and become non-zero-sized.
//! - Multiple ZST types are defined ([`ZST1`], [`ZST2`], etc) for different parameter count. This
//!   allows the type parameters to be `?Sized`.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use bytemuck::Pod;
use bytemuck::Zeroable;
use core::fmt::Debug;
use core::marker::PhantomData;
use paste::paste;



// ===========
// === ZST ===
// ===========

/// Defines zero-sized types, such as `ZST1<T1>`, `ZST2<T1, T2>`, etc. Separate definitions for
/// different parameter count allow the type parameters to be `?Sized`. Unfortunately, we can't use
/// tuples here, as tuples require their arguments to be `Sized`. In case your arguments are not,
/// using `ZST2<T1, T2>` is better than using `ZST<(T1, T2)>`.
macro_rules! define_zst {
    ($($ns:literal),*) => {
        define_zst! { @ [] $($ns)* }
    };
    (@ [] $n:literal $($ns:literal)*) => {
        define_zst! { @ [ [$n []] ] $($ns)* }
    };
    (@ [[$t:tt [$($prev:tt $($ts:tt)*)?]] $($ss:tt)*] $n:tt $($ns:tt)*) => {
        define_zst! { @ [ [$n [$t $($($ts)*)? $n]] [$t [$($prev $($ts)*)?]] $($ss)* ] $($ns)* }
    };
    (@ [$($ts:tt)*]) => {
        define_zst! { # $($ts)* }
    };
    (# $([$n:tt [ $($prev:tt $($ts:tt)*)? ]])*) => { paste! {
        define_zst! { ## $([
            [<ZST $n>]
            [T $($(,[<T $ts>])*)?]
            [T:?Sized $($(,[<T $ts>]:?Sized)*)?]
            $([<ZST $prev>] [$([<T $ts>]),*])?
        ])*}
    }};
    (## $([
        $name:tt [$($params:tt)*] [$($bounds:tt)*] $($prev_name:tt [$($prev_params:tt)*])?
    ])*) => { paste! {
        mod tp {
            use super::*;

            $(
                /// Zero-sized type representation. Read docs of [`ZST`] to learn more.
                #[repr(align(1))]
                pub struct $name < $($bounds)* >
                    (pub PhantomData<*const T>, $(pub $prev_name <$($prev_params)*> )? );
            )*
        }

        $(
            /// ZST constructor.
            #[allow(non_snake_case)]
            #[inline(always)]
            pub const fn $name<$($bounds)*>()
            -> $name<$($params)*> {
                tp::$name(PhantomData, $($prev_name())?)
            }

            #[allow(unsafe_code)]
            unsafe impl <$($bounds)*> Zeroable for $name<$($params)*> {}
            #[allow(unsafe_code)]
            unsafe impl <$($bounds)*> Pod for $name<$($params)*> where Self: 'static {}

            impl<$($bounds)*> Debug for $name<$($params)*> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
                    write!(f, stringify!($name))
                }
            }

            impl<$($bounds)*> Default for $name<$($params)*> {
                #[inline(always)]
                fn default() -> Self {
                    $name()
                }
            }

            impl<$($bounds)*> Copy for $name<$($params)*> {}
            impl<$($bounds)*> Clone for $name<$($params)*> {
                #[inline(always)]
                fn clone(&self) -> Self {
                    $name()
                }
            }

            impl<$($bounds)*> Eq for $name<$($params)*> {}
            impl<$($bounds)*> PartialEq for $name<$($params)*> {
                #[inline(always)]
                fn eq(&self, _: &Self) -> bool {
                    true
                }
            }

            impl<$($bounds)*> Ord for $name<$($params)*> {
                #[inline(always)]
                fn cmp(&self, _: &Self) -> std::cmp::Ordering {
                    std::cmp::Ordering::Equal
                }
            }

            impl<$($bounds)*> PartialOrd for $name<$($params)*> {
                #[inline(always)]
                fn partial_cmp(&self, _: &Self) -> Option<std::cmp::Ordering> {
                    Some(std::cmp::Ordering::Equal)
                }
            }

            impl<$($bounds)*> std::hash::Hash for $name<$($params)*> {
                fn hash<H: std::hash::Hasher>(&self, _: &mut H) {}
            }

            // === serde::Serialize ===

            impl<$($bounds)*> serde::Serialize for $name<$($params)*> {
                #[inline]
                fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
                where S: serde::Serializer {
                    serializer.serialize_unit_struct(stringify!($name))
                }
            }

            // === serde::Deserialize ===

            struct [<$name DataVisitor>]<$($bounds)*> {
                _marker: $name<$($params)*>,
            }

            impl<'de, $($bounds)*> serde::de::Visitor<'de> for [<$name DataVisitor>]<$($params)*> {
                type Value = $name<$($params)*>;

                fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                    formatter.write_str("unit")
                }

                #[inline]
                fn visit_unit<E>(self) -> Result<Self::Value, E>
                where E: serde::de::Error {
                    Ok($name())
                }
            }

            impl<'de, $($bounds)*> serde::Deserialize<'de> for $name<$($params)*> {
                fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
                where D: serde::Deserializer<'de> {
                    let visitor = [<$name DataVisitor>] { _marker: $name() };
                    deserializer.deserialize_unit_struct("ZST", visitor)
                }
            }

        )*
        pub use tp::*;
    }};
}
define_zst![1, 2, 3, 4, 5, 6, 7, 8];


// === Alias for ZST1 ===

/// Zero-sized type representation. Please note that it is better to use this type instead of
/// `PhantomData<T>` assumes that the type `T` is owned, and requires the drop checker to fire.
///
/// To learn more, see: https://doc.rust-lang.org/nomicon/phantom-data.html
pub type ZST<T> = ZST1<T>;

/// ZST constructor.
#[allow(non_snake_case)]
#[inline(always)]
pub const fn ZST<T: ?Sized>() -> ZST<T> {
    ZST1()
}
