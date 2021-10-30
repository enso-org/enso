// README README README README README README README README README README README 
// README README README README README README README README README README README 
// README README README README README README README README README README README 

// This library is in a very early stage. It will be refactored and improved 
// soon. It should not be reviewed now.

#![warn(unsafe_code)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![feature(generators, generator_trait)]
#![feature(type_ascription)]
#![feature(marker_trait_attr)]

pub mod generator;
pub mod shared;
pub mod singleton;
pub mod cartesian;

pub use enso_shapely_macros::*;
pub use generator::GeneratingIterator;

/// Replaces the first argument with the second one. It is useful when creating macros which match
/// a pattern and you want to generate as many repetitions of a token as there was matches. For
/// example, when matching `$($name:ident)*`, you may want to generate as many empty tuples as
/// the number of names matched. You can do it by using `$(replace!{$name,()})*`.
#[macro_export]
macro_rules! replace {
    ($a:tt,$($b:tt)*) => {$($b)*}
}

/// The same as [`newtype_prim`] but does not generate derive clauses.
#[macro_export]
macro_rules! newtype_prim_no_derives {
    ($( $(#$meta:tt)* $name:ident($type:ty); )*) => {$(
        $(#$meta)*
        pub struct $name {
            raw:$type
        }

        impl $name {
            /// Constructor.
            pub const fn new(raw:$type) -> Self {
                Self {raw}
            }
        }

        impl Deref for $name {
            type Target = $type;
            fn deref(&self) -> &Self::Target {
                &self.raw
            }
        }

        impl DerefMut for $name {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.raw
            }
        }

        impl From<$type>   for $name { fn from(t:$type)   -> Self { Self::new(t)   } }
        impl From<&$type>  for $name { fn from(t:&$type)  -> Self { Self::new(*t)  } }
        impl From<&&$type> for $name { fn from(t:&&$type) -> Self { Self::new(**t) } }

        impl From<$name>   for $type { fn from(t:$name)   -> Self { t.raw } }
        impl From<&$name>  for $type { fn from(t:&$name)  -> Self { t.raw } }
        impl From<&&$name> for $type { fn from(t:&&$name) -> Self { t.raw } }

        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.debug_tuple(stringify!($name))
                    .field(&self.raw)
                    .finish()
            }
        }
    )*}
}

/// Generates a newtype wrapper for the provided types. It also generates a lot of impls,
/// including Copy, Clone, Debug, Default, Display, From, Into, Deref, and DerefMut.
///
/// For the following input:
/// ```ignore
/// newtype_prim! {
///     AttributeIndex(usize);
/// }
/// ```
///
/// The following code is generated:
/// ```ignore
/// #[derive(Copy,Clone,CloneRef,Debug,Default,Display,Eq,Hash,Ord,PartialOrd,PartialEq)]
/// pub struct AttributeIndex {
///     raw: usize
/// }
/// impl AttributeIndex {
///     /// Constructor.
///     pub fn new(raw:usize) -> Self {
///         Self { raw }
///     }
/// }
/// impl Deref for AttributeIndex {
///     type Target = usize;
///     fn deref(&self) -> &Self::Target {
///         &self.raw
///     }
/// }
/// impl DerefMut for AttributeIndex {
///     fn deref_mut(&mut self) -> &mut Self::Target {
///         &mut self.raw
///     }
/// }
/// impl From<usize>   for AttributeIndex { fn from(t:usize)   -> Self { Self::new(t)   } }
/// impl From<&usize>  for AttributeIndex { fn from(t:&usize)  -> Self { Self::new(*t)  } }
/// impl From<&&usize> for AttributeIndex { fn from(t:&&usize) -> Self { Self::new(**t) } }
/// impl From<AttributeIndex>   for usize { fn from(t:AttributeIndex)   -> Self { t.raw } }
/// impl From<&AttributeIndex>  for usize { fn from(t:&AttributeIndex)  -> Self { t.raw } }
/// impl From<&&AttributeIndex> for usize { fn from(t:&&AttributeIndex) -> Self { t.raw } }
/// ```
#[macro_export]
macro_rules! newtype_prim {
    ($( $(#$meta:tt)* $name:ident($type:ty); )*) => {
        $crate::newtype_prim_no_derives! {
            $(
                $(#$meta)*
                #[derive(Copy,Clone,CloneRef,Default,Display,Eq,Hash,Ord,PartialOrd,PartialEq)]
                $name($type);
            )*
        }
    }
}

/// The same as [`newtype_prim`] but does not generate [`Default`] derive clause.
#[macro_export]
macro_rules! newtype_prim_no_default {
    ($( $(#$meta:tt)* $name:ident($type:ty); )*) => {
        $crate::newtype_prim_no_derives! {
            $(
                $(#$meta)*
                #[derive(Copy,Clone,CloneRef,Display,Eq,Hash,Ord,PartialOrd,PartialEq)]
                $name($type);
            )*
        }
    }
}

/// The same as [`newtype_prim`] but does not generate [`Default`] and [`Display`] derive clauses.
#[macro_export]
macro_rules! newtype_prim_no_default_no_display {
    ($( $(#$meta:tt)* $name:ident($type:ty); )*) => {
        $crate::newtype_prim_no_derives! {
            $(
                $(#$meta)*
                #[derive(Copy,Clone,CloneRef,Eq,Hash,Ord,PartialOrd,PartialEq)]
                $name($type);
            )*
        }
    }
}

#[macro_export]
macro_rules! derive_clone_plus {
    ($name:ident) => {
        impl<T:Clone+Into<$name>> From<&T> for $name {
            fn from(t: &T) -> Self {
                t.clone().into()
            }
        }
    }
}
