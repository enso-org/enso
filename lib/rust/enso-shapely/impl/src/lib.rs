// README README README README README README README README README README README 
// README README README README README README README README README README README 
// README README README README README README README README README README README 

// This library is in a very early stage. It will be refactored and improved 
// soon. It should not be reviewed now.

#![warn(unsafe_code)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![feature(generators, generator_trait)]
#![feature(specialization)]
#![feature(type_ascription)]
#![feature(overlapping_marker_traits)]

pub mod generator;
pub mod shared;
pub mod singleton;
pub mod cartesian;

pub use enso_shapely_macros::*;

pub use generator::EmptyIterator;
pub use generator::GeneratingIterator;

use shrinkwraprs::Shrinkwrap;


/// Replaces the first argument with the second one. It is useful when creating macros which match
/// a pattern and you want to generate as many repetitions of a token as there was matches. For
/// example, when matching `$($name:ident)*`, you may want to generate as many empty tuples as
/// the number of names matched. You can do it by using `$(replace!{$name,()})*`.
#[macro_export]
macro_rules! replace {
    ($a:tt,$b:tt) => {$b}
}

/// Generates a newtype wrapper for the provided types. It also generates a lot of impls,
/// including Copy, Clone, Debug, Default, Display, From, Into, Deref, and DerefMut.
///
/// For the following input:
/// ```compile_fail
/// newtype_copy! {
///     AttributeIndex(usize)
/// }
/// ```
///
/// The following code is generated:
/// ```compile_fail
/// #[derive(Copy, Clone, Debug, Default, Display, From, Into)]
/// pub struct AttributeIndex(usize);
/// impl Deref for AttributeIndex {
///     type Target = usize;
///     fn deref(&self) -> &Self::Target {
///         &self.0
///     }
/// }
/// impl DerefMut for AttributeIndex {
///     fn deref_mut(&mut self) -> &mut Self::Target {
///         &mut self.0
///     }
/// }
/// ```
#[macro_export]
macro_rules! newtype_copy {
    ($( $(#$meta:tt)* $name:ident($type:ty); )*) => {$(
        $(#$meta)*
        #[derive(Copy,Clone,CloneRef,Debug,Default,Display,From,Into)]
        pub struct $name($type);

        impl Deref for $name {
            type Target = $type;
            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl DerefMut for $name {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.0
            }
        }
    )*}
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

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

use std::ops::Deref;

macro_rules! extension_struct {
    ($name:ident { $($field:ident : $field_type:ty),* }) => { paste::item! {

        ////// With_NAME_ //////

        #[derive(Shrinkwrap)]
        #[shrinkwrap(mutable)]
        struct [<With $name>]<T>($($field_type),*, #[shrinkwrap(main_field)] T);

        ////// Has_NAME_ //////

        pub trait [<Has $name>] {
            $(fn $field(&self) -> &$field_type;)*
        }

        impl<T: [<Has $name Indirect>]>
        [<Has $name>] for T {
            $(fn $field(&self) -> &$field_type {
                [<Has $name Spec1>]::$field(self)
            })*
        }

        ////// Has_NAME_Indirect //////

        pub trait [<Has $name Indirect>] {}

        impl<T>
        [<Has $name Indirect>] for [<With $name>]<T> {}

        impl<T>
        [<Has $name Indirect>] for T
        where T: Deref, <Self as Deref>::Target : [<Has $name>] {}

        ////// Has_NAME_Spec1 //////

        trait [<Has $name Spec1>] {
            $(fn $field(&self) -> &$field_type;)*
        }

        impl<T>
        [<Has $name Spec1>] for [<With $name>]<T> {
            $(fn $field(&self) -> &$field_type {
                &self.0
            })*
        }

        impl<T: [<Has $name Indirect>]>
        [<Has $name Spec1>] for T {
            $(default fn $field(&self) -> &$field_type {
                [<Has $name Spec2>]::$field(self)
            })*
        }

        ////// Has_NAME_Spec2 //////

        trait [<Has $name Spec2>] {
            $(fn $field(&self) -> &$field_type;)*
        }

        impl<T: [<Has $name Indirect>]>
        [<Has $name Spec2>] for T {
            $(default fn $field(&self) -> &$field_type {
                unreachable!();
            })*
        }

        impl<T>
        [<Has $name Spec2>] for T
        where T: Deref, <Self as Deref>::Target : [<Has $name>] {
            $(fn $field(&self) -> &$field_type {
                self.deref().$field()
            })*
        }
    }};
}


extension_struct!(Label {
    label: String
});

extension_struct!(Foo {
    t1: String
});



// ==============
// === WithID ===
// ==============

struct WithID<T>(i32, T);

impl<T> Deref for WithID<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

struct WithID2<T>(i32, T);

impl<T> Deref for WithID2<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

//// === HasID ===
//
//pub trait HasID {
//    fn id(&self) -> &i32;
//}
//
//impl<T: MarkerCtxForHasID> HasID for T {
//    fn id(&self) -> &i32 {
//        HasIDForVariantOrAny::id(self)
//    }
//}
//
//// === MarkerCtxForHasID ===
//
//pub trait MarkerCtxForHasID {}
//
//impl<T> MarkerCtxForHasID for WithID<T> {}
//
//impl<T> MarkerCtxForHasID for T
//where T: Deref, <T as Deref>::Target : HasID {}
//
//
//// === HasIDForVariantOrAny ===
//
//trait HasIDForVariantOrAny {
//    fn id(&self) -> &i32;
//}
//impl<T> HasIDForVariantOrAny for WithID<T> {
//    fn id(&self) -> &i32 {
//        &self.0
//    }
//}
//impl<T: MarkerCtxForHasID> HasIDForVariantOrAny for T {
//    default fn id(&self) -> &i32 {
//        HasIDForDerefOrAny::id(self)
//    }
//}
//
//// === HasIDForDerefOrAny ===
//
//trait HasIDForDerefOrAny {
//    fn id(&self) -> &i32;
//}
//impl<T> HasIDForDerefOrAny for T
//where T: Deref, <Self as Deref>::Target : HasID {
//    fn id(&self) -> &i32 {
//        self.deref().id()
//    }
//}
//impl<T> HasIDForDerefOrAny for T {
//    default fn id(&self) -> &i32 {
//        unreachable!();
//    }
//}





// === HasID ===

pub trait HasID {
    fn id(&self) -> &i32;
}


//////////////////////////////////

#[overlappable]
impl<T> HasID for T
    where T: Deref, <Self as Deref>::Target : HasID {
    fn id(&self) -> &i32 {
        self.deref().id()
    }
}

impl<T: MarkerCtx_HasID> HasID for T {
    fn id(&self) -> &i32 {
        VariantOrAny_HasID::id(self)
    }
}

// === MarkerCtx_HasID ===

#[allow(non_camel_case_types)]
pub trait MarkerCtx_HasID {}

impl<T> MarkerCtx_HasID for T
    where T: Deref, <T as Deref>::Target : HasID {}

// === VariantOrAny_HasID ===

#[allow(non_camel_case_types)]
trait VariantOrAny_HasID {
    fn id(&self) -> &i32;
}

impl<T: MarkerCtx_HasID> VariantOrAny_HasID for T {
    default fn id(&self) -> &i32 {
        DerefOrAny_HasID::id(self)
    }
}

// === DerefOrAny_HasID ===

#[allow(non_camel_case_types)]
trait DerefOrAny_HasID {
    fn id(&self) -> &i32;
}
impl<T> DerefOrAny_HasID for T
    where T: Deref, <Self as Deref>::Target : HasID {
    fn id(&self) -> &i32 {
        self.deref().id()
    }
}
impl<T> DerefOrAny_HasID for T {
    default fn id(&self) -> &i32 {
        unreachable!();
    }
}


/////////////////////////////////////////////


//#[overlapping]
//impl<T> HasID for WithID<T> {
//    fn id(&self) -> &i32 {
//        &self.0
//    }
//}

impl<T> MarkerCtx_HasID for WithID<T> {}

impl<T> VariantOrAny_HasID for WithID<T> {
    fn id(&self) -> &i32 {
        &self.0
    }
}

//// NON-CONFLICTING:
//
//trait HasFoo2 {
//    fn foo(&self) -> i32;
//}
//impl<T> HasFoo2 for T {
//    default fn foo(&self) -> i32 {
//        7
//    }
//}
//impl<T> HasFoo2 for WithID<T> {
//    default fn foo(&self) -> i32 {
//        8
//    }
//}
//
//// CONFLICTING
//
//trait HasFoo3 {
//    fn foo(&self) -> i32;
//}
//impl<T> HasFoo3 for T
//    where T: Deref,
//          <T as Deref>::Target: HasFoo3 {
//    default fn foo(&self) -> i32 {
//        self.deref().foo()
//    }
//}
//impl<T> HasFoo3 for WithID<T> {
//    default fn foo(&self) -> i32 {
//        8
//    }
//}


// =============
// === Usage ===
// =============

struct _A(i32);

type _X = WithLabel<WithID<_A>>;

fn _test<T: HasID + HasLabel> (t: T) {
    println!("{:?}", t.label());
    println!("{:?}", t.id());
}

fn _main() {
    let v1 = WithLabel("label1".to_string(), WithID(0, _A(1)));
    _test(v1); // THIS IS EXAMPLE USE CASE WHICH DOES NOT COMPILE

//    println!("{}", 7.foo());
}