//! This module re-exports a lot of useful stuff. It is not meant to be used
//! by libraries, but it is definitely usefull for bigger projects. It also
//! defines several aliases and utils which may find their place in new
//! libraries in the future.

#![feature(specialization)]
#![feature(trait_alias)]

pub use boolinator::Boolinator;
pub use core::any::type_name;
pub use core::fmt::Debug;
pub use derivative::Derivative;
pub use derive_more::*;
pub use failure::Fail;
pub use itertools::Itertools;
pub use num::Num;
pub use paste;
pub use shrinkwraprs::Shrinkwrap;
pub use smallvec::SmallVec;
pub use std::cell::Ref;
pub use std::cell::RefMut;
pub use std::cell::RefCell;
pub use std::collections::HashMap;
pub use std::collections::HashSet;
pub use std::convert::identity;
pub use std::convert::TryFrom;
pub use std::convert::TryInto;
pub use std::fmt;
pub use std::fmt::Display;
pub use std::hash::Hash;
pub use std::iter;
pub use std::iter::FromIterator;
pub use std::marker::PhantomData;
pub use std::ops::Deref;
pub use std::ops::DerefMut;
pub use std::ops::Index;
pub use std::ops::IndexMut;
pub use std::rc::Rc;
pub use std::rc::Weak;
pub use std::slice;
pub use std::slice::SliceIndex;
pub use lazy_static::lazy_static;

use nalgebra::Matrix;
use nalgebra::DimName;
use nalgebra::Scalar;


/// Abstraction for any kind of string as an argument. Functions defined as
/// `fn test<S:Str>(s: Str) { ... }` can be called with `String`, `&String`,
/// and `&str` without requiring caller to know the implementation details.
pub trait Str = AsRef<str>;

/// Alias for `Default::default()`.
pub fn default<T: Default>() -> T {
    Default::default()
}

/// The following `PhantomData` implementations allow each argument to be non
/// Sized. Unfortunately, this is not equivalent to `PhantomData<(T1,T2,...)>`,
/// as tuple requires each arg to implement `Sized`.
pub type PhantomData2<T1,T2>                      = PhantomData<(PhantomData <T1>,                      PhantomData<T2>)>;
pub type PhantomData3<T1,T2,T3>                   = PhantomData2<PhantomData2<T1,T2>,                   PhantomData<T3>>;
pub type PhantomData4<T1,T2,T3,T4>                = PhantomData2<PhantomData3<T1,T2,T3>,                PhantomData<T4>>;
pub type PhantomData5<T1,T2,T3,T4,T5>             = PhantomData2<PhantomData4<T1,T2,T3,T4>,             PhantomData<T5>>;
pub type PhantomData6<T1,T2,T3,T4,T5,T6>          = PhantomData2<PhantomData5<T1,T2,T3,T4,T5>,          PhantomData<T6>>;
pub type PhantomData7<T1,T2,T3,T4,T5,T6,T7>       = PhantomData2<PhantomData6<T1,T2,T3,T4,T5,T6>,       PhantomData<T7>>;
pub type PhantomData8<T1,T2,T3,T4,T5,T6,T7,T8>    = PhantomData2<PhantomData7<T1,T2,T3,T4,T5,T6,T7>,    PhantomData<T8>>;
pub type PhantomData9<T1,T2,T3,T4,T5,T6,T7,T8,T9> = PhantomData2<PhantomData8<T1,T2,T3,T4,T5,T6,T7,T8>, PhantomData<T9>>;

/// Surprisingly useful function. Consider the following code:
///
/// ```compile_fail
/// fn init(self) -> Self {
///    let mut data = self.borrow_mut();
///    ...
///    self
///    }
/// ```
///
/// It may not compile telling that the last line moves self out, however,
/// borrow might be used there, when `data` is dropped and runs the destructor.
///
/// We can usethis function to narrow-down the lifetimes. The following code
/// compiles just fine:
///
/// ```compile_fail
/// fn init(self) -> Self {
///    with(self.borrow_mut(), |mut data| {
///        ...
///    });
///    self
///    }
/// ```
pub fn with<T, F: FnOnce(T) -> Out, Out>(t: T, f: F) -> Out { f(t) }

pub trait OptionOps {
    type Item;
    fn for_each<U, F: FnOnce(Self::Item) -> U>(self, f: F);
}

impl<T> OptionOps for Option<T> {
    type Item = T;
    fn for_each<U, F: FnOnce(Self::Item) -> U>(self, f: F) {
        if let Some(x) = self { f(x); }
    }
}



// ================
// === CloneRef ===
// ================

/// Like `Clone` but should be implemented only for cheap reference-based clones. Using `clone_ref`
/// instead of `clone` makes the code more clear and makes it easier to predict its performance.
pub trait CloneRef {
    fn clone_ref(&self) -> Self;
}



// ===================
// === WithPhantom ===
// ===================

/// A wrapper adding a phantom type to a structure.
#[derive(Derivative)]
#[derive(Shrinkwrap)]
#[shrinkwrap(mutable)]
#[derivative(Clone   (bound="T:Clone"))]
#[derivative(Default (bound="T:Default"))]
#[derivative(Debug   (bound="T:Debug"))]
pub struct WithPhantom<T, P=()> {
    #[shrinkwrap(main_field)]
    pub without_phantom: T,
    phantom: PhantomData<P>
}

impl<T, P> WithPhantom<T, P> {
    pub fn new(without_phantom: T) -> Self {
        let phantom = PhantomData;
        Self { without_phantom, phantom }
    }
}



// =====================
// === Rc Extensions ===
// =====================

/// Using `clone` for structures which are newtype-wrappers over `Rc` is error
/// prone and hides the real intention. Cloning should always be considered
/// pricey. This trait adds `clone_rc` method to every such wrapper.
pub trait IsRc {
    fn clone_rc(&self) -> Self;
}

impl<T,S> IsRc for T
    where T: From<Rc<S>>, T:Deref<Target=Rc<S>> {
    fn clone_rc(&self) -> Self {
        Rc::clone(self).into()
    }
}

/// See the documentation of `IsRc`. Unfortunately, it's not easy to merge
/// these two traits together in current Rust version, so `clone_rc` method
/// for `Rc<S>` is provided separately.
pub trait RcOps {
    fn clone_rc(&self) -> Self;
}

impl<T> RcOps for Rc<T> {
    fn clone_rc(&self) -> Self {
        Rc::clone(self)
    }
}



// ===================
// === TypeDisplay ===
// ===================

/// Like `Display` trait but for types. However, unlike `Display` it defaults to
/// `core::any::type_name` if not provided with explicit implementation.
pub trait TypeDisplay {
    fn type_display() -> String;
}

impl<T> TypeDisplay for T {
    default fn type_display() -> String {
        type_name::<Self>().to_string()
    }
}

// TODO: This impl should be hidden behind a flag. Not everybody using prelude want to import
//       nalgebra as well.
impl <T:Scalar,R:DimName,C:DimName,S> TypeDisplay for Matrix<T,R,C,S> {
    fn type_display() -> String {
        let cols = <C as DimName>::dim();
        let rows = <R as DimName>::dim();
        let item = type_name::<T>();
        match cols {
            1 => format!("Vector{}<{}>"    , rows, item),
            _ => format!("Matrix{}x{}<{}>" , rows, cols, item)
        }
    }
}



// =================================================================================================
// ========================================= README ================================================
// The below code was used to create references inside shared mutable structures. After refactoring
// the codebase it turned out we are not using it anymore. Getting such references is very tricky to
// get right, so this code is left here commented in case it would be needed in the future.
// =================================================================================================
// =================================================================================================

//// ==========================
//// === RefCell Management ===
//// ==========================
//
//// === RefGuard ===
//
//pub struct RefGuard<'t,Base,Data> {
//    data    : &'t Data,
//    _borrow : Ref<'t,Base>,
//}
//
//impl<'t,Base,Data> Deref for RefGuard<'t,Base,Data> {
//    type Target = Data;
//    fn deref(&self) -> &Self::Target {
//        self.data
//    }
//}
//
//impl<'t,Base,Data> RefGuard<'t,Base,Data> {
//    pub fn new<F:FnOnce(&'t Base) -> &'t Data>(base:&'t RefCell<Base>, f:F) -> Self {
//        let _borrow   = base.borrow();
//        let reference = unsafe { drop_lifetime(&_borrow) };
//        let data      = f(reference);
//        RefGuard {data,_borrow}
//    }
//}
//
//impl<'t,Base,Data:Debug> Debug for RefGuard<'t,Base,Data> {
//    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//        self.data.fmt(f)
//    }
//}
//
//
//// === Unsafe Utils ===
//
///// # Safety
///// This is a very unsafe function, use it with caution please. There are few
///// legitimate use cases listed below. You are not allowed to use this function
///// for any other use case. If you discover a new possibly legitimate case,
///// confirm it with Luna Rust Core team and add its description below.
/////
///// In long-run, the below use cases should be replaced with safe-versions
///// implemented as macros.
/////
///// 1. Keeping mutually connected fields in a single structure. Especially
/////    useful when defining iterators for wrappers keeping containers behind
/////    a shared `Rc<Refcell<...>>` gate. An example:
/////
/////    ```compile_fail
/////    use std::rc::Rc;
/////    use core::cell::RefCell;
/////    use core::cell::Ref;
/////
/////    pub struct SharedDirtyFlag<T> {
/////        data: Rc<RefCell<T>>
/////    }
/////
/////    impl<T> SharedDirtyFlag<T>
/////    where for<'t> &'t T: IntoIterator {
/////        pub fn iter(&self) -> SharedDirtyFlagIter<T> {
/////            let borrow    = self.data.borrow();
/////            let reference = unsafe { drop_lifetime(&borrow) };
/////            let iter      = reference.into_iter();
/////            SharedDirtyFlagIter { iter, borrow }
/////        }
/////    }
/////
/////    // CAUTION !!!
/////    // Please keep the fields in the correct order. They will be dropped
/////    // in order. Moreover, keep the borrow field private.
/////    pub struct SharedDirtyFlagIter<'t,T>
/////    where &'t T: IntoIterator {
/////        pub iter : <&'t T as IntoIterator>::IntoIter,
/////        borrow   : Ref<'t,T>
/////    }
/////    ```
//pub unsafe fn drop_lifetime<'a,'b,T>(t: &'a T) -> &'b T {
//    std::mem::transmute(t)
//}
//
///// # Safety
///// Please see the `drop_lifetime` docs.
//pub unsafe fn drop_lifetime_mut<'a,'b,T>(t: &'a mut T) -> &'b mut T {
//    std::mem::transmute(t)
//}