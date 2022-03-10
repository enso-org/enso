//! This module defines helpers and utilities for working with references.



// ============
// === With ===
// ============

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
/// We can use this function to narrow-down the lifetimes. The following code
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
pub fn with<T, F: FnOnce(T) -> Out, Out>(t: T, f: F) -> Out {
    f(t)
}



// =============
// === ToRef ===
// =============

/// Similar to `AsRef` but more specific and automatically implemented for every type. Allows for
/// conversion `&T` to `&T` (identity) and `T` to `&T` for any type `T`. In contrast, `AsRef`
/// requires explicit impls, so for example you cannot do `let t:&() = ().as_ref()`
pub trait ToRef<T>
where T: ?Sized {
    fn to_ref(&self) -> &T;
}
impl<T> ToRef<T> for T
where T: ?Sized
{
    fn to_ref(&self) -> &T {
        self
    }
}
impl<T> ToRef<T> for &T
where T: ?Sized
{
    fn to_ref(&self) -> &T {
        self
    }
}

// pub trait ToRef = ?Sized + HasRefValue + ToRef__<RefValue<Self>>;

pub trait HasRefValue {
    type RefValue: ?Sized;
}
impl<T> HasRefValue for T
where T: ?Sized
{
    default type RefValue = T;
}
impl<T> HasRefValue for &T
where T: ?Sized
{
    type RefValue = T;
}

pub type RefValue<T> = <T as HasRefValue>::RefValue;



// =============
// === Owned ===
// =============

/// The owned version of a type. It would be super cool if Rust would allow us to automatically
/// implement it for every type: `Owned<&T> = T` and `Owned<T> = T` if `T` was not a reference.
/// Unfortunately, we need to implement it by hand for every type now.
pub trait AsOwned {
    type Owned;
}

/// Owned type family.
pub type Owned<T> = <T as AsOwned>::Owned;

/// Converts type to its owned version.
pub trait IntoOwned = AsOwned + Into<Owned<Self>>;


// === Default Impls ===

impl<T> AsOwned for &T {
    type Owned = T;
}
