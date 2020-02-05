//! This type defines Wrap / Unwrap utilities. Unwrap is like `Deref` but does not implement
//! `impl<'a, T> Unwrap for &'a T` in order to make it less error prone. `Wrap` is like `pure` in
//! applicative functors – if lifts a value to the specific type.

use crate::std_reexports::*;

// ===============
// === Wrapper ===
// ===============

/// Trait for any type which wraps other type. See docs of `Wrapper` to learn more.
pub trait HasContent {
    type Content : ?Sized;
}

/// Accessor for the wrapped value.
pub type Content<T> = <T as HasContent>::Content;

/// Trait which enables `Sized` super-bound on the `Content` type.
pub trait SizedContent = HasContent where Content<Self> : Sized;

/// Trait for objects which wrap values. Please note that this implements safe wrappers, so the
/// object - value relation must be bijective.
pub trait Wrapper = Wrap + Unwrap;

/// Wrapping utility for values.
pub trait Wrap : HasContent + SizedContent {
    /// Wraps the value and returns the wrapped type.
    fn wrap(t:Self::Content) -> Self;
}

/// Unwrapping utility for wrapped types.
///
/// Please note that this trait is very similar to the Deref trait. However, there is a very
/// important difference. Unlike `Deref`, there is no `impl<'a, T> Unwrap for &'a T` defined. The
/// existence of such impl is very error prone when writing complex impls. The `Deref` docs warn
/// about it explicitly: "[...] Because of this, Deref should only be implemented for smart pointers
/// to avoid confusion.". As an example, consider the following code which contains infinite loop:
///
/// ```compile_fail
/// pub trait HasId {
///     fn id(&self) -> usize;
/// }
///
/// // Notice the lack of bound `<T as Deref>::Target : HasId`
/// impl<T:Deref> HasId for T {
///     fn id(&self) -> usize {
///         self.deref().id()
///     }
/// }
/// ```
///
/// And the correct version:
///
/// ```compile_fail
/// pub trait HasId {
///     fn id(&self) -> usize;
/// }
///
/// // Notice the lack of bound `<T as Deref>::Target : HasId`
/// impl<T:Deref> HasId for T where <T as Deref>::Target : HasId {
///     fn id(&self) -> usize {
///         self.deref().id()
///     }
/// }
/// ```
///
/// Both versions compile fine, but the former loops for ever.
pub trait Unwrap : HasContent {
    /// Unwraps this type to get the inner value.
    fn unwrap(&self) -> &Self::Content;
}


// === Utils ===

/// Wraps the value and returns the wrapped type.
pub fn wrap<T:Wrap>(t:T::Content) -> T {
    T::wrap(t)
}

/// Unwraps this type to get the inner value.
pub fn unwrap<T:Unwrap>(t:&T) -> &T::Content {
    T::unwrap(t)
}


// === Default Impls ===

// FIXME: https://github.com/rust-lang/rust/issues/68776
//default impl<T:Deref> HasContent for T {
//    type Content = <Self as Deref>::Target;
//}

default impl<T> Unwrap for T
    where T:Deref<Target=Content<T>> {
    fn unwrap (&self) -> &Self::Content {
        self.deref()
    }
}


// === Impls ===

impl<T:?Sized> HasContent for Rc<T> { type Content = T; }
impl<T>        Wrap       for Rc<T> { fn wrap(t:T) -> Self { Rc::new(t) } }
impl<T:?Sized> Unwrap     for Rc<T> {}

impl HasContent for String { type Content = char; }
impl Wrap       for String { fn wrap(t:char) -> Self { t.to_string() } }

impl<T> HasContent for Vec<T> { type Content = T; }
impl<T> Wrap       for Vec<T> { fn wrap(t:T) -> Self { vec![t] } }
