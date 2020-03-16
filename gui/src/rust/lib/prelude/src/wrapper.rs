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
pub trait HasSizedContent = HasContent where Content<Self> : Sized;

/// Trait for objects which wrap values. Please note that this implements safe wrappers, so the
/// object - value relation must be bijective.
pub trait Wrapper = Wrap + ContentRef;

/// Wrapping utility for values.
pub trait Wrap : HasSizedContent {
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
pub trait ContentRef : HasContent {
    /// Unwraps this type to get the inner value.
    fn content(&self) -> &Self::Content;
}

/// Runs a function on the reference to the content.
pub trait WithContent : HasSizedContent {
    /// Runs a function on the reference to the content.
    fn with_content<F,T>(&self,f:F) -> T where F : FnOnce(&Content<Self>) -> T;
}

/// Unwraps the content by consuming this value.
pub trait Unwrap : HasSizedContent {
    /// Unwraps the content by consuming this value.
    fn unwrap(self) -> Self::Content;
}


// === Utils ===

/// Wraps the value and returns the wrapped type.
pub fn wrap<T:Wrap>(t:T::Content) -> T {
    T::wrap(t)
}

/// Provides reference to the content of this value.
pub fn content<T:ContentRef>(t:&T) -> &T::Content {
    T::content(t)
}

/// Unwrap the content by consuming this value.
pub fn unwrap<T:Unwrap>(t:T) -> T::Content {
    T::unwrap(t)
}


// === Default Impls ===

impl<T:ContentRef + HasSizedContent> WithContent for T {
    fn with_content<F,S>(&self,f:F) -> S
        where F : FnOnce(&Content<Self>) -> S {
        f(self.content())
    }
}

// TODO: This should be implemented with the marker trait overlapping rules magic.
// impl<T:Deref> Unwrap for T
// where <T as Deref>::Target: Unwrap {
//     default fn unwrap(&self) -> &Self::Content {
//         self.deref().unwrap()
//     }
// }


// === Impls ===

impl<T:?Sized> HasContent for Rc<T> { type Content = T; }
impl<T>        Wrap       for Rc<T> { fn wrap(t:T) -> Self { Rc::new(t) } }
impl<T:?Sized> ContentRef for Rc<T> { fn content(&self) -> &Self::Content { self.deref() }}

impl HasContent for String { type Content = char; }
impl Wrap       for String { fn wrap(t:char) -> Self { t.to_string() } }

impl<T> HasContent for Vec<T> { type Content = T; }
impl<T> Wrap       for Vec<T> { fn wrap(t:T) -> Self { vec![t] } }
