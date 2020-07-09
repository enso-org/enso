//! This module reexports several commonly used types defined in the standard library.

// === Data ===

pub use std::any::Any;
pub use std::borrow::Cow;
pub use std::hash::Hash;
pub use std::marker::PhantomData;
pub use std::ops::Range;
pub use std::ops::RangeBounds;
pub use std::ops::RangeFrom;
pub use std::ops::RangeFull;
pub use std::ops::RangeInclusive;
pub use std::ops::RangeTo;
pub use std::ops::RangeToInclusive;

// === Format ===

pub use core::any::type_name;
pub use core::fmt::Debug;
pub use std::fmt::Display;
pub use std::fmt;
pub use std::iter::FromIterator;
pub use std::iter;


// === Data Operations ===

pub use std::ops::Deref;
pub use std::ops::DerefMut;
pub use std::ops::Index;
pub use std::ops::IndexMut;


// === Conversion ===

pub use std::convert::identity;
pub use std::convert::TryFrom;
pub use std::convert::TryInto;


// === References ===

pub use std::cell::Cell;
pub use std::cell::Ref;
pub use std::cell::RefCell;
pub use std::cell::RefMut;
pub use std::rc::Rc;
pub use std::rc::Weak;
pub use std::slice::SliceIndex;
pub use std::slice;


// === Operators ===

pub use std::ops::Add;
pub use std::ops::Div;
pub use std::ops::Mul;
pub use std::ops::Neg;
pub use std::ops::Sub;


// === Utils ===

pub use std::mem;

/// Alias for `Default::default()`.
pub fn default<T:Default>() -> T {
    Default::default()
}
