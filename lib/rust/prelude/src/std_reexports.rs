//! This module reexports several commonly used types defined in the standard library.


// ==============
// === Export ===
// ==============

pub use core::any::type_name;
pub use core::fmt::Debug;
pub use std::any::Any;
pub use std::borrow::Cow;
pub use std::cell::Cell;
pub use std::cell::Ref;
pub use std::cell::RefCell;
pub use std::cell::RefMut;
pub use std::convert::identity;
pub use std::convert::TryFrom;
pub use std::convert::TryInto;
pub use std::fmt;
pub use std::fmt::Display;
pub use std::hash::Hash;
pub use std::iter;
pub use std::iter::FromIterator;
pub use std::marker::PhantomData;
pub use std::mem;
pub use std::ops::Add;
pub use std::ops::Deref;
pub use std::ops::DerefMut;
pub use std::ops::Div;
pub use std::ops::Index;
pub use std::ops::IndexMut;
pub use std::ops::Mul;
pub use std::ops::Neg;
pub use std::ops::Range;
pub use std::ops::RangeBounds;
pub use std::ops::RangeFrom;
pub use std::ops::RangeFull;
pub use std::ops::RangeInclusive;
pub use std::ops::RangeTo;
pub use std::ops::RangeToInclusive;
pub use std::ops::Sub;
pub use std::rc::Rc;
pub use std::rc::Weak;
pub use std::slice;
pub use std::slice::SliceIndex;
pub use std::str::FromStr;



/// Alias for `Default::default()`.
pub fn default<T: Default>() -> T {
    Default::default()
}
