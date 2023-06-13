use crate::Clearable;
use crate::Deref;
use crate::DerefMut;

use core::fmt::Debug;


// ==============
// === Export ===
// ==============

pub use bytemuck;
pub use bytemuck::Zeroable;



// ======================
// === ZeroableOption ===
// ======================


/// Just like [`Option`], but can be initialized with zeroed memory. The stdlib [`Option`] can not.
/// For example, `Option::<bool>::None` is not represented as zeroed memory, as can be checked with
/// `println!("{:?}", unsafe { std::mem::transmute::<Option<bool>, [u8; 1]>(None) });`, which prints
/// `[2]`.
#[derive(Clone, Copy, Debug, Default)]
#[repr(u8)]
#[allow(missing_docs)]
pub enum ZeroableOption<T> {
    #[default]
    None = 0,
    Some(T),
}

// # Safety
// The [`ZeroableOption`] was designed to be zeroable. In particular, it uses `repr(u8)` and the
// [`ZeroableOption::None`] variant is associated with the `0` value.
#[allow(unsafe_code)]
unsafe impl<T> Zeroable for ZeroableOption<T> {}

#[allow(missing_docs)] // All the functions are the same as on the stdlib [`Option`] type.
impl<T> ZeroableOption<T> {
    #[inline(always)]
    pub fn as_ref(&self) -> Option<&T> {
        match self {
            ZeroableOption::None => None,
            ZeroableOption::Some(v) => Some(v),
        }
    }

    #[inline(always)]
    pub fn as_mut(&mut self) -> Option<&mut T> {
        match self {
            ZeroableOption::None => None,
            ZeroableOption::Some(v) => Some(v),
        }
    }

    #[inline(always)]
    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> ZeroableOption<U> {
        match self {
            ZeroableOption::None => ZeroableOption::None,
            ZeroableOption::Some(v) => ZeroableOption::Some(f(v)),
        }
    }

    #[inline(always)]
    pub fn is_some(&self) -> bool {
        match self {
            ZeroableOption::None => false,
            ZeroableOption::Some(_) => true,
        }
    }

    #[inline(always)]
    pub fn is_none(&self) -> bool {
        !self.is_some()
    }

    #[inline(always)]
    pub fn unwrap(self) -> T {
        match self {
            ZeroableOption::None => panic!("Called `ZeroableOption::unwrap()` on a `None` value."),
            ZeroableOption::Some(v) => v,
        }
    }

    #[inline(always)]
    pub fn into_option(self) -> Option<T> {
        match self {
            ZeroableOption::None => None,
            ZeroableOption::Some(v) => Some(v),
        }
    }
}

impl<T> From<T> for ZeroableOption<T> {
    #[inline(always)]
    fn from(v: T) -> Self {
        ZeroableOption::Some(v)
    }
}

impl<T> Clearable for ZeroableOption<T> {
    #[inline(always)]
    fn clear(&mut self) {
        *self = ZeroableOption::None;
    }
}



// =========================
// === ZeroableStaticStr ===
// =========================

/// A `&'static str` that can be initialized with zeroed memory.
#[derive(Clone, Copy, Zeroable)]
#[repr(transparent)]
pub struct ZeroableStaticStr {
    opt_str: ZeroableOption<&'static str>,
}

impl Deref for ZeroableStaticStr {
    type Target = str;
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        (*self).into()
    }
}

impl AsRef<str> for ZeroableStaticStr {
    #[inline(always)]
    fn as_ref(&self) -> &str {
        self.deref()
    }
}

impl Debug for ZeroableStaticStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self.as_ref(), f)
    }
}

impl From<&'static str> for ZeroableStaticStr {
    #[inline(always)]
    fn from(s: &'static str) -> Self {
        Self { opt_str: ZeroableOption::Some(s) }
    }
}

impl From<ZeroableStaticStr> for &'static str {
    #[inline(always)]
    fn from(s: ZeroableStaticStr) -> Self {
        match s.opt_str {
            ZeroableOption::None => "",
            ZeroableOption::Some(s) => s,
        }
    }
}



// =======================
// === ZeroableRefCell ===
// =======================

/// Version of [`RefCell`] that can be initialized with zeroed memory.
#[derive(Clone, Deref, Default, DerefMut)]
#[repr(transparent)]
pub struct ZeroableRefCell<T> {
    // Please note, that current [`RefCell`] implementation CAN be initialized with zeroed memory,
    // however, this is not a contract provided by the Rust standard library. Thus, in order not to
    // copy-paste its code, we are using the stdlib version, but we need to check if the
    // implementation did not change with the compiler version bump.
    //
    // To check if it is still correct after the version bump, you need to check if
    // [`RefCell::new`] initializes the struct with zero memory. To do it, check if every field
    // of [`RefCell`] defaults to a zeroable value. In the current case, [`RefCell`] has the
    // following fields:
    //
    // 1. `borrow: Cell<BorrowFlag>`, where `type BorrowFlag = isize;` and it is initialized with
    // `const UNUSED: BorrowFlag = 0;`.
    //
    // 2. `borrowed_at: Cell<Option<& ...>>`, which is zeroable, according to:
    // https://docs.rs/bytemuck/latest/bytemuck/trait.ZeroableInOption.html
    //
    // 3. `value: UnsafeCell<T>`, which is zeroable if `T` is zeroable, according to:
    // https://docs.rs/bytemuck/latest/bytemuck/trait.Zeroable.html
    #[cfg(not(version("1.69")))]
    cell: core::cell::RefCell<T>,
    #[cfg(version("1.69"))]
    after_compiler_version_bump: update_this_code_according_to_the_comment_above,
}

// # Safety
// Please see safety guarantees described in the comment inside the [`ZeroableRefCell`] struct
// implementation.
#[allow(unsafe_code)]
unsafe impl<T: Zeroable> Zeroable for ZeroableRefCell<T> {}

impl<T> ZeroableRefCell<T> {
    /// Constructor.
    pub fn new(t: T) -> Self {
        Self { cell: core::cell::RefCell::new(t) }
    }
}

impl<T: Debug> Debug for ZeroableRefCell<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.cell, f)
    }
}

impl<T> From<T> for ZeroableRefCell<T> {
    #[inline(always)]
    fn from(t: T) -> Self {
        Self { cell: t.into() }
    }
}



// ==============
// === Macros ===
// ==============

/// Allows deriving the [`Zeroable`] trait for parametrized structures without enforcing the
/// parameters to implement [`Zeroable`] as well. It is similar to how the [`Derivative`] macro
/// works. This is just a workaround until the official [`Zeroable`] derive macro supports custom
/// bounds. To learn more, see: https://github.com/Lokathor/bytemuck/issues/190.
#[macro_export]
macro_rules! derive_zeroable {
    (
        $(#$meta:tt)*
        pub struct $name:ident $([ $($bounds:tt)* ] [ $($bounds_impl:tt)* ] [ $($bounds_def:tt)* ])? {
            $($field:ident : $ty:ty),* $(,)?
        }
    ) => {
        $(#$meta)*
        pub struct $name $(< $($bounds_def)* >)? {
            $($field : $ty),*
        }
        unsafe impl $(< $($bounds_impl)* >)? $crate::Zeroable for $name $(< $($bounds)* >)?
            where $($ty: Zeroable),* {}
    };
}
