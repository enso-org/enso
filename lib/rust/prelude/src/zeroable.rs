pub use bytemuck;
pub use bytemuck::Zeroable;

use core::fmt::Debug;
use std::ops::Deref;

#[derive(Clone, Copy, Debug, Default)]
#[repr(u8)]
pub enum ZeroableOption<T> {
    #[default]
    None = 0,
    Some(T),
}

unsafe impl<T> Zeroable for ZeroableOption<T> {}

impl<T> ZeroableOption<T> {
    pub fn as_ref(&self) -> ZeroableOption<&T> {
        match self {
            ZeroableOption::None => ZeroableOption::None,
            ZeroableOption::Some(v) => ZeroableOption::Some(v),
        }
    }

    pub fn as_mut(&mut self) -> ZeroableOption<&mut T> {
        match self {
            ZeroableOption::None => ZeroableOption::None,
            ZeroableOption::Some(v) => ZeroableOption::Some(v),
        }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> ZeroableOption<U> {
        match self {
            ZeroableOption::None => ZeroableOption::None,
            ZeroableOption::Some(v) => ZeroableOption::Some(f(v)),
        }
    }

    pub fn is_some(&self) -> bool {
        match self {
            ZeroableOption::None => false,
            ZeroableOption::Some(_) => true,
        }
    }

    pub fn is_none(&self) -> bool {
        !self.is_some()
    }

    pub fn unwrap(self) -> T {
        match self {
            ZeroableOption::None => panic!("Called `ZeroableOption::unwrap()` on a `None` value."),
            ZeroableOption::Some(v) => v,
        }
    }
}


#[derive(Clone, Copy, Zeroable)]
pub struct ZeroableStaticStr {
    opt_str: ZeroableOption<&'static str>,
}

impl Deref for ZeroableStaticStr {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        match self.opt_str {
            ZeroableOption::None => "",
            ZeroableOption::Some(s) => s,
        }
    }
}

impl AsRef<str> for ZeroableStaticStr {
    fn as_ref(&self) -> &str {
        self.deref()
    }
}

impl Debug for ZeroableStaticStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self.as_ref(), f)
    }
}
