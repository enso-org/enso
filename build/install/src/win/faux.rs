//! Faux definitions to use on non-Windows platforms.

#![allow(non_camel_case_types)]

use crate::prelude::*;

#[derive(Clone, Copy, Debug)]
pub struct RegKey;

impl RegKey {
    pub const fn predef(_: HKEY) -> Self {
        panic!("Not supported on non-Windows platforms.")
    }

    pub fn open_subkey_with_flags(&self, _: &str, _: REG_SAM_FLAGS) -> Result<Self> {
        panic!("Not supported on non-Windows platforms.")
    }

    pub fn create_subkey(&self, _: &str) -> Result<(Self, RegDisposition)> {
        panic!("Not supported on non-Windows platforms.")
    }

    pub fn delete_subkey_all(&self, _: &str) -> Result {
        panic!("Not supported on non-Windows platforms.")
    }

    pub fn set_value<T: ToRegValue, N: AsRef<OsStr>>(&self, name: N, value: &T) -> Result {
        panic!("Not supported on non-Windows platforms.")
    }
}

#[derive(Clone, Copy, Debug)]
pub struct RegValue;

pub fn open_subkey_with_flags(key: &RegKey, subkey: &str, flags: u32) -> Result<RegKey> {
    panic!("Not supported on non-Windows platforms.")
}

pub trait ToRegValue {
    fn to_reg_value(&self) -> RegValue;
}

impl ToRegValue for &str {
    fn to_reg_value(&self) -> RegValue {
        panic!("Not supported on non-Windows platforms.")
    }
}

impl ToRegValue for String {
    fn to_reg_value(&self) -> RegValue {
        panic!("Not supported on non-Windows platforms.")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RegDisposition {}

pub type HKEY = isize;

pub const HKEY_CURRENT_USER: HKEY = 0;

pub type REG_SAM_FLAGS = u32;

pub const KEY_READ: REG_SAM_FLAGS = 0;

pub const KEY_WRITE: REG_SAM_FLAGS = 0;


pub mod mslnk {
    use crate::prelude::*;

    pub struct ShellLink;

    impl ShellLink {
        pub fn new(_: &Path) -> Result<Self> {
            panic!("Not supported on non-Windows platforms.")
        }

        pub fn set_name(&mut self, _: Option<String>) {
            panic!("Not supported on non-Windows platforms.")
        }

        pub fn create_lnk(&self, _: &Path) -> Result {
            panic!("Not supported on non-Windows platforms.")
        }
    }
}