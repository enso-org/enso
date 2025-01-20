//! Wrapper around the `winreg` crate functions that provides better error messages.

use crate::prelude::*;

use winreg::enums::RegDisposition;



/// Wrapper around [`RegKey::delete_subkey_all`] that provides better error messages.
pub fn delete_subkey_all(key: &RegKey, subkey: &str) -> Result {
    key.delete_subkey_all(subkey)
        .with_context(|| format!("Failed to delete subkey `{subkey}` in `{key:?}`."))
}

/// Wrapper around [`RegKey::create_subkey`] that provides better error messages.
pub fn create_subkey(key: &RegKey, subkey: &str) -> Result<(RegKey, RegDisposition)> {
    key.create_subkey(subkey)
        .with_context(|| format!("Failed to create subkey `{subkey}` in `{key:?}`."))
}

/// Wrapper around [`RegKey::open_subkey_with_flags`] that provides better error messages.
pub fn open_subkey_with_flags(key: &RegKey, subkey: &str, flags: u32) -> Result<RegKey> {
    key.open_subkey_with_flags(subkey, flags)
        .with_context(|| format!("Failed to open subkey `{subkey}` in `{key:?}`."))
}

/// Wrapper around [`RegKey::set_value`] that provides better error messages.
pub fn set_value(key: &RegKey, name: &str, value: &impl ToRegValue) -> Result {
    key.set_value(name, value)
        .with_context(|| format!("Failed to set value `{name}` in `{key:?}`."))
}
