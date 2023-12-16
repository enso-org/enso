use crate::prelude::*;

pub fn delete_subkey_all(key: &RegKey, subkey: &str) -> Result {
    key.delete_subkey_all(subkey)
        .with_context(|| format!("Failed to delete subkey `{}` in `{:?}`.", subkey, key))
}
