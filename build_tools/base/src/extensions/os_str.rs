//! Extensions for [`OsStr`].

use crate::prelude::*;



/// Extension methods for [`OsStr`].
pub trait OsStrExt {
    /// Get the underlying string as `str`.
    ///
    /// # Safety
    /// This will panic if the path contains invalid UTF-8 characters. Non-UTF-8 paths are not
    /// something that we want to spend time on supporting right now.
    fn as_str(&self) -> &str;
}

impl OsStrExt for OsStr {
    fn as_str(&self) -> &str {
        self.to_str().unwrap_or_else(|| panic!("String is not valid UTF-8: {self:?}"))
    }
}
