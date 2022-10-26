//! Extensions fot string-like types.

use crate::prelude::*;

/// Extension methods for strings and similar types.
pub trait StrLikeExt {
    /// Convenience variant of `FromString::from_str`.
    ///
    /// Should be preferred over [`str::parse`] due to better error messages.
    // FIXME: this needs better name!
    fn parse2<T: FromString>(&self) -> Result<T>;

    /// Convenience variant of `FromString::parse_into`.
    fn parse_through<T, R>(&self) -> Result<R>
    where
        T: FromString + TryInto<R>,
        <T as TryInto<R>>::Error: Into<anyhow::Error>;
}

impl<S: AsRef<str>> StrLikeExt for S {
    fn parse2<U: FromString>(&self) -> Result<U> {
        U::from_str(self.as_ref())
    }

    fn parse_through<T, R>(&self) -> Result<R>
    where
        T: FromString + TryInto<R>,
        <T as TryInto<R>>::Error: Into<anyhow::Error>, {
        T::parse_into(self.as_ref())
    }
}
