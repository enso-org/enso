//!Module with utilities for converting string-like values into other types.

use crate::prelude::*;

use anyhow::Context;
use std::any::type_name;



/// An equivalent of standard's library `std::str::FromStr` trait, but with nice error messages.
pub trait FromString: Sized {
    /// Parse a string into a value of this type. See: [`std::str::FromStr::from_str`].
    fn from_str(s: &str) -> Result<Self>;

    /// Parse a string into a value of this type and then convert it to `R`.
    fn parse_into<R>(text: impl AsRef<str>) -> Result<R>
    where
        Self: TryInto<R>,
        <Self as TryInto<R>>::Error: Into<anyhow::Error>, {
        let value = Self::from_str(text.as_ref())?;
        value.try_into().anyhow_err().context(format!(
            "Failed to convert {} => {}.",
            type_name::<Self>(),
            type_name::<R>(),
        ))
    }
}

impl<T> FromString for T
where
    T: std::str::FromStr,
    T::Err: Into<anyhow::Error>,
{
    fn from_str(text: &str) -> Result<Self> {
        text.parse::<T>().anyhow_err().context(format!(
            r#"Failed to parse "{}" as {}."#,
            text,
            type_name::<T>()
        ))
    }
}
