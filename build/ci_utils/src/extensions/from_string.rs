use crate::prelude::*;

use anyhow::Context;
use std::any::type_name;



pub trait FromString: Sized {
    fn from_str(s: &str) -> Result<Self>;

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
