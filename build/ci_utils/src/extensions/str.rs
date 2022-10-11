use crate::prelude::*;

use anyhow::Context;
use std::any::type_name;



pub trait StrLikeExt {
    // FIXME: this needs better name!
    fn parse2<T: FromString>(&self) -> Result<T>;

    fn parse_through<T, R>(&self) -> Result<R>
    where
        T: FromString + TryInto<R>,
        <T as TryInto<R>>::Error: Into<anyhow::Error>, {
        self.parse2::<T>()?.try_into().anyhow_err().context(format!(
            "Failed to convert {} => {}.",
            type_name::<Self>(),
            type_name::<R>(),
        ))
    }
}

impl<T: AsRef<str>> StrLikeExt for T {
    fn parse2<U: FromString>(&self) -> Result<U> {
        U::from_str(self.as_ref())
    }
}
