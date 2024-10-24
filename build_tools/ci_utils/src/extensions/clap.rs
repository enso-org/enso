use crate::prelude::*;

use clap::Arg;
use convert_case::Case;
use convert_case::Casing;



/// Extensions to the `clap::Arg`, intended to be used as argument attributes.
pub trait ArgExt: Sized {
    /// If the given value is `Some`, set it as a default.
    ///
    /// Useful primarily when presence of default value on a CLI argument depends on runtime
    /// conditions.
    fn maybe_default_os<S: Into<OsString>>(self, f: Option<S>) -> Self;


    /// Like `env` but prefixes the generated environment variable name with
    /// `ENVIRONMENT_VARIABLE_NAME_PREFIX`.
    fn prefixed_env(self, prefix: impl AsRef<str>) -> Self;
}

impl ArgExt for Arg {
    fn maybe_default_os<S: Into<OsString>>(self, f: Option<S>) -> Self {
        match f {
            Some(default) => self.default_value(default.into()).required(false),
            None => self,
        }
    }

    fn prefixed_env(self, prefix: impl AsRef<str>) -> Self {
        let var_name =
            format!("{}_{}", prefix.as_ref(), self.get_id().as_str().to_case(Case::UpperSnake));
        self.env(var_name)
    }
}
