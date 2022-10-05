use crate::prelude::*;

use crate::global::store_static_text;

use clap::Arg;



/// Extensions to the `clap::Arg`, intended to be used as argument attributes.
pub trait ArgExt<'h>: Sized + 'h {
    /// If the given value is `Some`, set it as a default.
    ///
    /// Useful primarily when presence of default value on a CLI argument depends on runtime
    /// conditions.
    fn maybe_default<S: AsRef<str>>(self, f: impl Borrow<Option<S>>) -> Self;

    fn maybe_default_os<S: AsRef<OsStr>>(self, f: Option<S>) -> Self {
        let f = f.as_ref().map(AsRef::as_ref);
        self.maybe_default(f.as_ref().map(|s| s.as_str()))
    }

    fn maybe_default_t<S: ToString>(self, f: impl Borrow<Option<S>> + 'h) -> Self {
        let printed = f.borrow().as_ref().map(|v| v.to_string());
        self.maybe_default(printed)
    }

    /// Like `env` but prefixes the generated environment variable name with
    /// `ENVIRONMENT_VARIABLE_NAME_PREFIX`.
    fn prefixed_env(self, prefix: impl AsRef<str>) -> Self;
}

impl<'h> ArgExt<'h> for Arg<'h> {
    fn maybe_default<S: AsRef<str>>(self, f: impl Borrow<Option<S>>) -> Self {
        if let Some(default) = f.borrow().as_ref() {
            self.default_value(store_static_text(default)).required(false)
        } else {
            self
        }
    }

    fn prefixed_env(self, prefix: impl AsRef<str>) -> Self {
        use heck::ToShoutySnakeCase;
        let var_name = format!("{}_{}", prefix.as_ref(), self.get_id().to_shouty_snake_case());
        self.env(store_static_text(var_name))
    }
}
