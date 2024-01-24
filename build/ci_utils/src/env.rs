use crate::prelude::*;

use crate::env::accessor::PathLike;
use crate::env::accessor::RawVariable;
use crate::env::accessor::TypedVariable;

use anyhow::Context;
use std::collections::BTreeSet;
use unicase::UniCase;


// ==============
// === Export ===
// ==============

pub mod accessor;
pub mod known;



/// Like [`std::env::current_dir`], but with nicer error message.
pub fn current_dir() -> Result<PathBuf> {
    std::env::current_dir().context("Failed to get current directory.")
}

/// Like [`std::env::set_current_dir`], but with log and nicer error message.
pub fn set_current_dir(path: impl AsRef<Path>) -> Result {
    debug!("Changing working directory to {}.", path.as_ref().display());
    std::env::set_current_dir(&path)
        .with_context(|| format!("Failed to set current directory to {}.", path.as_ref().display()))
}

/// Run the given function with the current directory set to the given path.
///
/// After the function returns, the previous current directory is restored, even if the function
/// fails.
pub fn try_with_current_dir(path: impl AsRef<Path>, f: impl FnOnce() -> Result) -> Result {
    let old_dir = current_dir()?;
    set_current_dir(&path)?;
    let result = f();
    set_current_dir(old_dir)?;
    result
}

/// Like [`std::env::current_exe`], but with nicer error message.
pub fn current_exe() -> Result<PathBuf> {
    std::env::current_exe().context("Failed to get current executable path.")
}

/// Like [`std::env::set_var`], but with log.
pub fn set_var<K: AsRef<OsStr>, V: AsRef<OsStr>>(key: K, value: V) {
    debug!(
        "Setting environment variable {} to {}.",
        key.as_ref().as_str(),
        value.as_ref().as_str()
    );
    std::env::set_var(key, value)
}

/// Like [`std::env::remove_var`], but with log.
pub fn remove_var<K: AsRef<OsStr>>(key: K) {
    debug!("Removing environment variable {}.", key.as_ref().as_str());
    std::env::remove_var(key)
}

/// Define typed accessors for environment variables. Supported types include `String`, `PathBuf`,
/// and other types that implement `FromStr`.
///
/// Example:
/// ```
/// # use std::path::PathBuf;
/// # use ide_ci::define_env_var;
/// # use ide_ci::env::accessor::TypedVariable;
/// define_env_var! {
///     /// Documentation.
///     ENV_VAR_NAME, PathBuf;
/// }
/// let path = ENV_VAR_NAME.get().unwrap_or_else(|_error| PathBuf::from("default"));
/// ```
#[macro_export]
macro_rules! define_env_var {
    () => {};
    ($(#[$attr:meta])* $name: ident, Vec<PathBuf>; $($tail:tt)*) => {
        #[allow(non_upper_case_globals)]
        $(#[$attr])*
        pub const $name: $crate::env::accessor::PathLike =
            $crate::env::accessor::PathLike(stringify!($name));
        $crate::define_env_var!($($tail)*);
    };
    ($(#[$attr:meta])* $name: ident, PathBuf; $($tail:tt)*) => {
        #[allow(non_upper_case_globals)]
        $(#[$attr])*
        pub const $name: $crate::env::accessor::PathBufVariable =
            $crate::env::accessor::PathBufVariable(stringify!($name));
        $crate::define_env_var!($($tail)*);
    };
    ($(#[$attr:meta])* $name: ident, String; $($tail:tt)*) => {
        #[allow(non_upper_case_globals)]
        $(#[$attr])*
        pub const $name: $crate::env::accessor::SimpleVariable<String, str> =
            $crate::env::accessor::SimpleVariable::new(stringify!($name));
        $crate::define_env_var!($($tail)*);
    };
    ($(#[$attr:meta])* $name: ident, $ty_name: ty; $($tail:tt)*) => {
        #[allow(non_upper_case_globals)]
        $(#[$attr])*
        pub const $name: $crate::env::accessor::SimpleVariable<$ty_name> =
            $crate::env::accessor::SimpleVariable::new(stringify!($name));
        $crate::define_env_var!($($tail)*);
    };
}

/// Get the value of the environment variable. If it is not set, return an error.
///
/// Meant as a replacement for [`std::env::var`] which does not provide a nice error message.
pub fn expect_var(name: impl AsRef<str>) -> Result<String> {
    let name = name.as_ref();
    std::env::var(name).with_context(|| format!("Missing environment variable {name}."))
}

/// Like [`expect_var`] but returns a [`OsString`].
pub fn expect_var_os(name: impl AsRef<OsStr>) -> Result<OsString> {
    let name = name.as_ref();
    std::env::var_os(name)
        .with_context(|| format!("Missing environment variable {}.", name.to_string_lossy()))
}

pub fn prepend_to_path(path: impl AsRef<Path>) -> Result {
    known::PATH.prepend(path)
}

/// A modification to some environment variable.
#[derive(Clone, Debug)]
pub enum Action {
    /// Remove the variable.
    Remove,
    /// Set the variable to the given value.
    Set(String),
    /// Prepend the given paths to the variable.
    PrependPaths(Vec<PathBuf>),
}

/// A modification of a given environment variable.
#[derive(Clone, Debug)]
pub struct Modification {
    /// The name of the variable.
    ///
    /// # Note
    /// This name if case-insensitive on all platforms, like on Windows. This is to reduce risk of
    /// introducing bugs on Windows by relying on non-Windows-specific behavior. One could ask why
    /// not the other way around, i.e. why not make it case-sensitive on Windows?
    /// Well, bugs somehow do not happen that way.
    pub variable_name: UniCase<String>,
    /// The action to perform on the variable.
    pub action:        Action,
}

impl Modification {
    pub fn new(variable: &impl RawVariable, action: Action) -> Self {
        Self { variable_name: UniCase::new(variable.name().into()), action }
    }

    pub fn prepend_path(variable: &PathLike, path: impl Into<PathBuf>) -> Self {
        Self::new(variable, Action::PrependPaths(vec![path.into()]))
    }

    pub fn set<V>(variable: &V, value: &V::Borrowed) -> Result<Self>
    where V: TypedVariable {
        Ok(Self::new(variable, Action::Set(variable.generate(value)?)))
    }

    pub fn apply(&self) -> Result {
        let normalized_name = &*self.variable_name;
        match &self.action {
            Action::Remove => remove_var(normalized_name),
            Action::Set(value) => {
                set_var(normalized_name, value);
            }
            Action::PrependPaths(paths_to_prepend) =>
                if let Ok(old_value) = std::env::var(normalized_name) {
                    debug!(
                        "Prepending to {} the following paths: {:?}",
                        self.variable_name, paths_to_prepend
                    );
                    let new_paths_set = paths_to_prepend.iter().collect::<BTreeSet<_>>();
                    let old_paths = std::env::split_paths(&old_value).collect_vec();

                    let old_paths_filtered =
                        old_paths.iter().filter(|old_path| !new_paths_set.contains(old_path));
                    let new_value =
                        std::env::join_paths(paths_to_prepend.iter().chain(old_paths_filtered))?;
                    set_var(&*self.variable_name, new_value);
                } else {
                    let new_value = std::env::join_paths(paths_to_prepend)?;
                    set_var(&*self.variable_name, new_value);
                },
        };
        Ok(())
    }
}
