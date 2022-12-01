use crate::prelude::*;

use anyhow::Context;
use std::collections::BTreeSet;
use unicase::UniCase;


// ==============
// === Export ===
// ==============

pub mod known;



pub fn current_dir() -> Result<PathBuf> {
    std::env::current_dir().context("Failed to get current directory.")
}

#[context("Failed to set current directory to {}.", path.as_ref().display())]
pub fn set_current_dir(path: impl AsRef<Path>) -> Result {
    debug!("Changing working directory to {}.", path.as_ref().display());
    std::env::set_current_dir(&path).anyhow_err()
}

/// Define typed accessors for environment variables. Supported types inclide `String`, `PathBuf`,
/// and other types that implement `FromStr`.
///
/// Example:
/// ```
/// # use std::path::PathBuf;
/// # use ide_ci::define_env_var;
/// # use ide_ci::env::new::TypedVariable;
/// define_env_var! {
///     /// Documentation.
///     ENV_VAR_NAME, PathBuf;
/// };
/// let path = ENV_VAR_NAME.get().unwrap_or_else(|_error| PathBuf::from("default"));
/// ```
#[macro_export]
macro_rules! define_env_var {
    () => {};
    ($(#[$attr:meta])* $name: ident, PathBuf; $($tail:tt)*) => {
        #[allow(non_upper_case_globals)]
        $(#[$attr])*
        pub const $name: $crate::env::new::PathBufVariable =
            $crate::env::new::PathBufVariable(stringify!($name));
        $crate::define_env_var!($($tail)*);
    };
    ($(#[$attr:meta])* $name: ident, String; $($tail:tt)*) => {
        #[allow(non_upper_case_globals)]
        $(#[$attr])*
        pub const $name: $crate::env::new::SimpleVariable<String, str> =
            $crate::env::new::SimpleVariable::new(stringify!($name));
        $crate::define_env_var!($($tail)*);
    };
    ($(#[$attr:meta])* $name: ident, $ty_name: ty; $($tail:tt)*) => {
        #[allow(non_upper_case_globals)]
        $(#[$attr])*
        pub const $name: $crate::env::new::SimpleVariable<$ty_name> =
            $crate::env::new::SimpleVariable::new(stringify!($name));
        $crate::define_env_var!($($tail)*);
    };
}

pub mod new {
    use super::*;
    use crate::program::command::FallibleManipulator;

    /// An environment variable of known name.
    ///
    /// "raw" means that we do not know its "real" type and deal only with strings. When more type
    /// safety is needed, implement `TypedVariable` as well.
    pub trait RawVariable {
        /// The name of this environment variable.
        fn name(&self) -> &str;

        /// Has this variable been set?
        ///
        /// Note that a variable may be set to the empty string. This can lead to unexpected
        /// results, because in some environments variables can be unset by setting them to the
        /// empty string.
        fn is_set(&self) -> bool {
            std::env::var(self.name()) != Err(std::env::VarError::NotPresent)
        }

        fn get_raw(&self) -> Result<String> {
            expect_var(self.name())
        }

        fn get_raw_os(&self) -> Result<OsString> {
            expect_var_os(self.name())
        }

        fn set_raw(&self, value: impl AsRef<OsStr>) {
            std::env::set_var(self.name(), value);
        }

        fn remove(&self) {
            std::env::remove_var(self.name());
        }
    }

    pub trait TypedVariable: RawVariable {
        type Value;
        type Borrowed: ?Sized = Self::Value;

        fn parse(&self, value: &str) -> Result<Self::Value>;
        fn generate(&self, value: &Self::Borrowed) -> Result<String>;

        fn get(&self) -> Result<Self::Value> {
            self.parse(self.get_raw()?.as_str())
        }

        fn set(&self, value: &Self::Borrowed) -> Result {
            let value = self.generate(value)?;
            self.set_raw(value);
            Ok(())
        }

        fn set_workflow_env(
            &self,
            value: impl Borrow<Self::Borrowed>,
        ) -> BoxFuture<'static, Result> {
            let name = self.name().to_string();
            let value = self.generate(value.borrow());
            value
                .and_then_async(move |value| crate::actions::workflow::set_env(name, &value))
                .boxed()
        }
    }

    impl<Variable: TypedVariable, Value: AsRef<Variable::Borrowed>> FallibleManipulator
        for (Variable, Value)
    {
        fn try_applying<C: IsCommandWrapper + ?Sized>(&self, command: &mut C) -> Result {
            let value = self.0.generate(self.1.as_ref())?;
            command.env(self.0.name(), value);
            Ok(())
        }
    }

    #[derive(Clone, Copy, Debug, Display, Ord, PartialOrd, Eq, PartialEq)]
    pub struct PathBufVariable(pub &'static str);

    impl const From<&'static str> for PathBufVariable {
        fn from(value: &'static str) -> Self {
            PathBufVariable(value)
        }
    }

    impl RawVariable for PathBufVariable {
        fn name(&self) -> &str {
            self.0
        }
    }

    impl TypedVariable for PathBufVariable {
        type Value = PathBuf;
        type Borrowed = Path;
        fn parse(&self, value: &str) -> Result<Self::Value> {
            PathBuf::from_str(value)
        }
        fn generate(&self, value: &Self::Borrowed) -> Result<String> {
            value.to_str().context("Path is not a valid string.").map(ToString::to_string)
        }
    }

    #[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq, derive_more::Deref)]
    pub struct SimpleVariable<Value, Borrowed: ?Sized = Value> {
        #[deref]
        pub name:          &'static str,
        pub phantom_data:  PhantomData<Value>,
        pub phantom_data2: PhantomData<Borrowed>,
    }

    impl<Value, Borrowed: ?Sized> From<&'static str> for SimpleVariable<Value, Borrowed> {
        fn from(value: &'static str) -> Self {
            SimpleVariable::new(value)
        }
    }

    impl<Value, Borrowed: ?Sized> const AsRef<str> for SimpleVariable<Value, Borrowed> {
        fn as_ref(&self) -> &str {
            self.name
        }
    }

    impl<Value, Borrowed: ?Sized> From<&SimpleVariable<Value, Borrowed>> for String {
        fn from(value: &SimpleVariable<Value, Borrowed>) -> Self {
            value.name.to_string()
        }
    }

    impl<Value, Borrowed: ?Sized> From<SimpleVariable<Value, Borrowed>> for String {
        fn from(value: SimpleVariable<Value, Borrowed>) -> Self {
            value.name.to_string()
        }
    }

    impl<Value, Borrowed: ?Sized> SimpleVariable<Value, Borrowed> {
        pub const fn new(name: &'static str) -> Self {
            Self { name, phantom_data: PhantomData, phantom_data2: PhantomData }
        }
    }

    impl<Value, Borrowed: ?Sized> RawVariable for SimpleVariable<Value, Borrowed> {
        fn name(&self) -> &str {
            self.name
        }
    }

    impl<Value: FromString, Borrowed: ToString + ?Sized> TypedVariable
        for SimpleVariable<Value, Borrowed>
    {
        type Value = Value;
        type Borrowed = Borrowed;
        fn parse(&self, value: &str) -> Result<Self::Value> {
            Value::from_str(value)
        }
        fn generate(&self, value: &Self::Borrowed) -> Result<String> {
            Ok(Borrowed::to_string(value))
        }
    }

    impl<Value, Borrowed: ?Sized> Display for SimpleVariable<Value, Borrowed> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.name)
        }
    }

    #[derive(Clone, Copy, Debug, Display, Ord, PartialOrd, Eq, PartialEq)]
    pub struct PathLike(pub &'static str);

    impl RawVariable for PathLike {
        fn name(&self) -> &str {
            self.0
        }
    }

    impl TypedVariable for PathLike {
        type Value = Vec<PathBuf>;
        fn parse(&self, value: &str) -> Result<Self::Value> {
            Ok(std::env::split_paths(value).collect())
        }

        fn generate(&self, value: &Self::Value) -> Result<String> {
            std::env::join_paths(value)?
                .into_string()
                .map_err(|e| anyhow!("Not a valid UTF-8 string: '{}'.", e.to_string_lossy()))
        }
    }

    impl PathLike {
        pub fn prepend(&self, value: impl Into<PathBuf>) -> Result {
            let mut paths = self.get()?;
            paths.insert(0, value.into());
            self.set(&paths)
        }
    }

    /// Environment variable consisting of string separated by a given separator.
    #[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
    pub struct Separated {
        pub name:      &'static str,
        pub separator: &'static str,
    }

    impl RawVariable for Separated {
        fn name(&self) -> &str {
            self.name
        }
    }

    impl TypedVariable for Separated {
        type Value = Vec<String>;

        fn parse(&self, value: &str) -> Result<Self::Value> {
            Ok(value.split(self.separator).map(ToString::to_string).collect())
        }

        fn generate(&self, value: &Self::Borrowed) -> Result<String> {
            Ok(value.join(self.separator))
        }
    }
}

const PATH_ENVIRONMENT_NAME: &str = "PATH";


pub fn expect_var(name: impl AsRef<str>) -> Result<String> {
    let name = name.as_ref();
    std::env::var(name).with_context(|| anyhow!("Missing environment variable {name}."))
}

pub fn expect_var_os(name: impl AsRef<OsStr>) -> Result<OsString> {
    let name = name.as_ref();
    std::env::var_os(name)
        .with_context(|| anyhow!("Missing environment variable {}.", name.to_string_lossy()))
}

pub fn prepend_to_path(path: impl Into<PathBuf>) -> Result {
    let path = path.into();
    trace!("Prepending {} to {PATH_ENVIRONMENT_NAME}.", path.display());
    let old_value = std::env::var_os(PATH_ENVIRONMENT_NAME);
    let old_pieces = old_value.iter().flat_map(std::env::split_paths);
    let new_pieces = once(path).chain(old_pieces);
    let new_value = std::env::join_paths(new_pieces)?;
    std::env::set_var(PATH_ENVIRONMENT_NAME, new_value);
    Ok(())
}

// pub async fn fix_duplicated_env_var(var_name: impl AsRef<OsStr>) -> Result {
//     let var_name = var_name.as_ref();
//
//     let mut paths = indexmap::IndexSet::new();
//     while let Ok(path) = std::env::var(var_name) {
//         paths.extend(std::env::split_paths(&path));
//         std::env::remove_var(var_name);
//     }
//     std::env::set_var(var_name, std::env::join_paths(paths)?);
//     Ok(())
// }

#[derive(Clone, Debug)]
pub enum Action {
    Remove,
    Set(String),
    PrependPaths(Vec<PathBuf>),
}

#[derive(Clone, Debug)]
pub struct Modification {
    pub variable_name: UniCase<String>,
    pub action:        Action,
}

impl Modification {
    pub fn apply(&self) -> Result {
        let normalized_name = &*self.variable_name;
        match &self.action {
            Action::Remove => {
                debug!("Removing {}", self.variable_name);
                std::env::remove_var(normalized_name)
            }
            Action::Set(value) => {
                debug!("Setting {}={}", self.variable_name, value);
                std::env::set_var(normalized_name, value);
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
                    std::env::set_var(&*self.variable_name, new_value);
                } else {
                    let new_value = std::env::join_paths(paths_to_prepend)?;
                    std::env::set_var(&*self.variable_name, new_value);
                },
        };
        Ok(())
    }
}
