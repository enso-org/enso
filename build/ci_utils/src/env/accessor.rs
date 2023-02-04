use crate::prelude::*;

use crate::env::expect_var;
use crate::env::expect_var_os;
use crate::program::command::FallibleManipulator;



/// An environment variable of known name.
///
/// "raw" means that we do not know its "real" type and deal only with strings. When more type
/// safety is needed, implement `TypedVariable` as well.
pub trait RawVariable {
    /// The name of this environment variable.
    fn name(&self) -> &str;

    /// Check if this variable has been set.
    ///
    /// Note that a variable may be set to the empty string. This can lead to unexpected
    /// results, because in some environments variables can be unset by setting them to the
    /// empty string.
    fn is_set(&self) -> bool {
        std::env::var(self.name()) != Err(std::env::VarError::NotPresent)
    }

    /// Get the raw text value of this variable.
    fn get_raw(&self) -> Result<String> {
        expect_var(self.name())
    }

    /// Get the raw text value of this variable in the form of [`OsString`].
    fn get_raw_os(&self) -> Result<OsString> {
        expect_var_os(self.name())
    }

    /// Set the raw text value of this variable.
    fn set_raw(&self, value: impl AsRef<OsStr>) {
        crate::env::set_var(self.name(), value);
    }

    /// Remove (i.e. unset) this variable.
    fn remove(&self) {
        std::env::remove_var(self.name());
    }
}

/// An environment variable of known name and type.
///
/// This trait can be used to define typed accessors for environment variables.
pub trait TypedVariable: RawVariable {
    /// The type of this variable.
    type Value;

    /// The borrowed type of this variable.
    type Borrowed: ?Sized = Self::Value;

    /// Construct a value of this variable by parsing the raw text value.
    fn parse(&self, value: &str) -> Result<Self::Value>;

    /// Pretty-print a value of this variable to the raw string.
    fn generate(&self, value: &Self::Borrowed) -> Result<String>;

    /// Get the value of this variable.
    fn get(&self) -> Result<Self::Value> {
        self.parse(self.get_raw()?.as_str())
    }

    /// Set the value of this variable.
    fn set(&self, value: &Self::Borrowed) -> Result {
        let value = self.generate(value)?;
        self.set_raw(value);
        Ok(())
    }

    /// Set the value of this variable, and make it persistent within the current CI job.
    ///
    /// When used in non-CI environments, just sets the variable and outputs log.
    fn set_workflow_env(&self, value: impl Borrow<Self::Borrowed>) -> BoxFuture<'static, Result> {
        let name = self.name().to_string();
        let value = self.generate(value.borrow());
        value.and_then_async(move |value| crate::actions::workflow::set_env(name, &value)).boxed()
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

/// Wrapper for the environment variable which values are represented as [`PathBuf`].
///
/// We cannot use generic wrapper, as [`PathBuf`] is not [`Display`].
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
        value
            .to_str()
            .with_context(|| format!("Path is not a valid string: {value:?}."))
            .map(ToString::to_string)
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
    #[context("Failed to prepend path `{}` to `{}`.", value.as_ref().display(), self.name())]
    pub fn prepend(&self, value: impl AsRef<Path>) -> Result {
        let value = value.as_ref().to_path_buf();
        trace!("Prepending {} to {}.", value.display(), self.name());
        let mut paths = self.get()?;
        paths.insert(0, value);
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
