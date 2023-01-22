use crate::prelude::*;

use crate::program::command::Manipulator;

#[derive(Clone, Debug, Copy)]
pub struct CMake;

impl Program for CMake {
    fn executable_name(&self) -> &str {
        "cmake"
    }
}

/// Defines the given variable in the CMake cache.
#[derive(Clone, Debug)]
pub struct SetVariable {
    /// Variable name.
    pub variable: String,
    /// Variable value.
    pub value:    String,
}

impl SetVariable {
    fn new(variable: impl Into<String>, value: impl Into<String>) -> Self {
        let variable = variable.into();
        let value = value.into();
        Self { variable, value }
    }

    pub fn option(name: impl Into<String>, value: bool) -> Self {
        Self::new(name, if value { "ON" } else { "OFF" })
    }

    pub fn filepath(name: impl Into<String>, value: impl AsRef<Path>) -> Self {
        Self::new(name, value.as_ref().as_str())
    }

    pub fn path(name: impl Into<String>, value: impl AsRef<Path>) -> Self {
        Self::new(name, value.as_ref().as_str())
    }
}

impl Manipulator for SetVariable {
    fn apply<C: IsCommandWrapper + ?Sized>(&self, command: &mut C) {
        command.arg("-D").arg(format!("{}={}", self.variable, self.value));
    }
}
