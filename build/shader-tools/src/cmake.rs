use crate::prelude::*;
use ide_ci::program::command::Manipulator;

pub struct CMake;

impl Program for CMake {
    fn executable_name(&self) -> &str {
        "cmake"
    }
}

/// Set the given variable in the CMake cache.
pub struct SetVariable {
    pub variable: String,
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
