//! Wrappers for [CMake program](https://cmake.org/) and its commands.

use crate::prelude::*;

use crate::program::command::Manipulator;



// =====================
// === Configuration ===
// =====================

/// Standard build configurations defined by CMake.
///
/// See [CMake documentation](https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#default-and-custom-configurations)
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq)]
pub enum Configuration {
    /// Non-optimized, debuggable binary.
    Debug,
    /// Optimized binary.
    Release,
    /// Add compiler flags for generating debug information (the -g flag for GCC / clang), and will
    /// result in debuggable, yet much larger binaries.
    RelWithDebInfo,
    /// Add compiler flags for generating more compact binaries (the -Os flag for GCC / clang),
    /// possibly on the expense of program speed.
    MinSizeRel,
}


// ===================
// === CLI Options ===
// ===================

/// Define build type for a single configuration generator.
///
/// See <https://cmake.org/cmake/help/latest/variable/CMAKE_BUILD_TYPE.html>.
pub fn build_type(config: Configuration) -> SetVariable {
    SetVariable::string("CMAKE_BUILD_TYPE", config.to_string())
}

/// Option that can be passed to `cmake --build` command.
#[derive(Clone, Copy, Debug)]
pub enum BuildOption {
    /// The maximum number of concurrent processes to use when building. If value is omitted the
    /// native build tool's default number is used.
    Parallel(Option<u32>),
    /// For multi-configuration tools, choose configuration.
    ///
    /// Single configuration tools will ignore this option, instead [`build_type`] should be used
    /// during the generation phase.
    Configuration(Configuration),
}

impl BuildOption {
    /// Enable default level of parallelism.
    pub fn parallel() -> Self {
        Self::Parallel(None)
    }
}

impl Manipulator for BuildOption {
    fn apply<C: IsCommandWrapper + ?Sized>(&self, command: &mut C) {
        match self {
            BuildOption::Parallel(jobs) => {
                command.arg("--parallel");
                if let Some(jobs) = jobs {
                    command.arg(jobs.to_string());
                }
            }
            BuildOption::Configuration(config) => {
                command.arg("--config");
                command.arg(config.to_string());
            }
        }
    }
}

/// Options for `cmake --install` family of commands.
#[derive(Clone, Debug)]
pub enum InstallOption {
    /// Install to the given directory.
    Prefix(PathBuf),
}

impl Manipulator for InstallOption {
    fn apply<C: IsCommandWrapper + ?Sized>(&self, command: &mut C) {
        match self {
            InstallOption::Prefix(path) => {
                command.arg("--prefix");
                command.arg(path);
            }
        }
    }
}

/// Defines the given variable in the CMake cache.
#[derive(Clone, Debug)]
pub struct SetVariable {
    /// Variable name.
    pub variable: String,
    /// Variable value.
    value:        String,
}

impl SetVariable {
    fn new(variable: impl Into<String>, value: impl Into<String>) -> Self {
        let variable = variable.into();
        let value = value.into();
        Self { variable, value }
    }

    /// Set given boolean option variable (`BOOL` type).
    pub fn option(name: impl Into<String>, value: bool) -> Self {
        Self::new(name, if value { "ON" } else { "OFF" })
    }

    /// Set given file path variable (`FILEPATH` type).
    pub fn file_path(name: impl Into<String>, value: impl AsRef<Path>) -> Self {
        Self::new(name, value.as_ref().as_str())
    }

    /// Set directory path variable (`PATH` type).
    pub fn directory_path(name: impl Into<String>, value: impl AsRef<Path>) -> Self {
        Self::new(name, value.as_ref().as_str())
    }

    /// Set given string variable (`STRING` type).
    pub fn string(name: impl Into<String>, value: impl Into<String>) -> Self {
        Self::new(name, value)
    }
}

impl Manipulator for SetVariable {
    fn apply<C: IsCommandWrapper + ?Sized>(&self, command: &mut C) {
        command.arg("-D").arg(format!("{}={}", self.variable, self.value));
    }
}

// ===============
// === Program ===
// ===============

/// The [CMake program](https://cmake.org/).
///
/// See [the official CLI documentation](https://cmake.org/cmake/help/latest/manual/cmake.1.html).
#[derive(Clone, Debug, Copy)]
pub struct CMake;

impl Program for CMake {
    fn executable_name(&self) -> &str {
        "cmake"
    }
}

// ===========================
// === Helper Entry Points ===
// ===========================

/// Generate build files for the given project.
#[context("Failed to generate build files for {}.", source_dir.as_ref().display())]
pub fn generate(source_dir: impl AsRef<Path>, build_dir: impl AsRef<Path>) -> Result<Command> {
    Ok(CMake.cmd()?.with_arg(source_dir.as_ref()).with_current_dir(build_dir.as_ref()))
}

/// Build the project. The build_dir must be the same as the one used in the [generation
/// step](generate).
pub fn build(build_dir: impl AsRef<Path>) -> Result<Command> {
    Ok(CMake
        .cmd()?
        .with_arg("--build")
        .with_arg(".")
        .with_applied(&BuildOption::parallel())
        .with_current_dir(&build_dir))
}

/// Install the project. The build_dir must be the same as the one used in the [generation
/// step](generate), and [`build`] should have been called before.
pub fn install(build_dir: impl AsRef<Path>, prefix_dir: impl AsRef<Path>) -> Result<Command> {
    Ok(CMake
        .cmd()?
        .with_arg("--install")
        .with_arg(".")
        .with_applied(&InstallOption::Prefix(prefix_dir.as_ref().to_path_buf()))
        .with_current_dir(&build_dir))
}
