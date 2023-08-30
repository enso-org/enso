use crate::prelude::*;

use crate::new_command_type;
use crate::program::command::Manipulator;
use crate::programs::Cargo;

use tempfile::TempDir;



/// What kind of Cargo build profile should be used.
///
/// Typically affects optimization, debug symbol generation and so.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Display, strum::AsRefStr)]
#[strum(serialize_all = "kebab-case")]
pub enum Profile {
    Dev,
    Release,
    Profile,
}

impl AsRef<OsStr> for Profile {
    fn as_ref(&self) -> &OsStr {
        OsStr::new(match self {
            Profile::Dev => "--dev",
            Profile::Release => "--release",
            Profile::Profile => "--profile",
        })
    }
}

#[derive(Clone, Copy, Debug, Display)]
pub enum Target {
    Bundler,
    NodeJs,
    Web,
    NoModules,
}

impl AsRef<OsStr> for Target {
    fn as_ref(&self) -> &OsStr {
        OsStr::new(match self {
            Target::Bundler => "bundler",
            Target::NodeJs => "nodejs",
            Target::Web => "web",
            Target::NoModules => "no-modules",
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, strum::AsRefStr)]
#[strum(serialize_all = "kebab-case")]
pub enum TestFlags {
    Chrome,
    Firefox,
    Headless,
    Node,
    Release,
    Safari,
}

impl Manipulator for TestFlags {
    fn apply<C: IsCommandWrapper + ?Sized>(&self, command: &mut C) {
        command.arg(format!("--{}", self.as_ref()));
    }
}

#[derive(Clone, Copy, Debug)]
pub struct WasmPack;

impl Program for WasmPack {
    type Command = WasmPackCommand;
    fn executable_name(&self) -> &'static str {
        "wasm-pack"
    }
}


new_command_type! {WasmPack, WasmPackCommand}

impl WasmPackCommand {
    pub fn build(&mut self) -> &mut Self {
        self.arg("build")
    }

    pub fn test(&mut self) -> &mut Self {
        self.arg("test")
    }

    pub fn target(&mut self, target: Target) -> &mut Self {
        self.arg("--target").arg(target)
    }

    /// Sets the output directory with a relative path.
    pub fn output_directory(&mut self, output_path: impl AsRef<Path>) -> &mut Self {
        self.arg("--out-dir").arg(output_path.as_ref())
    }

    /// Sets the output file names. Defaults to package name.
    pub fn output_name(&mut self, output_name: impl AsRef<Path>) -> &mut Self {
        self.arg("--out-name").arg(output_name.as_ref())
    }

    /// Enable wasm-bindgen weak references feature.
    /// https://rustwasm.github.io/wasm-bindgen/reference/weak-references.html
    pub fn weak_refs(&mut self) -> &mut Self {
        self.arg("--weak-refs")
    }

    /// Enable wasm-bindgen reference types feature.
    /// https://rustwasm.github.io/wasm-bindgen/reference/reference-types.html
    pub fn reference_types(&mut self) -> &mut Self {
        self.arg("--reference-types")
    }
}

// new_command_type! {WasmPack, WasmPackBuildCommand}

pub async fn install_if_missing() -> Result {
    let temp = TempDir::new()?;
    // We want to run this command in a temporary directory, as to install wasm-pack using a
    // system-wide default toolchain, rather than overrides for the current folder (which is likely
    // under our repository root).
    //
    // Note that this will install the tool to the default system-wide location, not temp.
    if WasmPack.lookup().is_err() {
        Cargo.cmd()?.args(["install", "wasm-pack"]).current_dir(temp.path()).run_ok().await?;
        // TODO
        //  this kind of function likely could use some generalization, that should also cover how
        //  PATH updates are handled
    }
    Ok(())
}
