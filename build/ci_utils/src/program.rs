use crate::prelude::*;

use crate::program::command::MyCommand;

use location::Location;


// ==============
// === Export ===
// ==============

pub mod command;
pub mod location;
pub mod resolver;
pub mod shell;
pub mod version;
pub mod with_cwd;

pub use command::Command;
pub use resolver::Resolver;
pub use shell::Shell;



// TODO: consider project manger wrapper:
// TODO: separate locating (which might be stateful, e.g. with additional directories)
// TODO: separate "what can be done with its command" from the rest of program (e.g. from name)

pub const EMPTY_ARGS: [&str; 0] = [];



/// A set of utilities for using a known external program.
///
/// The trait covers program lookup and process management.
// `Sized + 'static` bounds are due to using `Self` as type parameter for `Command` constructor.
#[async_trait]
pub trait Program: Sized + 'static {
    type Command: MyCommand<Self> + Send + Sync + IsCommandWrapper = Command;

    type Version: version::IsVersion = Version;

    /// The name used to find and invoke the program.
    ///
    /// This should just the stem name, not a full path. The os-specific executable extension should
    /// be skipped.
    fn executable_name(&self) -> &str;

    /// If program can be found under more than one name, additional names are provided.
    ///
    /// The primary name is provided by ['executable_name'].
    fn executable_name_fallback() -> Vec<&'static str> {
        vec![]
    }

    /// Additional directories that will be treated as-if appended to PATH.
    fn default_locations(&self) -> Vec<PathBuf> {
        Vec::new()
    }

    fn pretty_name(&self) -> &str {
        self.executable_name()
    }

    /// Locate the program executable.
    ///
    /// The lookup locations are program-defined, they typically include Path environment variable
    /// and program-specific default locations.
    fn lookup(&self) -> anyhow::Result<Location<Self>> {
        Resolver::<Self>::new(self.executable_names(), self.default_locations())?
            .lookup()
            .map(Location::new)
    }

    fn require_present(&self) -> BoxFuture<'static, Result<String>> {
        let executable_name = self.executable_name().to_owned();
        let get_version_string = self.version_string();
        async move {
            let version = get_version_string.await?;
            debug!("Found {}: {}", executable_name, version);
            Ok(version)
        }
        .boxed()
    }

    async fn require_present_at(&self, required_version: &Self::Version) -> Result {
        let found_version = self.version().await?;
        if &found_version != required_version {
            bail!(
                "Failed to find {} in version == {}. Found version: {}",
                self.executable_name(),
                required_version,
                found_version
            )
        }
        Ok(())
    }

    fn cmd(&self) -> Result<Self::Command> {
        let program_path = self.lookup()?;
        let mut command = Self::Command::new_program(program_path);
        if let Some(current_dir) = self.current_directory() {
            command.borrow_mut().current_dir(current_dir);
        }
        self.init_command(&mut command);
        Ok(command)
    }

    fn init_command<'a>(&self, cmd: &'a mut Self::Command) -> &'a mut Self::Command {
        cmd
    }

    fn current_directory(&self) -> Option<PathBuf> {
        None
    }

    fn handle_exit_status(status: std::process::ExitStatus) -> Result {
        status.exit_ok().anyhow_err()
    }

    /// Command that prints to stdout the version of given program.
    ///
    /// If this is anything other than `--version` the implementor should overwrite this method.
    fn version_command(&self) -> Result<Self::Command> {
        let mut cmd = self.cmd()?;
        cmd.borrow_mut().arg("--version");
        Ok(cmd)
    }

    fn version_string(&self) -> BoxFuture<'static, Result<String>> {
        let command = self.version_command();
        async move {
            let output = command?.borrow_mut().run_stdout().await?;
            Ok(output.trim().to_string())
        }
        .boxed()
    }

    async fn version(&self) -> Result<Self::Version> {
        let stdout = self.version_string().await?;
        self.parse_version(&stdout)
    }

    /// Retrieve semver-compatible version from the string in format provided by the
    /// `version_string`.
    ///
    /// Some programs do not follow semver for versioning, for them this method is unspecified.
    fn parse_version(&self, version_text: &str) -> Result<Self::Version> {
        Self::Version::find_in_text(version_text)
    }
}

#[async_trait]
pub trait ProgramExt: Program {
    async fn require_present_that(
        &self,
        required_version: impl version::IsVersionPredicate<Version = Self::Version>,
    ) -> Result {
        let found_version = self.version().await?;
        ensure!(
            required_version.matches(&found_version),
            "Failed to find {} in version that satisfied requirement {}. Found version: {}",
            self.executable_name(),
            required_version,
            found_version
        );
        Ok(())
    }

    fn executable_names(&self) -> Vec<&str> {
        let mut ret = vec![self.executable_name()];
        ret.extend(Self::executable_name_fallback());
        ret
    }

    fn args(&self, args: impl IntoIterator<Item: AsRef<OsStr>>) -> Result<Self::Command> {
        let mut cmd = self.cmd()?;
        cmd.borrow_mut().args(args);
        Ok(cmd)
    }
}

impl<T> ProgramExt for T where T: Program {}


///

#[derive(Clone, Debug)]
pub struct Unknown(pub String);
impl Program for Unknown {
    fn executable_name(&self) -> &str {
        &self.0
    }
}

pub fn lookup(executable_name: impl AsRef<str>) -> Result<PathBuf> {
    Resolver::<()>::new(vec![executable_name.as_ref()], vec![])?.lookup()
}
