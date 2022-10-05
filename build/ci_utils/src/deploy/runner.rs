use crate::prelude::*;

use crate::models::config::Runner;
use crate::models::config::RunnerLocation;

use platforms::target::OS;
use std::collections::BTreeSet;



/// Name of the directory with a runner that is placed in runner's container image build context.
///
/// Must be in sync with relevant entries in `Dockerfile`s (the `ADD` commands).
pub const DIRECTORY_WITH_RUNNER_PACKAGE: &str = "runner";

pub const DIRECTORY_WITH_CI_CRATE: &str = "ci";

/// Full runner configuration.
#[derive(Clone, Debug)]
pub struct Config {
    /// Repository where this runner is registered.
    pub location:    RunnerLocation,
    /// Runner's name.
    pub runner:      Runner,
    /// Operating system of the runner's image. It is possible to have Linux on Windows or macOS,
    /// so we don't assume this to be always equal to `TARGET_OS`.
    pub os:          OS,
    pub server_name: String,
    pub index:       usize,
}

impl Config {
    /// Pretty printed triple with repository owner, repository name and runner name.
    pub fn qualified_name(&self) -> String {
        let location_prefix = match &self.location {
            RunnerLocation::Organization(org) => iformat!("{org.name}"),
            RunnerLocation::Repository(repo) => iformat!("{repo.owner}-{repo.name}"),
        };
        iformat!("{location_prefix}-{self.runner.name}-{self.server_name}-{self.index}")
    }

    /// The custom labels that the runner will be registered with.
    ///
    /// Apart from them, the GH-defined labels are always used.
    pub fn custom_labels(&self) -> BTreeSet<String> {
        once(self.runner.name.clone())
            .chain(once(self.server_name.clone()))
            .chain(once(self.qualified_name()))
            .chain(once(format!("index-{}", self.index)))
            .chain(self.runner.labels.as_ref().into_iter().flatten().cloned())
            .collect()
    }

    /// The list of custom labels pretty printed in the format expected by the `--labels` argument
    /// of the runner's configure script.
    pub fn registered_labels_arg(&self) -> OsString {
        self.custom_labels().into_iter().join(",").into()
    }

    pub fn registered_name(&self) -> String {
        format!("{}-{}-{}", &self.runner.name, self.server_name, self.index)
    }

    pub fn register_script_call_args(
        &self,
        token: impl AsRef<str>,
    ) -> Result<impl IntoIterator<Item = String>> {
        let url = self.location.url()?;
        let name = self.registered_name();
        let labels = self.registered_labels_arg();
        Ok([
            "--unattended",
            "--replace",
            "--name",
            name.as_str(),
            "--url",
            url.as_str(),
            "--token",
            token.as_ref(),
            "--labels",
            labels.as_str(),
        ]
        .map(into))
    }

    pub fn guest_root_path(&self) -> PathBuf {
        if self.os == OS::Windows { r"C:\" } else { "/" }.into()
    }

    pub fn guest_runner_dir(&self) -> PathBuf {
        self.guest_root_path().join(DIRECTORY_WITH_RUNNER_PACKAGE)
    }

    pub fn guest_ci_dir(&self) -> PathBuf {
        self.guest_root_path().join(DIRECTORY_WITH_CI_CRATE)
    }

    pub fn guest_config_script_path(&self) -> PathBuf {
        self.guest_runner_dir().join(self.config_script_filename())
    }

    pub fn guest_run_script_path(&self) -> PathBuf {
        let mut ret = self.guest_runner_dir().join("run");
        ret.set_extension(script_extension(self.os));
        ret
    }

    pub fn config_script_filename(&self) -> PathBuf {
        let mut ret = PathBuf::from("config");
        ret.set_extension(script_extension(self.os));
        ret
    }
}

/// The extension used by the scripts that are part of GitHub Actions Runner distribution.
pub fn script_extension(os: OS) -> &'static str {
    if os == OS::Windows {
        "cmd"
    } else {
        "sh"
    }
}
