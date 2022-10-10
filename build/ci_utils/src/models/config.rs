//! Model for the configuration file describing desired CI deployment.

use crate::prelude::*;

use crate::github::OrganizationPointer;
use crate::github::RepoPointer;
use crate::serde::regex_vec;
use crate::serde::single_or_sequence;

use regex::Regex;
use std::collections::HashMap;



pub type Config = BTreeMap<String, MachineConfig>;
/// Root type of the configuration file.
pub type MachineConfig = Vec<RepoConfig>;

/// Description of the runners deployment for a specific GitHub repository.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct RepoConfig {
    #[serde(flatten)]
    pub location: RunnerLocation,
    /// Runner names. Names are also added to their runners as a label.
    #[serde(deserialize_with = "single_or_sequence")]
    pub runners:  Vec<RunnerRepr>,
    /// Regular expressions that describe runner names that are externally managed.
    ///
    /// Such runners will not be removed when deploying managed runners.
    #[serde(default, with = "regex_vec")]
    pub external: Vec<Regex>,
}

impl RepoConfig {
    /// Check if a runner by given name should be considered as externally managed.
    pub fn is_external(&self, name: &str) -> bool {
        self.external.iter().any(|regex| regex.is_match(name))
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum RunnerLocation {
    Organization(OrganizationContext),
    Repository(RepoContext),
}

impl RunnerLocation {
    /// Generate a token that can be used to register a new runner for this repository.
    pub async fn generate_runner_registration_token(
        &self,
        octocrab: &Octocrab,
    ) -> anyhow::Result<crate::github::model::RegistrationToken> {
        match self {
            RunnerLocation::Organization(org) =>
                org.generate_runner_registration_token(octocrab).await,
            RunnerLocation::Repository(repo) =>
                repo.generate_runner_registration_token(octocrab).await,
        }
    }

    /// The runner's registration target URL.
    pub fn url(&self) -> anyhow::Result<Url> {
        match self {
            RunnerLocation::Organization(org) => org.url(),
            RunnerLocation::Repository(repo) => repo.url(),
        }
    }
}

/// Data denoting a specific GitHub organization.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct OrganizationContext {
    pub name: String,
}

impl OrganizationPointer for OrganizationContext {
    fn name(&self) -> &str {
        &self.name
    }
}

/// Data denoting a specific GitHub repository.
#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub struct RepoContext {
    /// Owner - an organization's or user's name.
    pub owner: String,
    pub name:  String,
}

impl RepoPointer for RepoContext {
    fn owner(&self) -> &str {
        &self.owner
    }

    fn name(&self) -> &str {
        &self.name
    }
}

impl Display for RepoContext {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", self.owner, self.name)
    }
}

/// Parse from strings in format "owner/name". Opposite of `Display`.
impl std::str::FromStr for RepoContext {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s.split('/').collect_vec().as_slice() {
            [owner, name] => Ok(Self { owner: owner.to_string(), name: name.to_string() }),
            slice => bail!("Failed to parse string '{}': Splitting by '/' should yield exactly 2 pieces, found: {}", s, slice.len()),
        }
    }
}

impl RepoContext {
    pub fn new(owner: impl Into<String>, name: impl Into<String>) -> Self {
        Self { owner: owner.into(), name: name.into() }
    }
}

/// Description of the runners deployment for a specific GitHub repository.
#[allow(clippy::large_enum_variant)] // We don't mind.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum RunnerRepr {
    Shorthand(String),
    Full(Runner),
}

impl RunnerRepr {
    pub fn resolve(&self) -> Runner {
        match self {
            Self::Shorthand(name) => Runner::new(name),
            RunnerRepr::Full(runner) => runner.clone(),
        }
    }
}

fn default_dockerfile() -> String {
    "Dockerfile".into()
}

fn default_target() -> String {
    "runner".into()
}

fn default_count() -> usize {
    1
}

/// Description of the runners deployment for a specific GitHub repository.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Runner {
    pub name:          String,
    #[serde(default = "default_dockerfile")]
    pub dockerfile:    String,
    #[serde(default = "default_target")]
    pub target:        String,
    #[serde(default)]
    pub labels:        Option<Vec<String>>,
    #[serde(default)]
    pub args:          Vec<String>,
    #[serde(default)]
    pub ports:         HashMap<u16, u16>,
    #[serde(default)]
    pub docker_access: bool,
    #[serde(default = "default_count")]
    pub count:         usize,
    #[serde(default)]
    pub volumes:       HashMap<PathBuf, PathBuf>,
    #[serde(default)]
    pub env:           HashMap<String, String>,
}

impl Runner {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name:          name.into(),
            dockerfile:    default_dockerfile(),
            target:        default_target(),
            labels:        default(),
            args:          default(),
            ports:         default(),
            docker_access: default(),
            count:         1,
            volumes:       default(),
            env:           default(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn deserialization() -> Result {
        let contents = r"
- repository:
    owner: enso-org
    name: ci
  runners:
  - name: metarunner
    target: metarunner
    docker_access: true";

        let _config = serde_yaml::from_str::<MachineConfig>(contents)?;
        Ok(())
    }
}
