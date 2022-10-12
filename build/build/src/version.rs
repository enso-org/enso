use crate::prelude::*;

use anyhow::Context;
use chrono::Datelike;
use derivative::Derivative;
use ide_ci::define_env_var;
use ide_ci::env::new::TypedVariable;
use ide_ci::models::config::RepoContext;
use octocrab::models::repos::Release;
use semver::Prerelease;
use std::collections::BTreeSet;
use strum::EnumIter;
use strum::EnumString;
use strum::IntoEnumIterator;
use tracing::instrument;



// Variable that stores Enso Engine version.
define_env_var! {
    ENSO_VERSION, Version;
    ENSO_EDITION, String;
    ENSO_RELEASE_MODE, bool;
}

pub const LOCAL_BUILD_PREFIX: &str = "dev";
pub const NIGHTLY_BUILD_PREFIX: &str = "nightly";

pub fn default_dev_version() -> Version {
    let mut ret = Version::new(0, 0, 0);
    ret.pre = Prerelease::new(LOCAL_BUILD_PREFIX).unwrap();
    ret
}

pub fn is_nightly_release(release: &Release) -> bool {
    !release.draft && release.tag_name.contains(NIGHTLY_BUILD_PREFIX)
}

pub async fn nightly_releases(
    octocrab: &Octocrab,
    repo: &RepoContext,
) -> Result<impl Iterator<Item = Release>> {
    Ok(repo.all_releases(octocrab).await?.into_iter().filter(is_nightly_release))
}

pub async fn latest_nightly_release(octocrab: &Octocrab, repo: &RepoContext) -> Result<Release> {
    // TODO: this assumes that releases are returned in date order, to be confirmed
    //       (but having to download all the pages to see which is latest wouldn't be nice)
    nightly_releases(octocrab, repo).await?.next().context("Failed to find any nightly releases.")
}


#[derive(Clone, Derivative, Serialize, Deserialize, Shrinkwrap, PartialEq, Eq)]
#[derivative(Debug)]
pub struct Versions {
    #[shrinkwrap(main_field)]
    #[derivative(Debug(format_with = "std::fmt::Display::fmt"))]
    pub version:      Version,
    pub release_mode: bool,
}

impl Default for Versions {
    fn default() -> Self {
        Versions { version: default_dev_version(), release_mode: false }
    }
}

impl Versions {
    pub fn new(version: Version) -> Self {
        let release_mode = !version.pre.as_str().contains(LOCAL_BUILD_PREFIX)
            && !version.pre.as_str().contains("SNAPSHOT");
        Versions { version, release_mode }
    }

    pub fn edition_name(&self) -> String {
        self.version.to_string()
    }

    pub fn pretty_name(&self) -> String {
        format!("Enso {}", self.version)
    }

    pub fn local_prerelease() -> Result<Prerelease> {
        Prerelease::new(LOCAL_BUILD_PREFIX).anyhow_err()
    }

    pub async fn nightly_prerelease(octocrab: &Octocrab, repo: &RepoContext) -> Result<Prerelease> {
        let date = chrono::Utc::now();
        let date = date.format("%F").to_string();

        let todays_pre_text = format!("{}.{}", NIGHTLY_BUILD_PREFIX, date);
        let generate_ith = |index: u32| -> Result<Prerelease> {
            let pre = if index == 0 {
                Prerelease::from_str(&todays_pre_text)?
            } else {
                Prerelease::from_str(&format!("{}.{}", todays_pre_text, index))?
            };
            Ok(pre)
        };

        let relevant_nightly_versions = nightly_releases(octocrab, repo)
            .await?
            .filter_map(|release| {
                if release.tag_name.contains(&todays_pre_text) {
                    let version = Version::parse(&release.tag_name).ok()?;
                    Some(version.pre)
                } else {
                    None
                }
            })
            .collect::<BTreeSet<_>>();

        // Generate subsequent tonight nightly subreleases, until a free one is found.
        // Should happen rarely.
        for index in 0.. {
            let pre = generate_ith(index)?;
            if !relevant_nightly_versions.contains(&pre) {
                return Ok(pre);
            }
        }
        unreachable!("After infinite loop.")
    }

    pub fn tag(&self) -> String {
        self.version.to_string()
    }

    pub fn publish(&self) -> Result {
        let edition = self.edition_name();
        ENSO_VERSION.emit_to_workflow(&self.version)?;
        ENSO_EDITION.emit_to_workflow(edition.as_str())?;
        ENSO_RELEASE_MODE.emit_to_workflow(&self.release_mode)?;
        Ok(())
    }
}

#[context("Deducing version using changelog file: {}", changelog_path.as_ref().display())]
pub fn base_version(changelog_path: impl AsRef<Path>) -> Result<Version> {
    if let Ok(from_env) = ENSO_VERSION.get() {
        return Ok(from_env);
    }

    let changelog_contents = ide_ci::fs::read_to_string(changelog_path.as_ref())?;
    let mut headers = crate::changelog::Changelog(&changelog_contents)
        .iterate_headers()
        .map(|h| Version::find_in_text(h.text));

    let version = match headers.next() {
        None => generate_initial_version(),
        Some(Ok(top_version)) => top_version,
        Some(Err(_top_non_version_thingy)) => match headers.next() {
            Some(Ok(version)) => suggest_next_version(&version),
            None => generate_initial_version(),
            Some(Err(_)) => bail!("Two leading release headers have no version number in them."),
        },
    };
    Ok(version)
}

pub fn current_year() -> u64 {
    chrono::Utc::today().year() as u64
}

pub fn generate_initial_version() -> Version {
    Version::new(current_year(), 1, 1)
}

pub fn suggest_next_version(previous: &Version) -> Version {
    let year = current_year();
    if previous.major == year {
        Version::new(year, previous.minor + 1, 1)
    } else {
        generate_initial_version()
    }
}

#[instrument(ret)]
pub fn versions_from_env(expected_build_kind: Option<BuildKind>) -> Result<Option<Versions>> {
    if let Ok(version) = ENSO_VERSION.get() {
        // The currently adopted version scheme uses same string for version and edition name,
        // so we enforce it here. There are no fundamental reasons for this requirement.
        if let Ok(edition) = ENSO_EDITION.get() {
            ensure!(
                version.to_string() == edition,
                "Inconsistent {} and {} variable values.",
                ENSO_VERSION.name,
                ENSO_EDITION.name
            );
        }
        if let Some(expected_build_kind) = expected_build_kind {
            let found_build_kind = BuildKind::deduce(&version)?;
            ensure!(
                found_build_kind == expected_build_kind,
                "Build kind mismatch. Found: {}, expected: {}.",
                found_build_kind,
                expected_build_kind
            )
        }
        let versions = Versions::new(version);
        Ok(Some(versions))
    } else {
        Ok(None)
    }
}

#[instrument(skip_all, ret)]
pub async fn deduce_versions(
    octocrab: &Octocrab,
    build_kind: BuildKind,
    target_repo: Result<&RepoContext>,
    root_path: impl AsRef<Path>,
) -> Result<Versions> {
    debug!("Deciding on version to target.");
    if let Some(versions) = versions_from_env(Some(build_kind))? {
        Ok(versions)
    } else {
        let changelog_path = crate::paths::root_to_changelog(&root_path);
        let version = Version {
            pre: match build_kind {
                BuildKind::Dev => Versions::local_prerelease()?,
                BuildKind::Nightly => Versions::nightly_prerelease(octocrab, target_repo?).await?,
            },
            ..base_version(&changelog_path)?
        };
        Ok(Versions::new(version))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_nightly_test() {
        let is_nightly = |text: &str| BuildKind::Nightly.matches(&Version::parse(text).unwrap());
        assert!(is_nightly("2022.1.1-nightly.2022.1.1"));
        assert!(is_nightly("2022.1.1-nightly"));
        assert!(is_nightly("2022.1.1-nightly.2022.1.1"));
        assert!(is_nightly("2022.1.1-nightly.2022.1.1"));

        let version = Version::parse("2022.1.1-nightly.2022-06-06.3").unwrap();
        assert!(BuildKind::deduce(&version).contains(&BuildKind::Nightly));
    }

    #[test]
    #[ignore]
    fn iii() -> Result {
        dbg!(base_version(r"H:\nbo\enso\app\gui\changelog.md")?);
        Ok(())
    }
}

#[derive(clap::ArgEnum, Clone, Copy, PartialEq, Eq, Debug, EnumString, EnumIter, strum::Display)]
#[strum(serialize_all = "kebab-case")]
pub enum BuildKind {
    Dev,
    Nightly,
}

impl BuildKind {
    pub fn prerelease_prefix(self) -> &'static str {
        match self {
            BuildKind::Dev => LOCAL_BUILD_PREFIX,
            BuildKind::Nightly => NIGHTLY_BUILD_PREFIX,
        }
    }

    pub fn matches(self, version: &Version) -> bool {
        version.pre.as_str().starts_with(self.prerelease_prefix())
    }

    pub fn deduce(version: &Version) -> Result<Self> {
        BuildKind::iter()
            .find(|kind| kind.matches(version))
            .context(format!("Failed to deduce build kind for version {version}"))
    }
}
