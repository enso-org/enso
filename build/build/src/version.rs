//! Code that deals with the version of Enso.

use crate::prelude::*;

use anyhow::Context;
use chrono::Datelike;
use derivative::Derivative;
use ide_ci::define_env_var;
use ide_ci::env::new::TypedVariable;
use ide_ci::github;
use octocrab::models::repos::Release;
use semver::Prerelease;
use std::collections::BTreeSet;
use strum::EnumIter;
use strum::EnumString;
use strum::IntoEnumIterator;
use tracing::instrument;



// Variable that stores Enso Engine version.
define_env_var! {
    /// The version of Enso (shared by GUI and Engine).
    ENSO_VERSION, Version;
    /// Edition name for the build.
    ///
    /// By convention, this is the same as the version.
    ENSO_EDITION, String;

    /// Whether the development-specific Engine features should be disabled.
    ENSO_RELEASE_MODE, bool;
}

pub const LOCAL_BUILD_PREFIX: &str = "dev";
pub const NIGHTLY_BUILD_PREFIX: &str = "nightly";
pub const RC_BUILD_PREFIX: &str = "rc";

/// Check if the given GitHub release matches the provided kind.
pub fn is_release_of_kind(release: &Release, kind: Kind) -> bool {
    match kind {
        Kind::Dev => release.tag_name.contains(LOCAL_BUILD_PREFIX),
        Kind::Nightly => release.tag_name.contains(NIGHTLY_BUILD_PREFIX),
        Kind::Rc => release.tag_name.contains(RC_BUILD_PREFIX),
        Kind::Stable => !release.prerelease,
    }
}

/// List all releases in the GitHub repository that are of a given kind.
pub async fn releases_of_kind(
    repo: &github::repo::Handle<impl IsRepo>,
    kind: Kind,
) -> Result<impl Iterator<Item = Release>> {
    Ok(repo.all_releases().await?.into_iter().filter(move |r| is_release_of_kind(r, kind)))
}

/// Get the latest nightly release in the GitHub repository.
pub async fn latest_nightly_release(repo: &github::repo::Handle<impl IsRepo>) -> Result<Release> {
    // TODO: this assumes that releases are returned in date order, to be confirmed
    //       (but having to download all the pages to see which is latest wouldn't be nice)
    releases_of_kind(repo, Kind::Nightly)
        .await?
        .next()
        .context("Failed to find any nightly releases.")
}

/// Keeps the version of Enso, edition name and whether this version should be treated as a release.
///
/// Basically this is everything that is needed to define the version of the build.
#[derive(Clone, Derivative, Serialize, Deserialize, Shrinkwrap, PartialEq, Eq)]
#[derivative(Debug)]
pub struct Versions {
    /// The version of Enso.
    ///
    /// Currently it also doubles as the edition name. In future we might want to separate them.
    #[shrinkwrap(main_field)]
    #[derivative(Debug(format_with = "std::fmt::Display::fmt"))]
    pub version: Version,

    /// Whether this version should be treated as a release.
    ///
    /// This is later propagated to [`ENSO_RELEASE_MODE`] environment variable.
    pub release_mode: bool,
}

impl Versions {
    /// Create a new version from a single SemVer [`Version`] value.
    ///
    /// Edition name will be deduced, to be the same as the version.
    /// Whether this version should be treated as a release is deduced from the version's
    /// [pre-release](https://semver.org/#spec-item-9) part.
    pub fn new(version: Version) -> Self {
        let release_mode = !version.pre.as_str().contains(LOCAL_BUILD_PREFIX)
            && !version.pre.as_str().contains("SNAPSHOT");
        Versions { version, release_mode }
    }

    /// Get the edition name.
    ///
    /// By convention, this is the same as the version.
    pub fn edition_name(&self) -> String {
        self.version.to_string()
    }

    /// Pretty print the product name and version, e.g. "Enso 2022.1.0".
    pub fn pretty_name(&self) -> String {
        format!("Enso {}", self.version)
    }

    pub fn local_prerelease() -> Result<Prerelease> {
        Prerelease::new(LOCAL_BUILD_PREFIX).anyhow_err()
    }

    pub async fn nightly_prerelease(
        repo: &github::repo::Handle<impl IsRepo>,
    ) -> Result<Prerelease> {
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

        let relevant_nightly_versions = releases_of_kind(repo, Kind::Nightly)
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

    /// Generate prerelease string for the "release candidate" release.
    ///
    /// We list all the RC releases in the repository, and increment the number of the latest one.
    pub async fn rc_prerelease(
        version: &Version,
        repo: &github::repo::Handle<impl IsRepo>,
    ) -> Result<Prerelease> {
        let relevant_rc_versions = releases_of_kind(repo, Kind::Rc)
            .await?
            .filter_map(|release| {
                let release_version = Version::parse(&release.tag_name).ok()?;
                let version_matches = release_version.major == version.major
                    && release_version.minor == version.minor
                    && release_version.patch == version.patch;
                version_matches.then_some(release_version.pre)
            })
            .collect::<BTreeSet<_>>();

        // Generate subsequent RC sub-releases, until a free one is found.
        // Should happen rarely.
        for index in 0.. {
            let pre = Prerelease::from_str(&format!("{}.{}", RC_BUILD_PREFIX, index))?;
            if !relevant_rc_versions.contains(&pre) {
                return Ok(pre);
            }
        }
        unreachable!("After infinite loop.")
    }

    /// Get a git tag that should be applied to a commit released as this version.
    pub fn tag(&self) -> String {
        self.version.to_string()
    }

    pub async fn publish(&self) -> Result {
        let edition = self.edition_name();
        // Some components (like SBT) consume version information through these environment
        // variables.
        ENSO_VERSION.set(&self.version)?;
        ENSO_EDITION.set(&edition)?;
        ENSO_RELEASE_MODE.set(&self.release_mode)?;

        // This is actually used only in some workflows (primarily the release one, where release
        // creation and the asset compilation happen in separate jobs). Still, no harm in doing this
        // always.
        //
        // Note that our output names are the same as the environment variable names.
        ide_ci::actions::workflow::set_output(ENSO_VERSION.name, &ENSO_VERSION.get_raw()?).await?;
        ide_ci::actions::workflow::set_output(ENSO_EDITION.name, &ENSO_EDITION.get_raw()?).await?;
        ide_ci::actions::workflow::set_output(
            ENSO_RELEASE_MODE.name,
            &ENSO_RELEASE_MODE.get_raw()?,
        )
        .await?;

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
    chrono::Utc::now().year() as u64
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
pub fn versions_from_env(expected_build_kind: Option<Kind>) -> Result<Option<Versions>> {
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
            let found_build_kind = Kind::deduce(&version)?;
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
pub async fn deduce_or_generate(
    repo: Result<&github::repo::Handle<impl IsRepo>>,
    kind: Kind,
    root_path: impl AsRef<Path>,
) -> Result<Versions> {
    debug!("Deciding on version to target.");
    if let Some(versions) = versions_from_env(Some(kind))? {
        Ok(versions)
    } else {
        let changelog_path = crate::paths::root_to_changelog(&root_path);
        let base_version = base_version(&changelog_path)?;
        let version = Version {
            pre: match kind {
                Kind::Dev => Versions::local_prerelease()?,
                Kind::Nightly => Versions::nightly_prerelease(repo?).await?,
                Kind::Rc => Versions::rc_prerelease(&base_version, repo?).await?,
                Kind::Stable => todo!(), //Versions::stable(repo?).await?,
            },
            ..base_version
        };
        Ok(Versions::new(version))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_nightly_test() {
        let is_nightly = |text: &str| Kind::Nightly.matches(&Version::parse(text).unwrap());
        assert!(is_nightly("2022.1.1-nightly.2022.1.1"));
        assert!(is_nightly("2022.1.1-nightly"));
        assert!(is_nightly("2022.1.1-nightly.2022.1.1"));
        assert!(is_nightly("2022.1.1-nightly.2022.1.1"));

        let version = Version::parse("2022.1.1-nightly.2022-06-06.3").unwrap();
        assert!(Kind::deduce(&version).contains(&Kind::Nightly));
    }

    #[test]
    #[ignore]
    fn iii() -> Result {
        dbg!(base_version(r"H:\nbo\enso\app\gui\changelog.md")?);
        Ok(())
    }
}

#[derive(
    clap::ArgEnum,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Debug,
    EnumString,
    EnumIter,
    strum::Display,
    strum::AsRefStr
)]
#[strum(serialize_all = "kebab-case")]
pub enum Kind {
    Dev,
    Nightly,
    Rc,
    Stable,
}

impl Kind {
    pub fn prerelease_prefix(self) -> &'static str {
        match self {
            Kind::Dev => LOCAL_BUILD_PREFIX,
            Kind::Nightly => NIGHTLY_BUILD_PREFIX,
            Kind::Rc => RC_BUILD_PREFIX,
            Kind::Stable => "",
        }
    }

    pub fn matches(self, version: &Version) -> bool {
        version.pre.as_str().starts_with(self.prerelease_prefix())
    }

    pub fn deduce(version: &Version) -> Result<Self> {
        Kind::iter()
            .find(|kind| kind.matches(version))
            .context(format!("Failed to deduce build kind for version {version}"))
    }
}
