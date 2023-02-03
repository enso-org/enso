//! Code that deals with the version of Enso.
//!
//! Enso uses [Semantic Versioning 2.0.0](https://semver.org/). By the convention the major version
//! number is fixed to the year of the release. The minor version number is incremented for each
//! (backwards incompatible) release.

use crate::prelude::*;

use anyhow::Context;
use chrono::Datelike;
use derivative::Derivative;
use ide_ci::define_env_var;
use ide_ci::env::accessor::TypedVariable;
use ide_ci::github;
use octocrab::models::repos::Release;
use semver::Prerelease;
use strum::EnumIter;
use strum::EnumString;
use strum::IntoEnumIterator;
use tracing::instrument;


// ==============
// === Export ===
// ==============

pub mod nightly;
pub mod promote;



// Variable that stores Enso Engine version.
// They are also used by the SBT part of the build.
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
    matches!(release.tag_name.parse2(), Ok(version) if kind.matches(&version))
}

/// List all releases in the GitHub repository that are of a given kind.
pub async fn releases_of_kind(
    repo: &github::repo::Handle<impl IsRepo>,
    kind: Kind,
) -> Result<impl Iterator<Item = Release>> {
    Ok(repo.all_releases().await?.into_iter().filter(move |r| is_release_of_kind(r, kind)))
}

/// Get the latest nightly release in the GitHub repository.
#[context("Failed to get the latest nightly release in {repo}.")]
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
#[derive(Clone, Derivative, Serialize, Deserialize, Deref, PartialEq, Eq)]
#[derivative(Debug)]
pub struct Versions {
    /// The version of Enso.
    ///
    /// Currently it also doubles as the edition name. In future we might want to separate them.
    #[deref]
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

    let year = current_year();
    let version = match headers.next() {
        None => generate_initial_version(year),
        Some(Ok(top_version)) => top_version,
        Some(Err(_top_non_version_thingy)) => match headers.next() {
            Some(Ok(version)) => suggest_next_version(&version, year),
            None => generate_initial_version(year),
            Some(Err(_)) => bail!("Two leading release headers have no version number in them."),
        },
    };
    Ok(version)
}

pub fn current_year() -> u64 {
    chrono::Utc::now().year() as u64
}

pub fn generate_initial_version(current_year: u64) -> Version {
    Version::new(current_year, 1, 1)
}

pub fn suggest_next_version(previous: &Version, current_year: u64) -> Version {
    if previous.major == current_year {
        Version::new(current_year, previous.minor + 1, 1)
    } else {
        generate_initial_version(current_year)
    }
}

/// Generate a next RC version for the given version.
///
/// ```
/// use enso_build::prelude::*;
/// use enso_build::version::increment_rc_version;
/// let rc_version = Version::from_str("0.2.3-rc.1").unwrap();
/// assert_eq!(increment_rc_version(&rc_version).unwrap().to_string(), "0.2.3-rc.2");
/// ```
pub fn increment_rc_version(version: &Version) -> Result<Version> {
    ensure!(Kind::Rc.matches(version), "Version is not an RC version: {}.", version);
    match version.pre.split('.').collect_vec().as_slice() {
        [RC_BUILD_PREFIX, index] => {
            let index = index.parse2::<u32>().context("Parsing RC index.")?;
            let pre = generate_rc_prerelease(index + 1)?;
            Ok(Version { pre, ..version.clone() })
        }
        _ => bail!("RC version has an unexpected prerelease: {}.", version),
    }
}

/// Compare version core, i.e. the part that is not a prerelease/build.
///
/// This is used to determine if a version is a prerelease of another version.
///
/// # Example
/// ```
/// use enso_build::prelude::*;
/// use enso_build::version::same_core_version;
///
/// let version = Version::from_str("1.2.3").unwrap();
/// assert!(same_core_version(&version, &version));
/// let version_beta = Version::from_str("1.2.3-beta").unwrap();
/// assert!(same_core_version(&version, &version_beta));
/// let version_build = Version::from_str("1.2.3+build").unwrap();
/// assert!(same_core_version(&version, &version_build));
/// let other_version = Version::from_str("1.2.4").unwrap();
/// assert!(!same_core_version(&version, &other_version));
/// ```
pub fn same_core_version(a: &Version, b: &Version) -> bool {
    a.major == b.major && a.minor == b.minor && a.patch == b.patch
}

pub fn generate_rc_prerelease(index: u32) -> Result<Prerelease> {
    Prerelease::from_str(&format!("{RC_BUILD_PREFIX}.{index}"))
}

#[instrument(ret)]
pub fn versions_from_env() -> Result<Option<Versions>> {
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
        let versions = Versions::new(version);
        Ok(Some(versions))
    } else {
        Ok(None)
    }
}

#[instrument(skip_all, ret)]
pub async fn deduce_or_generate<P, F>(release_provider: P) -> Result<Versions>
where
    P: FnOnce() -> F,
    F: Future<Output = Result<Vec<Version>>>, {
    debug!("Deciding on version to target.");
    if let Some(versions) = versions_from_env()? {
        Ok(versions)
    } else {
        let releases = release_provider().await?;
        let releases = promote::Releases::new_now(releases)?;
        let next_stable = releases.generate_version(promote::Designation::Stable)?;
        let dev_version = Version { pre: Prerelease::from_str(LOCAL_BUILD_PREFIX)?, ..next_stable };
        Ok(Versions::new(dev_version))
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
    /// Get the first piece of prerelease identifier that is used to designate this kind of version.
    ///
    /// Returns `None` if this kind of version requires empty prerelease.
    pub fn prerelease_prefix(self) -> Option<&'static str> {
        match self {
            Kind::Dev => Some(LOCAL_BUILD_PREFIX),
            Kind::Nightly => Some(NIGHTLY_BUILD_PREFIX),
            Kind::Rc => Some(RC_BUILD_PREFIX),
            Kind::Stable => None,
        }
    }

    /// Check if the given version number matches this kind.
    ///
    /// ```
    /// use enso_build::version::Kind;
    /// use semver::Version;
    ///
    /// assert!(Kind::Nightly.matches(&Version::parse("2022.1.1-nightly.2022.1.1").unwrap()));
    /// assert!(Kind::Nightly.matches(&Version::parse("2022.1.1-nightly").unwrap()));
    /// assert!(Kind::Rc.matches(&Version::parse("2022.1.1-rc.1").unwrap()));
    /// assert!(!Kind::Nightly.matches(&Version::parse("2022.1.1-rc.1").unwrap()));
    /// assert!(!Kind::Stable.matches(&Version::parse("2022.1.1-rc.1").unwrap()));
    /// assert!(!Kind::Stable.matches(&Version::parse("2022.1.1-nightly").unwrap()));
    /// assert!(Kind::Stable.matches(&Version::parse("2022.1.1").unwrap()));
    /// ```
    pub fn matches(self, version: &Version) -> bool {
        if let Some(prefix) = self.prerelease_prefix() {
            version.pre.starts_with(prefix)
        } else {
            version.pre.is_empty()
        }
    }

    /// Deduce the kind of the version.
    ///
    /// ```
    /// use enso_build::prelude::*;
    /// use enso_build::version::Kind;
    ///
    /// fn main() -> Result {
    ///     assert_eq!(Kind::deduce(&Version::parse("2022.1.1-dev")?)?, Kind::Dev);
    ///     assert_eq!(Kind::deduce(&Version::parse("2022.1.1-nightly.2022.1.1")?)?, Kind::Nightly);
    ///     assert_eq!(Kind::deduce(&Version::parse("2022.1.1-rc.1")?)?, Kind::Rc);
    ///     assert_eq!(Kind::deduce(&Version::parse("2022.1.1")?)?, Kind::Stable);
    ///     Ok(())
    /// }
    /// ```
    pub fn deduce(version: &Version) -> Result<Self> {
        Kind::iter()
            .find(|kind| kind.matches(version))
            .with_context(|| format!("Failed to deduce build kind for version {version}"))
    }

    /// Check if this is one of the prerelease kinds.
    pub fn is_prerelease(self) -> bool {
        self != Kind::Stable
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
}
