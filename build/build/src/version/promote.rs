use crate::prelude::*;

use crate::version;
use crate::version::nightly::Nightly;
use crate::version::nightly::NightlyPrerelease;

use chrono::Datelike;
use ide_ci::programs::git;
use ide_ci::programs::git::Ref;



/// Describes what kind of version should be generated.
#[derive(
    clap::ArgEnum,
    Clone,
    Copy,
    Debug,
    PartialEq,
    Eq,
    Hash,
    strum::EnumString,
    strum::EnumIter,
    strum::AsRefStr
)]
#[strum(serialize_all = "lowercase")]
pub enum Designation {
    /// Create a new stable release.
    Stable,
    /// Create a new patch release.
    Patch,
    /// Create a new RC release.
    Rc,
    /// Create a new nightly release.
    Nightly,
}

pub async fn releases_on_remote(git: &git::Context) -> Result<Vec<Version>> {
    let remote_tags = git.list_remote_tags().await?;
    Ok(remote_tags
        .into_iter()
        .filter_map(|entry| match entry.r#ref {
            Ref::Tag { name, .. } => Version::from_str(&name).ok(),
            _ => None,
        })
        .collect())
}

/// List of all releases on the remote.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Releases {
    /// A list of all released versions.
    pub versions: BTreeSet<Version>,
    ///  Current date.
    ///
    /// It is used to determine major version in some cases, we keep it as parameter to keep the
    /// code testable. Also, defines a day for nightly releases.
    pub date:     chrono::NaiveDate,
}

impl Releases {
    /// Create, while using the current date.
    pub fn new_now(versions: impl IntoIterator<Item = Version>) -> Result<Self> {
        let date = chrono::Utc::now().naive_utc().date();
        Self::new(versions, date)
    }

    /// Create with a given date.
    pub fn new(
        versions: impl IntoIterator<Item = Version>,
        date: chrono::NaiveDate,
    ) -> Result<Self> {
        let versions: BTreeSet<_> = versions.into_iter().collect();
        for version in &versions {
            ensure!(
                version.major <= (date.year() as u64),
                "Found version from the future: {}.",
                version
            );
        }
        Ok(Self { versions, date })
    }

    /// Generate, based on all releases on the remote.
    pub async fn from_remote(git: &git::Context) -> Result<Self> {
        let date = chrono::Utc::now();
        let versions = releases_on_remote(git).await?;
        Self::new(versions, date.date_naive())
    }

    /// Iterates over releases of a given [`version::Kind`].
    pub fn of_kind(&self, kind: version::Kind) -> impl Iterator<Item = &Version> {
        self.versions.iter().filter(move |version| kind.matches(version))
    }

    /// Get the latest release of a given [`version::Kind`].
    pub fn latest_of_kind(&self, kind: version::Kind) -> Option<&Version> {
        self.of_kind(kind).max()
    }

    /// Generate a version for next release of a given [`Designation`].
    pub fn generate_version(&self, designation: Designation) -> Result<Version> {
        let latest_stable = self.latest_of_kind(version::Kind::Stable);
        let next_stable = if let Some(latest_stable) = latest_stable {
            version::suggest_next_version(latest_stable, self.date.year() as u64)
        } else {
            version::generate_initial_version(self.date.year() as u64)
        };

        let ret = match designation {
            Designation::Stable => next_stable,
            Designation::Patch => {
                let latest_stable = latest_stable.with_context(|| {
                    "No stable releases found. There must be some stable release before creating \
            a patch release."
                })?;
                Version::new(latest_stable.major, latest_stable.minor, latest_stable.patch + 1)
            }
            Designation::Rc => {
                let last_relevant_rc = self
                    .of_kind(version::Kind::Rc)
                    .filter(|version| version::same_core_version(version, &next_stable))
                    .max();
                if let Some(last_relevant_rc) = last_relevant_rc {
                    version::increment_rc_version(last_relevant_rc)?
                } else {
                    Version { pre: version::generate_rc_prerelease(1)?, ..next_stable }
                }
            }
            Designation::Nightly => {
                let last_relevant_nightly = self
                    .of_kind(version::Kind::Nightly)
                    .filter(|v| v.same_core(&next_stable))
                    .filter_map(|v| Nightly::try_from(v).ok())
                    .filter(|v| v.prerelease().date == self.date)
                    .max();
                if let Some(last_relevant_nightly) = last_relevant_nightly {
                    last_relevant_nightly.next().into()
                } else {
                    let prerelease = NightlyPrerelease::new(self.date, None);
                    Version { pre: prerelease.try_into()?, ..next_stable }
                }
            }
        };
        Ok(ret)
    }
}

#[cfg(test)]
mod tests {
    use super::*;


    fn expect_next_release(
        date: chrono::NaiveDate,
        releases_so_far: impl IntoIterator<Item: AsRef<str>>,
        designation: Designation,
        expected: &str,
    ) {
        let releases_so_far =
            releases_so_far.into_iter().map(|version| Version::from_str(version.as_ref()).unwrap());
        let releases = Releases::new(releases_so_far, date).unwrap();
        let next_release = releases.generate_version(designation).unwrap();
        assert_eq!(next_release.to_string(), expected);
    }

    #[test]
    fn test_version_generation() -> Result {
        use Designation::*;
        let previously_released_versions = ["2021.1.1", "2021.1.2"];
        let date = chrono::NaiveDate::from_ymd_opt(2021, 2, 22).context("Invalid date")?;
        let next_date = chrono::NaiveDate::from_ymd_opt(2021, 2, 23).context("Invalid date")?;
        expect_next_release(date, previously_released_versions, Stable, "2021.2.1");
        expect_next_release(date, previously_released_versions, Patch, "2021.1.3");
        expect_next_release(date, previously_released_versions, Rc, "2021.2.1-rc.1");
        expect_next_release(
            date,
            previously_released_versions,
            Nightly,
            "2021.2.1-nightly.2021.2.22",
        );

        let previously_released_versions = ["2021.1.1", "2021.1.2", "2021.2.1-nightly.2021.2.22"];
        expect_next_release(
            date,
            previously_released_versions,
            Nightly,
            "2021.2.1-nightly.2021.2.22.1",
        );
        expect_next_release(
            next_date,
            previously_released_versions,
            Nightly,
            "2021.2.1-nightly.2021.2.23",
        );
        Ok(())
    }
}
