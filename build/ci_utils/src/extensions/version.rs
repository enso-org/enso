use crate::prelude::*;

use semver::Prerelease;



/// Extension methods for [`Version`].
pub trait VersionExt {
    /// Get the version numbers, excluding the prerelease or build metadata.
    fn core(&self) -> (u64, u64, u64);

    /// Check if the version are the same while ignoring any prerelease or build metadata.
    fn same_core(&self, other: &Self) -> bool {
        self.core() == other.core()
    }

    /// Get the identifiers (i.e. the dot-separated parts after the hyphen) of the prerelease.
    ///
    /// ```
    /// # use semver::Version;
    /// # use ide_ci::extensions::version::VersionExt;
    /// assert!(Version::parse("1.2.3").unwrap().identifiers().is_empty());
    /// assert_eq!(Version::parse("1.2.3-alpha").unwrap().identifiers(), vec!["alpha"]);
    /// assert_eq!(Version::parse("1.2.3-alpha.1").unwrap().identifiers(), vec!["alpha", "1"]);
    /// assert_eq!(Version::parse("1.2.3-alpha+build.1").unwrap().identifiers(), vec!["alpha"]);
    /// ```
    fn identifiers(&self) -> Vec<&str>;

    /// Generate next minor version for this major release.
    ///
    /// ```
    /// # use semver::Version;
    /// # use ide_ci::extensions::version::VersionExt;
    /// let version = Version::parse("1.2.3-dev").unwrap();
    /// assert_eq!(version.next_minor().to_string(), "1.3.0");
    ///
    /// let version = Version::parse("2.2.0+fooo").unwrap();
    /// assert_eq!(version.next_minor().to_string(), "2.3.0");
    /// ```
    fn next_minor(&self) -> Self;
}

impl VersionExt for Version {
    fn core(&self) -> (u64, u64, u64) {
        (self.major, self.minor, self.patch)
    }
    fn identifiers(&self) -> Vec<&str> {
        self.pre.identifiers()
    }
    fn next_minor(&self) -> Self {
        Version::new(self.major, self.minor + 1, 0)
    }
}

pub trait PrereleaseExt {
    /// Get the identifiers (i.e. the dot-separated parts) of the prerelease.
    fn identifiers(&self) -> Vec<&str>;
}

impl PrereleaseExt for Prerelease {
    fn identifiers(&self) -> Vec<&str> {
        if self.is_empty() {
            default()
        } else {
            self.split('.').collect()
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn split_identifiers() -> Result {
        let version = Version::parse("1.2.3-alpha.1")?;
        assert_eq!(version.identifiers(), vec!["alpha", "1"]);

        let version = Version::parse("1.2.3")?;
        assert_eq!(version.identifiers(), Vec::<&str>::new());
        Ok(())
    }
}
