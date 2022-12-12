use crate::prelude::*;

use semver::Prerelease;



pub trait VersionExt {
    fn core(&self) -> (u64, u64, u64);
    fn same_core(&self, other: &Self) -> bool {
        self.core() == other.core()
    }
    /// Get the identifiers (i.e. the dot-separated parts after the hyphen) of the prerelease.
    fn identifiers(&self) -> Vec<&str>;
}

impl VersionExt for Version {
    fn core(&self) -> (u64, u64, u64) {
        (self.major, self.minor, self.patch)
    }
    fn identifiers(&self) -> Vec<&str> {
        self.pre.identifiers()
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
