use crate::prelude::*;

use regex::Regex;
use std::sync::LazyLock;



// Taken from the official semver description:
// https://semver.org/#is-there-a-suggested-regular-expression-regex-to-check-a-semver-string
const SEMVER_REGEX_CODE: &str = r"(?P<major>0|[1-9]\d*)\.(?P<minor>0|[1-9]\d*)\.(?P<patch>0|[1-9]\d*)(?:-(?P<prerelease>(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+(?P<buildmetadata>[0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?";

/// Regular expression that matches a semver within a text.
static SEMVER_REGEX: LazyLock<Regex> = LazyLock::new(||
    // unwrap safe, as this is covered by test `semver_regex_parses`.
    Regex::new(SEMVER_REGEX_CODE).unwrap());

pub trait IsVersion: Debug + Display + PartialEq + Eq + Clone + Send + Sync + 'static {
    fn find_in_text_internal(text: &str) -> Result<Self>;

    fn find_in_text(text: &str) -> Result<Self>
    where Self: Sized {
        Self::find_in_text_internal(text).context(r#"Failed to find semver in the text: "{text}"."#)
    }
}

impl IsVersion for Version {
    fn find_in_text_internal(text: &str) -> Result<Self> {
        let matched =
            SEMVER_REGEX.find(text).context("No semver-like substring found within the text.")?;
        let version_text = matched.as_str();
        Version::from_str(version_text)
    }
}

pub trait IsVersionPredicate: Display + Send + 'static {
    type Version: IsVersion;
    fn matches(&self, version: &Self::Version) -> bool;
    fn require(&self, version: &Self::Version) -> Result {
        ensure!(
            self.matches(version),
            "Version {} does not match the predicate {}.",
            version,
            self
        );
        Ok(())
    }
}

impl IsVersionPredicate for semver::VersionReq {
    type Version = Version;
    fn matches(&self, version: &Self::Version) -> bool {
        self.matches(version)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn semver_regex_parses() {
        let _ = SEMVER_REGEX.deref(); // Does not panic.
    }

    #[test]
    fn parse_cargo() -> Result {
        let text = "cargo 1.57.0-nightly (c7957a74b 2021-10-11)";
        let version = Version::find_in_text(text)?;
        assert_eq!(version.major, 1);
        assert_eq!(version.minor, 57);
        assert_eq!(version.patch, 0);
        assert_eq!(version.pre, semver::Prerelease::new("nightly")?);
        assert_eq!(version.build, <_>::default());
        Ok(())
    }
}
