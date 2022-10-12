use crate::prelude::*;

use serde::de::DeserializeOwned;



pub trait PathExt: AsRef<Path> {
    fn join_iter<P: AsRef<Path>>(&self, segments: impl IntoIterator<Item = P>) -> PathBuf {
        let mut ret = self.as_ref().to_path_buf();
        ret.extend(segments);
        ret
    }

    /// Strips the leading `\\?\` prefix from Windows paths if present.
    fn without_verbatim_prefix(&self) -> &Path {
        self.as_str().strip_prefix(r"\\?\").map_or(self.as_ref(), Path::new)
    }

    /// Appends a new extension to the file.
    ///
    /// Does not try to replace previous extension, unlike `set_extension`.
    /// Does nothing when given extension string is empty.
    ///
    /// ```
    /// use ide_ci::extensions::path::PathExt;
    /// use std::path::PathBuf;
    ///
    /// let path = PathBuf::from("foo.tar").with_appended_extension("gz");
    /// assert_eq!(path, PathBuf::from("foo.tar.gz"));
    ///
    /// let path = PathBuf::from("foo").with_appended_extension("zip");
    /// assert_eq!(path, PathBuf::from("foo.zip"));
    /// ```
    fn with_appended_extension(&self, extension: impl AsRef<OsStr>) -> PathBuf {
        if extension.as_ref().is_empty() {
            return self.as_ref().into();
        } else {
            let mut ret = self.as_ref().to_path_buf().into_os_string();
            ret.push(".");
            ret.push(extension.as_ref());
            ret.into()
        }
    }

    #[context("Failed to deserialize file `{}` as type `{}`.", self.as_ref().display(), std::any::type_name::<T>())]
    fn read_to_json<T: DeserializeOwned>(&self) -> Result<T> {
        let content = crate::fs::read_to_string(self)?;
        serde_json::from_str(&content).anyhow_err()
    }

    fn write_as_json<T: Serialize>(&self, value: &T) -> Result {
        trace!("Writing JSON to {}.", self.as_ref().display());
        let file = crate::fs::create(self)?;
        serde_json::to_writer(file, value).anyhow_err()
    }

    fn write_as_yaml<T: Serialize>(&self, value: &T) -> Result {
        trace!("Writing YAML to {}.", self.as_ref().display());
        let file = crate::fs::create(self)?;
        serde_yaml::to_writer(file, value).anyhow_err()
    }

    fn as_str(&self) -> &str {
        self.as_ref().to_str().unwrap()
    }

    /// Split path to components and collect them into a new PathBuf.
    ///
    /// This is useful for `/` -> native separator conversion.
    fn normalize(&self) -> PathBuf {
        self.as_ref().components().collect()
    }

    /// Like `parent` but provides a sensible error message if the path has no parent.
    fn try_parent(&self) -> Result<&Path> {
        self.as_ref()
            .parent()
            .with_context(|| format!("Failed to get parent of path `{}`.", self.as_ref().display()))
    }
}

impl<T: AsRef<Path>> PathExt for T {}

pub fn display_fmt(path: &Path, f: &mut Formatter) -> std::fmt::Result {
    std::fmt::Display::fmt(&path.display(), f)
}

#[cfg(test)]
mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn stripping_unc_prefix() {
        let path_with_unc = Path::new(r"\\?\H:\NBO\ci-build\target\debug\enso-build2.exe");
        let path_without_unc = Path::new(r"H:\NBO\ci-build\target\debug\enso-build2.exe");
        assert_eq!(path_with_unc.without_verbatim_prefix(), path_without_unc);
        assert_eq!(path_without_unc.without_verbatim_prefix(), path_without_unc);
    }
}
