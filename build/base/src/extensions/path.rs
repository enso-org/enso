//! Extension methods for `Path` and `Path`-like types.1

use crate::prelude::*;

use serde::de::DeserializeOwned;



/// A number of extensions for `Path`-like types.
pub trait PathExt: AsRef<Path> {
    /// Append multiple segments to this path.
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
    /// use enso_build_base::extensions::path::PathExt;
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

    /// Parse this file's contents as a JSON-serialized value.
    #[context("Failed to deserialize file `{}` as type `{}`.", self.as_ref().display(), std::any::type_name::<T>())]
    fn read_to_json<T: DeserializeOwned>(&self) -> Result<T> {
        let content = crate::fs::read_to_string(self)?;
        serde_json::from_str(&content).with_context(|| format!("File content was: {}", content))
    }

    /// Write this file with a JSON-serialized value.
    fn write_as_json<T: Serialize>(&self, value: &T) -> Result {
        trace!("Writing JSON to {}.", self.as_ref().display());
        let file = crate::fs::create(self)?;
        serde_json::to_writer(file, value).anyhow_err()
    }

    /// Parse this file's contents as a YAML-serialized value.
    fn read_to_yaml<T: DeserializeOwned>(&self) -> Result<T> {
        let content = crate::fs::read_to_string(self)?;
        serde_yaml::from_str(&content).anyhow_err()
    }

    /// Write this file with a YAML-serialized value.
    fn write_as_yaml<T: Serialize>(&self, value: &T) -> Result {
        trace!("Writing YAML to {}.", self.as_ref().display());
        let file = crate::fs::create(self)?;
        serde_yaml::to_writer(file, value).anyhow_err()
    }

    /// Get the path as `str`.
    ///
    /// # Safety
    /// This will panic if the path contains invalid UTF-8 characters. Non-UTF-8 paths are not
    /// something that we want to spend time on supporting right now.
    fn as_str(&self) -> &str {
        self.as_ref()
            .to_str()
            .unwrap_or_else(|| panic!("Path is not valid UTF-8: {:?}", self.as_ref()))
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

    /// Like `file_name` but provides a sensible error message if the path has no file name.
    fn try_file_name(&self) -> Result<&OsStr> {
        self.as_ref().file_name().with_context(|| {
            format!("Failed to get file name of path `{}`.", self.as_ref().display())
        })
    }

    /// Like `file_stem` but provides a sensible error message if the path has no file stem.
    fn try_file_stem(&self) -> Result<&OsStr> {
        self.as_ref().file_stem().with_context(|| {
            format!("Failed to get file stem of path `{}`.", self.as_ref().display())
        })
    }

    /// Like `extension` but provides a sensible error message if the path has no extension.
    /// Note that this method fails for paths like `foo.`.
    fn try_extension(&self) -> Result<&OsStr> {
        self.as_ref().extension().with_context(|| {
            format!("Failed to get extension of path `{}`.", self.as_ref().display())
        })
    }

    /// Takes filename and splits it into file stem and extension.
    ///
    /// Fails if the path's filename has no extension.
    fn split_filename(&self) -> Result<SplitFilename> {
        let stem = self.try_file_stem()?;
        let extension = self.try_extension()?;
        Ok(SplitFilename { stem, extension })
    }

    /// Returns the path with replaced parent. The filename is kept intact.
    ///
    /// If there is no filename in the path, it is fully replaced.
    fn with_parent(&self, parent: impl AsRef<Path>) -> PathBuf {
        let mut ret = parent.as_ref().to_path_buf();
        ret.extend(self.as_ref().file_name());
        ret
    }
}

impl<T: AsRef<Path>> PathExt for T {}

/// A method that displays a value using `Display` trait.
pub fn display_fmt(path: &Path, f: &mut Formatter) -> std::fmt::Result {
    std::fmt::Display::fmt(&path.display(), f)
}


/// A result of splitting a path into its filename components.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SplitFilename<'a> {
    /// The file stem.
    pub stem:      &'a OsStr,
    /// The file extension.
    pub extension: &'a OsStr,
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

    #[test]
    /// This test just makes sure that usage of as_str correctly compiles without lifetime issues.
    /// (there were such before)
    fn foo() {
        fn bar(path: impl AsRef<Path>) {
            path.as_str();
            path.as_ref().as_str();
        }

        bar("");
        bar(String::from(""));
    }
}
