//! Utilities and wrappers for the [`tempfile`] crate.

use crate::prelude::*;

/// Path to the possibly temporary directory.
///
/// It may be either a temporary directory created by [`tempfile::tempdir`]  that will be deleted
/// when dropped or a non-temporary directory that will not be deleted.
#[derive(Debug)]
pub enum Directory {
    /// Temporary directory.
    Temp(tempfile::TempDir),
    /// Non-temporary directory.
    NonTemp(PathBuf),
}

impl AsRef<Path> for Directory {
    fn as_ref(&self) -> &Path {
        match self {
            Self::Temp(temp) => temp.path(),
            Self::NonTemp(path) => path,
        }
    }
}

impl Deref for Directory {
    type Target = Path;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl From<tempfile::TempDir> for Directory {
    fn from(temp: tempfile::TempDir) -> Self {
        Self::Temp(temp)
    }
}

impl From<PathBuf> for Directory {
    fn from(path: PathBuf) -> Self {
        Self::NonTemp(path)
    }
}

impl Directory {
    pub fn new() -> Result<Self> {
        let temp = tempfile::tempdir()?;
        Ok(Self::Temp(temp))
    }


    /// Don't delete the directory when dropped.
    pub fn not_dropped(self) -> Self {
        match self {
            Self::Temp(temp) => {
                let path = temp.into_path();
                Self::NonTemp(path)
            }
            Self::NonTemp(_) => self,
        }
    }
}


#[derive(Clone, Copy, Debug, Display)]
pub enum Policy {
    /// Don't delete the directory when dropped.
    DontDrop,
    /// Delete the directory when dropped.
    Drop,
}

impl Default for Policy {
    fn default() -> Self {
        Self::Drop
    }
}

pub fn new_tempdir(policy: Policy) -> Result<Directory> {
    let temp = tempfile::tempdir()?;
    Ok(if matches!(policy, Policy::DontDrop) {
        temp.into_path().into()
    } else {
        temp.into()
    })
}
