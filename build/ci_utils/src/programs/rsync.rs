use crate::prelude::*;



#[derive(Clone, Copy, Debug)]
pub struct Rsync;

impl Program for Rsync {
    fn executable_name(&self) -> &'static str {
        "rsync"
    }
}

#[derive(Clone, Copy, Debug, strum::AsRefStr)]
pub enum Option {
    /// archive mode; equals -rlptgoD (no -H,-A,-X)
    Archive,
    /// delete extraneous files from dest dirs
    Delete,
}

impl AsRef<OsStr> for Option {
    fn as_ref(&self) -> &OsStr {
        OsStr::new(match self {
            Self::Archive => "--archive",
            Self::Delete => "--delete",
        })
    }
}

pub async fn mirror_directory(source: impl AsRef<Path>, destination: impl AsRef<Path>) -> Result {
    // rsync treats "path/to/dir" and "path/to/dir/" differently.
    // We want the latter (otherwise `source` would be placed inside `destination`), so we append an
    // empty path segment.
    let source = source.as_ref().join("");
    Rsync
        .cmd()?
        .args([Option::Archive, Option::Delete])
        .arg(&source)
        .arg(destination.as_ref())
        .run_ok()
        .await
}
