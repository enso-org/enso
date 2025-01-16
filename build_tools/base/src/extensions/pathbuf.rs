//! Extensions to the [`PathBuf`] type.

use crate::prelude::*;



/// Extension methods for [`PathBuf`].
pub trait PathBufExt {
    /// Replace the [parent][std::path::Path::parent] directory of the path, maintaining the
    /// [filename][std::path::Path::file_name].
    fn set_parent(&mut self, parent: impl AsRef<Path>);
}

impl PathBufExt for PathBuf {
    fn set_parent(&mut self, parent: impl AsRef<Path>) {
        let parent = parent.as_ref();
        let filename = self.file_name().map(ToOwned::to_owned);
        self.clear();
        self.push(parent);
        self.extend(filename);
    }
}
