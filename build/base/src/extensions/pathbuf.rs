use crate::prelude::*;



pub trait PathBufExt {
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
