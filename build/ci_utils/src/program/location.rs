use crate::prelude::*;

use crate::program::command::MyCommand;



#[derive(Clone, Debug, derive_more::Deref, derive_more::DerefMut, PartialEq, Eq)]
pub struct Location<P> {
    #[deref]
    #[deref_mut]
    pub executable_path: PathBuf,
    pub phantom_data:    PhantomData<P>,
}

impl<P> AsRef<Path> for Location<P> {
    fn as_ref(&self) -> &Path {
        &self.executable_path
    }
}

impl<P> AsRef<OsStr> for Location<P> {
    fn as_ref(&self) -> &OsStr {
        self.executable_path.as_ref()
    }
}

impl<P: Program> Location<P> {
    pub fn new(path: impl Into<PathBuf>) -> Self {
        Self { executable_path: path.into(), phantom_data: default() }
    }

    pub fn cmd(&self) -> P::Command {
        P::Command::new_program(self)
    }
}
