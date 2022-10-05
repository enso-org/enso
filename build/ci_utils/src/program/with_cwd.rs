use crate::prelude::*;



/// Wrapper over a program that invokes it with a given working directory.
#[derive(Clone, Debug, Default)]
pub struct WithCwd<T> {
    pub working_directory:  Option<PathBuf>,
    pub underlying_program: T,
}

impl<T: Program> Program for WithCwd<T> {
    fn executable_name(&self) -> &str {
        self.underlying_program.executable_name()
    }

    fn current_directory(&self) -> Option<PathBuf> {
        self.working_directory.clone()
    }
}

impl<T> WithCwd<T> {
    pub fn new(underlying_program: T, working_directory: impl Into<PathBuf>) -> Self {
        Self { underlying_program, working_directory: Some(working_directory.into()) }
    }
}
