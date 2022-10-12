use crate::prelude::*;

use crate::program::command::Manipulator;


// ==============
// === Export ===
// ==============

pub use crate::programs::java::Classpath;



#[derive(Clone, Debug)]
pub enum Options {
    /// Specify where to place generated class files
    Directory(PathBuf),
}

impl Manipulator for Options {
    fn apply<C: IsCommandWrapper + ?Sized>(&self, command: &mut C) {
        match self {
            Options::Directory(path) => command.arg("-d").arg(path),
        };
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Javac;

impl Program for Javac {
    fn executable_name(&self) -> &str {
        "javac"
    }
}
