use crate::prelude::*;



#[derive(Clone, Copy, Debug)]
pub struct GraalPy;

impl Program for GraalPy {
    type Command = Command;
    type Version = Version;

    fn executable_name(&self) -> &'static str {
        "graalpy"
    }
}
