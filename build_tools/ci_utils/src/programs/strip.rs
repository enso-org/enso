use crate::prelude::*;



/// Program that discards symbols and other data from object files.
#[derive(Debug, Clone, Copy)]
pub struct Strip;

impl Program for Strip {
    type Command = Command;
    type Version = Version;

    fn executable_name(&self) -> &str {
        "strip"
    }
}
