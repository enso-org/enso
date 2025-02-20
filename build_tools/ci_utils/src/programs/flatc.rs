use crate::prelude::*;



#[derive(Clone, Copy, Debug, Default)]
pub struct Flatc;

impl Program for Flatc {
    type Command = Command;
    type Version = Version;

    fn executable_name(&self) -> &'static str {
        "flatc"
    }
}
