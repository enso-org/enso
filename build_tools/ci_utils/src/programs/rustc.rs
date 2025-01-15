use crate::prelude::*;

use crate::program::command::Manipulator;



/// Lint check or a group of such.
// Full list can be obtained by `rustc -W help`.
#[derive(Clone, Copy, Debug, strum::Display, strum::AsRefStr)]
#[strum(serialize_all = "kebab-case")]
pub enum Lint {
    // == Groups ==
    /// All lints that are set to issue warnings.
    Warnings,
}

/// An option that can b e passed as a command line argument to rustc.
#[derive(Clone, Copy, Debug)]
pub enum Option {
    /// Set lint denied
    Deny(Lint),
}

impl Manipulator for Option {
    fn apply<C: IsCommandWrapper + ?Sized>(&self, command: &mut C) {
        match self {
            Option::Deny(lint) => {
                command.arg("--deny").arg(lint.as_ref());
            }
        }
    }
}
