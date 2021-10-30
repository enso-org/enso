//! Native console formatter implementation.

use crate::prelude::*;

use crate::entry::level;
use crate::entry::GenericEntry;
use crate::processor::formatter;



// =====================
// === NativeConsole ===
// =====================

/// A nicely looking, colorful, basic formatter for a JavaScript console.
#[derive(Clone,Copy,Debug,Default)]
pub struct NativeConsole;

impl formatter::Output for NativeConsole {
    type Output = String;
}


// === Impls ===

impl formatter::Definition<level::Warning> for NativeConsole {
    fn format(entry:&GenericEntry) -> Option<Self::Output> {
        entry.content.message().map(|msg| format!("[W] {}",msg))
    }
}

impl formatter::Definition<level::Error> for NativeConsole {
    fn format(entry:&GenericEntry) -> Option<Self::Output> {
        entry.content.message().map(|msg| format!("[E] {}",msg))
    }
}

impl<Level> formatter::Definition<Level> for NativeConsole {
    default fn format(entry:&GenericEntry) -> Option<Self::Output> {
        entry.content.message().map(|msg| msg.to_owned())
    }
}
