use crate::prelude::*;

use sysinfo::Pid;


// ==============
// === Export ===
// ==============

pub mod hierarchy;



/// Kills the process and all its descendants.
///
/// Note that in case of partial failures, the function will at most log the error and continue.
/// As much processes as possible will receive the kill signal.
#[instrument]
pub fn kill_process_subtree(pid: Pid) {
    let mut system = sysinfo::System::new();
    hierarchy::Hierarchy::new(&mut system).kill_process_subtree(pid);
}
