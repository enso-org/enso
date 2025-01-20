use crate::prelude::*;

use sysinfo::Pid;
use sysinfo::Process;
use sysinfo::ProcessRefreshKind;
use sysinfo::System;



/// A wrapper over [`System`] that represents information about the process hierarchy.
#[derive(Debug, Clone)]
pub struct Hierarchy<'a> {
    /// Data about all known processes.
    pub processes: &'a HashMap<Pid, Process>,
    /// Children processes of each process.
    pub children:  HashMap<Pid, HashSet<Pid>>,
}

impl<'a> Hierarchy<'a> {
    /// Creates a new instance of the process hierarchy.
    ///
    /// The system will be used to refresh the process information.
    pub fn new(system: &'a mut System) -> Self {
        trace!("Refreshing system information.");
        system.refresh_processes_specifics(ProcessRefreshKind::default());
        let processes = system.processes();
        let mut children = HashMap::<_, HashSet<Pid>>::new();
        for (pid, process) in processes {
            let parent_pid = process.parent();
            if let Some(parent_pid) = parent_pid {
                children.entry(parent_pid).or_default().insert(*pid);
            } else {
                // Not really an error, some processes might not have a parent, like "System" or
                // "System Idle Process". Also, we might not have permissions to see the parent of
                // some processes.
                trace!(%pid, "Process has no parent information.");
            }
        }
        Self { processes, children }
    }

    /// Kills the process and all its descendants.
    ///
    /// Note that in case of partial failures, the function will at most log the error and continue.
    /// As much processes as possible will receive the kill signal.
    pub fn kill_process_subtree(&self, pid: Pid) {
        if let Some(children) = self.children.get(&pid) {
            for child in children {
                self.kill_process_subtree(*child);
            }
        }
        if let Some(process) = self.processes.get(&pid) {
            let name = process.name();
            let command = process.cmd();
            trace!(%pid, %name, ?command, "Killing process.");
            if !process.kill() {
                warn!(%pid, %name, ?command, "Failed to kill process.");
            }
        } else {
            warn!(%pid, "Failed to kill process. It does not exist.");
        }
    }
}
