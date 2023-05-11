//! This module defines a shared, global state that stores a task spawner.
//!
//! It is expected that as a part of one-time initialization routine, IDE shall
//! call `set_spawner` to define a global spawner. Once that is done, the whole
//! codebase can use `spawn` function to schedule new tasks on the global
//! executor.
//!
//! The executor responsible for spawning tasks is expected to remain alive
//! indefinitely, at least until the last `spawn` call has been made.
//!
//! The IDE runs in single-threaded web environment, while dealing with remote
//! resources. As such, it is common that it will want to perform asynchronous
//! operations. Thus we want basically every single place in this crate to be
//! able to just "schedule a task" for execution.
//!
//! As there is no reason to use more than a one executor, it is reasonable to
//! make it globally accessible through this module.
//!
//! To learn more about concepts involved in asynchronous programming, like
//! executors, futures, tasks please refer to
//! https://rust-lang.github.io/async-book/
//!
//! # Debugging
//!
//! To see additional logs from the executor, enable `debug` feature of this crate.
//! This will enable additional logging of task spawning and completion, as well as
//! a [`print_running_tasks`] function to debug the list of currently running tasks.

use crate::prelude::*;

use futures::task::LocalSpawn;
use futures::task::LocalSpawnExt;



// =====================
// === GlobalSpawner ===
// =====================

/// Global spawner container. This structure is kept in the global variable `SPAWNER`. See module
/// docs for details.
#[derive(Default)]
struct GlobalSpawner {
    spawner:       RefCell<Option<Box<dyn LocalSpawn>>>,
    #[cfg(feature = "debug")]
    running_tasks: debug::RunningTasks,
}

impl GlobalSpawner {
    fn set_spawner(&self, spawner_to_set: impl LocalSpawn + 'static) {
        info!("Setting new spawner");
        *self.spawner.borrow_mut() = Some(Box::new(spawner_to_set))
    }

    fn spawn_with_name(&self, _name: &'static str, f: impl Future<Output = ()> + 'static) {
        // Note [Global Executor Safety]
        let mut borrowed = self.spawner.borrow_mut();
        if let Some(unwrapped) = borrowed.as_mut() {
            #[cfg(feature = "debug")]
            let f = {
                let running_tasks = self.running_tasks.clone_ref();
                async move {
                    running_tasks.start(_name);
                    f.await;
                    running_tasks.finish(_name);
                }
            };
            if unwrapped.spawn_local(f).is_err() {
                error!("Failed to spawn the task. Global executor might have been dropped.");
            }
        } else {
            error!("Fail to spawn the task. No global executor has been provided.")
        }
    }

    /// Print the list of currently running tasks to the console.
    #[cfg(feature = "debug")]
    fn print_running_tasks(&self) {
        console_log!("Running tasks:");
        for (name, count) in self.running_tasks.to_vec() {
            console_log!("{}: {}", name, count);
        }
    }
}

thread_local! {
    /// Global spawner handle.
    ///
    /// It should be set up once, as part of the IDE initialization routine and
    /// remain accessible indefinitely.
    ///
    /// This is made thread local for tests which may be run in parallel; Each test should set
    /// executor independently.
    static SPAWNER: GlobalSpawner = default();
}

/// Sets the global spawner. It will remain accessible until it is set again to
/// something else.
///
/// Caller should also ensure that the spawner will remain functional the whole
/// time, so e.g. it must not drop the executor connected with this spawner.
pub fn set_spawner(spawner_to_set: impl LocalSpawn + 'static) {
    // Note [Global Executor Safety]
    SPAWNER.with(|s| s.set_spawner(spawner_to_set));
}

/// Print the list of currently running tasks to the console.
pub fn print_running_tasks() {
    #[cfg(feature = "debug")]
    SPAWNER.with(|s| s.print_running_tasks());
    #[cfg(not(feature = "debug"))]
    console_log!("Debug feature is disabled. Enable it to see the list of running tasks.");
}

/// Spawns a task using the global spawner.
///
/// `name` is used for debugging purposes only if `debug` feature is enabled.
///
/// Panics, if called when there is no global spawner set or if it fails to
/// spawn task (e.g. because the connected executor was prematurely dropped).
pub fn spawn(name: &'static str, f: impl Future<Output = ()> + 'static) {
    SPAWNER.with(|s| s.spawn_with_name(name, f));
}

/// Process stream elements while object under `weak` handle exists.
///
/// `name` is used for debugging purposes only if `debug` feature is enabled.
///
/// Like [`utils::channel::process_stream_with_handle`] but automatically spawns the processor.
pub fn spawn_stream_handler<Weak, Stream, Function, Ret>(
    name: &'static str,
    weak: Weak,
    stream: Stream,
    handler: Function,
) where
    Stream: StreamExt + Unpin + 'static,
    Weak: WeakElement + 'static,
    Function: FnMut(Stream::Item, Weak::Strong) -> Ret + 'static,
    Ret: Future<Output = ()> + 'static,
{
    let handler = channel::process_stream_with_handle(stream, weak, handler);
    spawn(name, handler);
}



// =======================
// === Debug Utilities ===
// =======================

#[cfg(feature = "debug")]
mod debug {
    use super::*;
    use std::collections::HashMap;

    /// Tracks the currently running async tasks, and logs start and finish of each task spawned
    /// with [`GlobalSpawner`].
    #[derive(Debug, Default, Clone, CloneRef)]
    pub struct RunningTasks(Rc<RefCell<HashMap<&'static str, usize>>>);

    impl RunningTasks {
        /// Execution of the task started.
        pub fn start(&self, name: &'static str) {
            console_log!("Starting task {}.", name);
            let mut borrowed = self.0.borrow_mut();
            let entry = borrowed.entry(name).or_insert(0);
            *entry += 1;
        }

        /// Execution of the task finished.
        pub fn finish(&self, name: &'static str) {
            console_log!("Task {} finished.", name);
            let mut borrowed = self.0.borrow_mut();
            let entry = borrowed.entry(name).or_insert(0);
            *entry = entry.saturating_sub(1);
        }

        /// The list of currently running tasks.
        pub fn to_vec(&self) -> Vec<(&'static str, usize)> {
            let borrowed = self.0.borrow();
            borrowed.iter().filter(|(_, count)| **count > 0).map(|(n, c)| (*n, *c)).collect_vec()
        }
    }
}

// Note [Global Executor Safety]
// =============================
// This borrowing is safe, because the global mutable state is only accessed through the
// two functions provided in this module, and the functions do not leak reference to the global
// spawner.
