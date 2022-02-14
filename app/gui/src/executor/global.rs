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


use crate::prelude::*;

use futures::task::LocalSpawn;
use futures::task::LocalSpawnExt;

thread_local! {
    /// Global spawner handle.
    ///
    /// It should be set up once, as part of the IDE initialization routine and
    /// remain accessible indefinitely.
    ///
    /// This is made thread local for tests which may be run in parallel; Each test should set
    /// executor independently.
    static SPAWNER: RefCell<Option<Box<dyn LocalSpawn>>> = default();
}

/// Sets the global spawner. It will remain accessible until it is set again to
/// something else.
///
/// Caller should also ensure that the spawner will remain functional the whole
/// time, so e.g. it must not drop the executor connected with this spawner.
pub fn set_spawner(spawner_to_set: impl LocalSpawn + 'static) {
    // Note [Global Executor Safety]
    SPAWNER.with(|s| *s.borrow_mut() = Some(Box::new(spawner_to_set)));
}

/// Spawns a task using the global spawner.
/// Panics, if called when there is no global spawner set or if it fails to
/// spawn task (e.g. because the connected executor was prematurely dropped).
pub fn spawn(f: impl Future<Output = ()> + 'static) {
    SPAWNER.with(|spawner| {
        let error_msg = "No global executor has been provided.";
        // Note [Global Executor Safety]
        let mut borrowed = spawner.borrow_mut();
        let unwrapped = borrowed.as_mut().expect(error_msg);
        let error_msg = "Failed to spawn the task. Global executor might have been dropped.";
        unwrapped.spawn_local(f).expect(error_msg);
    });
}
/// Process stream elements while object under `weak` handle exists.
///
/// Like [`utils::channel::process_stream_with_handle`] but automatically spawns the processor.
pub fn spawn_stream_handler<Weak, Stream, Function, Ret>(
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
    spawn(handler);
}

// Note [Global Executor Safety]
// =============================
// This borrowing is safe, because the global mutable state is only accessed through the
// two functions provided in this module, and the functions do not leak reference to the global
// spawner.
