//! Code dealing with executors, i.e. entities that are used to execute asynchronous
//! computations, like `Future`s or `Stream`s.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]


// ==============
// === Export ===
// ==============

pub mod global;
pub mod test_utils;
pub mod web;



pub mod prelude {
    pub use enso_prelude::*;

    pub use futures::task::LocalSpawnExt;
    pub use futures::Future;
    pub use futures::FutureExt;
    pub use futures::Stream;
    pub use futures::StreamExt;

    pub use enso_profiler as profiler;
    pub use enso_profiler::prelude::*;
}

/// Creates a new running executor with its own event loop. Registers them as a global executor.
pub fn setup_global_executor() -> crate::web::EventLoopExecutor {
    let executor = crate::web::EventLoopExecutor::new_running();
    crate::global::set_spawner(executor.spawner.clone());
    executor
}
