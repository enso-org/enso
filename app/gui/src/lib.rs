//! The Enso IDE GUI.
//!
//! This rust crate is compiled to WASM library with all the logic of the Enso IDE GUI layer.
//! See README of the repository for the presentation of Enso IDE and its features.
//!
//! ## Where Things Start
//!
//! The function point which should be called by the web page embedding the Enso IDE is
//! `entry_point_main`.
//!
//! ## Main Layers
//!
//! - **Backend (Engine)**: The Enso IDE GUI uses the Engine Services as backend to manage and
//!   evaluate the Enso modules. The API of the services is described in the
//!   [Enso Protocol Documentation](https://enso.org/docs/developer/enso/language-server/protocol-architecture.html).
//!   and implemented in the [`engine_protocol`] crate (`controller/engine-protocol`).
//! - **Engine Model** (the [`model`] module): The Engine Model reflects the state of the Engine
//!   services: opened project, modules, attached visualizations and other entities. This Model is
//!   responsible for caching and synchronizing its state with the Engine Services.
//! - **Controllers** (the [`controller`] module). The controllers implement the logic of the Enso
//!   GUI and exposes the API to be easily used by the presenter.
//!   - **Double Representation** (the [`double_representation`] crate in
//!     `controller/double-representation`): The particular part of controllers: a library
//!     implementing conversion between textual and graph representation of Enso language.
//! - **View** (the [`ide-view`] crate in the `view` directory): A typical view layer: controls,
//!   widgets etc. implemented on the EnsoGL framework (See [`ensogl`] crate).
//! - **Presenter** (the [`presenter`] module): Synchronizes the state of the engine entities with
//!   the view, and passes the user interations to the controllers.

#![recursion_limit = "512"]
// === Features ===
#![feature(arc_unwrap_or_clone)]
#![feature(async_closure)]
#![feature(associated_type_bounds)]
#![feature(bool_to_option)]
#![feature(cell_update)]
#![feature(drain_filter)]
#![feature(exact_size_is_empty)]
#![feature(iter_order_by)]
#![feature(option_result_contains)]
#![feature(trait_alias)]
#![feature(result_into_ok_or_err)]
#![feature(result_option_inspect)]
#![feature(map_try_insert)]
#![feature(assert_matches)]
#![feature(cell_filter_map)]
#![feature(hash_drain_filter)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

use prelude::*;
use wasm_bindgen::prelude::*;


// ==============
// === Export ===
// ==============

pub mod config;
pub mod constants;
pub mod controller;
pub mod executor;
pub mod ide;
pub mod integration_test;
pub mod model;
pub mod notification;
pub mod presenter;
pub mod sync;
pub mod test;
pub mod transport;

pub use crate::ide::*;
pub use ide_view as view;



#[cfg(test)]
mod tests;

/// Common types that should be visible across the whole IDE crate.
pub mod prelude {
    pub use ast::prelude::*;
    pub use enso_prelude::*;
    pub use ensogl::prelude::*;

    pub use crate::constants;
    pub use crate::controller;
    pub use crate::executor;
    pub use crate::model;
    pub use crate::model::traits::*;

    pub use enso_profiler;
    pub use enso_profiler::prelude::*;

    pub use engine_protocol::prelude::BoxFuture;
    pub use engine_protocol::prelude::StaticBoxFuture;
    pub use engine_protocol::prelude::StaticBoxStream;

    pub use futures::task::LocalSpawnExt;
    pub use futures::Future;
    pub use futures::FutureExt;
    pub use futures::Stream;
    pub use futures::StreamExt;

    pub use std::ops::Range;

    pub use uuid::Uuid;

    #[cfg(test)]
    pub use wasm_bindgen_test::wasm_bindgen_test;
    #[cfg(test)]
    pub use wasm_bindgen_test::wasm_bindgen_test_configure;
}

// Those imports are required to have all examples entry points visible in IDE.
#[allow(unused_imports)]
mod examples {
    use enso_debug_scene::*;
    use ensogl_examples::*;
}
#[allow(unused_imports)]
use examples::*;
mod profile_workflow;



// ===================
// === Entry Point ===
// ===================

/// IDE startup function.
#[entry_point(ide)]
#[profile(Objective)]
#[allow(dead_code)]
pub fn main() {
    // Logging of build information.
    #[cfg(debug_assertions)]
    let debug_mode = true;
    #[cfg(not(debug_assertions))]
    let debug_mode = false;
    analytics::remote_log_value(
        "debug_mode",
        "debug_mode_is_active",
        analytics::AnonymousData(debug_mode),
    );
    let config =
        crate::config::Startup::from_web_arguments().expect("Failed to read configuration");
    let executor = crate::ide::initializer::setup_global_executor();
    let initializer = crate::ide::initializer::Initializer::new(config);
    executor::global::spawn(async move {
        let ide = initializer.start().await;
        ensogl::system::web::document
            .get_element_by_id("loader")
            .map(|t| t.parent_node().map(|p| p.remove_child(&t).unwrap()));
        std::mem::forget(ide);
    });
    std::mem::forget(executor);
}
