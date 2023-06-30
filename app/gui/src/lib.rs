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
#![feature(cell_update)]
#![feature(drain_filter)]
#![feature(exact_size_is_empty)]
#![feature(iter_order_by)]
#![feature(option_result_contains)]
#![feature(trait_alias)]
#![feature(result_option_inspect)]
#![feature(map_try_insert)]
#![feature(assert_matches)]
#![feature(hash_drain_filter)]
#![feature(unwrap_infallible)]
#![feature(if_let_guard)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]



extern crate core;

use prelude::*;

use wasm_bindgen::prelude::*;

mod profile_workflow;
#[cfg(test)]
mod tests;


// ==============
// === Export ===
// ==============

pub mod config;
pub mod constants;
pub mod controller;
pub mod ide;
pub mod integration_test;
pub mod model;
pub mod presenter;
pub mod retry;
pub mod sync;
pub mod test;
pub mod transport;

pub use crate::ide::*;
pub use engine_protocol;
use enso_executor::web::EventLoopExecutor;
pub use ide_view as view;

/// Common types that should be visible across the whole IDE crate.
pub mod prelude {
    pub use ast::prelude::*;
    pub use enso_prelude::*;
    pub use ensogl::prelude::*;

    pub use crate::constants;
    pub use crate::controller;
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

    pub use enso_executor as executor;
    pub use enso_notification as notification;

    pub use std::ops::Range;

    pub use uuid::Uuid;

    #[cfg(test)]
    pub use wasm_bindgen_test::wasm_bindgen_test;
    #[cfg(test)]
    pub use wasm_bindgen_test::wasm_bindgen_test_configure;
}



// ====================
// === Entry Points ===
// ====================

// These imports are required to have all entry points (such as examples) and `before_main`
// functions (such as the dynamic-asset loader), available in the IDE.
#[allow(unused_imports)]
mod imported_for_entry_points {
    use enso_debug_scene::*;
    use ensogl_dynamic_assets::*;
    use ensogl_examples::*;
}
#[allow(unused_imports)]
use imported_for_entry_points::*;



// ====================
// === Global State ===
// ====================

thread_local! {
    static EXECUTOR: RefCell<Option<EventLoopExecutor>> = default();
    static IDE: RefCell<Option<Result<Ide, FailedIde>>> = default();
}



// =======================
// === IDE Entry Point ===
// =======================

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
    let config = config::Startup::from_web_arguments().expect("Failed to read configuration");
    let executor = executor::setup_global_executor();
    EXECUTOR.with(move |global_executor| global_executor.replace(Some(executor)));
    let initializer = Initializer::new(config);
    executor::global::spawn(async move {
        let ide = initializer.start().await;
        ensogl::system::web::document
            .get_element_by_id("loader")
            .map(|t| t.parent_node().map(|p| p.remove_child(&t).unwrap()));
        IDE.with(move |global_ide| global_ide.replace(Some(ide)));
    });
}



// ================
// === IDE Drop ===
// ================

/// Drop all structure created so far.
///
/// All connections will be closed and all visuals will be removed.
#[wasm_bindgen]
pub fn drop() {
    let ide = IDE.with(RefCell::take);
    if let Some(Ok(ide)) = ide {
        //TODO[ao] #6420 We should not do this, but somehow the `dom` field in the scene is
        // leaking.
        ide.ensogl_app.display.default_scene.dom.root.remove();
        // The presenter need to be dropped first, so all visible components should hide themselves
        // and be pushed to the garbage collector before we clear it.
        mem::drop(ide.presenter);
        //TODO[ao] As widgets which are garbage-collected may (and ofter do) keep references to
        // [`Application`] or [`World`], we need to force dropping them. This is not an ideal
        // solution, but the garbage collector will be removed soon anyway - see
        // https://github.com/enso-org/enso/issues/6850#issuecomment-1576754037
        ide.ensogl_app.display.force_garbage_drop();
        mem::drop(ide.ensogl_app)
    };
    EXECUTOR.with(RefCell::take);
    leak_detector::TRACKED_OBJECTS.with(|objects| {
        let objects = objects.borrow();
        if !objects.is_empty() {
            error!("Tracked objects leaked after dropping entire application!");
            error!("Leaked objects: {objects:#?}");
        }
    })
}
