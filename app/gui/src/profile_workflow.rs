//! Defines profilable workflows, and an entry point that runs a specified workflow.

use crate::integration_test::prelude::*;
use wasm_bindgen::prelude::*;

use enso_debug_api as debug_api;
use enso_web as web;



// ====================
// === ReflectMatch ===
// ====================

/// Match a value against a set of candidates; if no match is found, the list of candidates is
/// available.
macro_rules! reflect_match {
    (@acc ($dispatch:ident, $value:expr, $candidates:ident, {
        _ => $fallback:expr $(,)?
    }) -> {$( $branches:tt )*}) => {
        let mut $dispatch = ReflectMatch::new($value);
        match () {
            $( $branches )*
            _ => {
                let $candidates = $dispatch.candidates;
                $fallback
            }
        }
    };
    (@acc ($dispatch:ident, $value:expr, $candidates:ident, {
        $candidate:literal => $branch:expr,
        $( $rest:tt )*
    }) -> {$( $branches:tt )*}) => {
        reflect_match!(@acc ($dispatch, $value, $candidates, { $( $rest )* }) -> {
            $( $branches )*
            _ if $dispatch.matches($candidate) => $branch,
        })
    };
    ($candidates:ident, match $value:tt { $( $branches:tt )* }) => {
        reflect_match!(@acc (dispatch, $value, $candidates, { $( $branches )* }) -> {})
    };
}


// === ReflectMatch ===

/// Used to match a value against a set of candidates, while keeping track of the candidates.
struct ReflectMatch<T, U> {
    value:      T,
    candidates: Vec<U>,
}

impl<T, U> ReflectMatch<T, U> {
    /// Create a new dispatcher, for a given value.
    fn new(value: T) -> Self {
        let candidates = Default::default();
        Self { value, candidates }
    }

    /// Test the value against a candidate. Return whether it's a match.
    fn matches(&mut self, key: U) -> bool
    where T: PartialEq<U> {
        let matches = self.value == key;
        self.candidates.push(key);
        matches
    }
}



// ===================
// === Entry point ===
// ===================

#[wasm_bindgen]
#[allow(dead_code)]
pub async fn entry_point_profile() {
    web::forward_panic_hook_to_console();
    ensogl_text_msdf_sys::initialized().await;

    // Run selected workflow.
    let need_workflow = "`profile` entry point requires --workflow argument. \
    Try --workflow=help to see a list of options.";
    let selected = enso_config::ARGS.test_workflow.as_ref().expect(need_workflow);
    reflect_match!(options, match selected {
        "add_node" => profile_add_node().await,
        "new_project" => profile_new_project().await,
        _ => panic!("Unknown workflow: {selected}. Must be one of: {options:?}."),
    });

    // Emit profile and exit.
    debug_api::save_profile(&profiler::internal::take_log());
    debug_api::LifecycleController::new().expect("Workflows run in Electron").quit();
}



// ============================
// === Workflow definitions ===
// ============================

#[profile(Objective)]
async fn profile_add_node() {
    let test = Fixture::setup_new_project().await;
    let graph_editor = test.graph_editor();
    let initial_nodes = graph_editor.nodes().all.len();
    let expect_node_added = graph_editor.node_added.next_event();
    graph_editor.add_node();
    let _ = expect_node_added.expect();
    let added_nodes = 1;
    assert_eq!(graph_editor.nodes().all.len(), initial_nodes + added_nodes);
}

#[profile(Objective)]
async fn profile_new_project() {
    let test = Fixture::setup_new_project().await;
    mem::forget(Box::new(test));
}
