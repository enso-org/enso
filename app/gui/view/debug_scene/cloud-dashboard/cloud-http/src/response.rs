//! Module containing response types for the Cloud HTTP API.
//!
//! These types are used both on the server and the client side. On the server side, our Lambdas
//! return these types in response to requests. On the client side, we deserialize HTTP response
//! bodies to these types. By using identical models on both the server and client side, we avoid
//! mismatches between API specification and implementation. This does, however, mean that we have
//! to upgrade server and client side deployments in lockstep.

use enso_prelude::*;



// ============================================
// === `declare_routes_and_responses` Macro ===
// ============================================

/// This is a macro that is used to define the [`Route`]s to our Cloud API endpoints and the
/// responses returned by the Cloud API for requests to those endpoints.
///
/// The macro exists to keep the names of the [`Route`]s and their response structs in sync, rather
/// than for any code generation purposes. This is because each [`Route`] has a unique response type
/// and structure, so we can't use the macro to reduce code duplication.
macro_rules! declare_routes_and_responses {
    (
        $list_projects:ident
    ) => {
        // =============
        // === Route ===
        // =============

        /// A combination of the HTTP method and the relative URL path to an endpoint of our Cloud
        /// API.
        ///
        /// Instead of constructing HTTP requests manually using this enum, use the convenience
        /// methods on [`Client`] to send requests to the Cloud API instead -- it's simpler.
        ///
        /// Each variant in this enum corresponds to an available endpoint in our Cloud API. The
        /// [`Display`] impl of this enum provides the relative path to the endpoint. When combined
        /// with the base URL of our Cloud API (see [`Client`] for details), this becomes the full
        /// URL that requests should be sent to. The [`Route::method`] method provides the HTTP
        /// method that the request should be sent as.
        #[derive(Clone, Copy, Debug, Display)]
        #[allow(missing_docs)]
        pub(crate) enum Route {
            /// Endpoint for listing the user's projects. The response type for this endpoint is
            /// [`response::project::$list_projects`].
            #[display(fmt = "/projects")]
            $list_projects,
        }


        // === Main `impl` ===

        impl Route {
            /// Returns the [`http::Method`] used to interact with this route.
            ///
            /// [`http::Method`]: ::http::Method
            pub(crate) fn method(&self) -> http::Method {
                match self {
                    Self::$list_projects => http::Method::GET,
                }
            }
        }



        // ===============
        // === Project ===
        // ===============

        /// Module containing response types for [`Project`]-related [`Route`]s.
        ///
        /// [`Project`]: ::enso_cloud_view::project::Project
        pub mod project {
            use enso_cloud_view as view;



            // ====================
            // === ListProjects ===
            // ====================

            /// A response for a successful request to the [`ListProjects`] [`Route`].
            ///
            /// [`ListProjects`]: crate::Route::$list_projects
            /// [`Route`]: crate::Route
            #[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
            #[allow(missing_docs)]
            pub struct $list_projects {
                /// A list of all [`Project`]s that the user has access to. The list may be empty,
                /// if the user has access to no [`Project`]s or no [`Project`]s have been
                /// created.
                ///
                /// [`Project`]: ::enso_cloud_view::project::Project
                pub projects: Vec<view::project::Project>,
            }
        }
    };
}

declare_routes_and_responses!(ListProjects);
