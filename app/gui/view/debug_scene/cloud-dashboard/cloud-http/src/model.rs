//! Module containing request and response types for the Cloud HTTP API.
//!
//! These types are used both on the server and the client side. On the server side, our Lambdas
//! return the response types in response to requests. On the client side, we deserialize HTTP
//! response bodies to these response types. The same applies to the request types. By using
//! identical models on both the server and client side, we avoid mismatches between API
//! specification and implementation. This does, however, mean that we have to upgrade server and
//! client side deployments in lockstep.

use enso_prelude::*;

use enso_cloud_view as view;



// =====================================================
// === `declare_routes_requests_and_responses` Macro ===
// =====================================================

/// This is a macro that is used to define the [`Route`]s to our Cloud API endpoints, the types
/// of the request bodies that can be sent to the Cloud API to those endpoints, and the responses
/// returned by the Cloud API for requests to those endpoints.
///
/// The macro exists to keep the names of the [`Route`]s, their request structs, and their response
/// structs in sync, rather than for any code generation purposes. This is because each [`Route`]
/// has a unique request/response type pair and structure, so we can't use the macro to reduce code
/// duplication.
macro_rules! declare_routes_requests_and_responses {
    (
        $list_projects:ident,
        $open_project:ident,
        $close_project:ident
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
        ///
        /// [`Client`]: crate::Client
        #[derive(Clone, Copy, Debug, Display)]
        pub enum Route {
            /// Endpoint for listing the user's [`Project`]s. The response type for this endpoint
            /// is [`response::project::$list_projects`].
            ///
            /// [`Project`]: ::enso_cloud_view::project::Project
            #[display(fmt = "/projects")]
            $list_projects,
            /// Endpoint for opening a [`Project`] that is not currently open. The response type
            /// for this endpoint is [`response::project::$open_project`].
            ///
            /// [`Project`]: ::enso_cloud_view::project::Project
            #[display(fmt = "/projects/{}/open", _0)]
            $open_project(view::id::ProjectId),
            /// Endpoint for closing a [`Project`] that is not currently closed. The response type
            /// for this endpoint is [`response::project::$close_project`].
            ///
            /// [`Project`]: ::enso_cloud_view::project::Project
            #[display(fmt = "/projects/{}/close", _0)]
            $close_project(view::id::ProjectId),
        }


        // === Main `impl` ===

        impl Route {
            /// Returns the [`http::Method`] used to interact with this route.
            ///
            /// [`http::Method`]: ::http::Method
            pub fn method(&self) -> http::Method {
                match self {
                    Self::$list_projects => http::Method::GET,
                    Self::$open_project(_) => http::Method::POST,
                    Self::$close_project(_) => http::Method::POST,
                }
            }
        }



        // ===============
        // === Request ===
        // ===============

        /// Module containing request types for the Cloud API.
        ///
        /// See [`crate::model`] for more details.
        pub mod request {



            // ===============
            // === Project ===
            // ===============

            /// Module containing request types for [`Project`]-related [`Route`]s.
            ///
            /// [`Route`]: crate::Route
            /// [`Project`]: ::enso_cloud_view::project::Project
            pub mod project {



                // ===================
                // === OpenProject ===
                // ===================

                /// A request to the [`OpenProject`] [`Route`], which starts the process of opening
                /// (i.e., start running) a [`Project`] that is not currently [`Opened`].
                ///
                /// [`OpenProject`]: crate::Route::$open_project
                /// [`Route`]: crate::Route
                /// [`Project`]: ::enso_cloud_view::project::Project
                /// [`Opened`]: ::enso_cloud_view::project::StateTag::Opened
                #[derive(Clone, Copy, Debug, serde::Deserialize, serde::Serialize)]
                #[allow(missing_docs)]
                pub struct $open_project {
                    /// Whether the Cloud instance (i.e., backing VM) that the [`Project`] runs on
                    /// should be created from scratch. Value is `true` if the [`Project`]'s AMI is
                    /// different from the default one.
                    ///
                    /// [`Project`]: ::enso_cloud_view::project::Project
                    #[serde(default = "bool::default")]
                    pub force_create: bool,
                }



                // ====================
                // === CloseProject ===
                // ====================

                /// A request to the [`CloseProject`] [`Route`], which starts the process of closing
                /// (i.e., stop running) an [`Opened`] (or partially [`Opened`]) [`Project`] that
                /// is not currently [`Closed`].
                ///
                /// [`CloseProject`]: crate::Route::$close_project
                /// [`Route`]: crate::Route
                /// [`Opened`]: ::enso_cloud_view::project::StateTag::Opened
                /// [`Project`]: ::enso_cloud_view::project::Project
                /// [`Closed`]: ::enso_cloud_view::project::StateTag::Closed
                #[derive(Clone, Copy, Debug, serde::Deserialize, serde::Serialize)]
                #[allow(missing_docs)]
                pub struct $close_project;
            }
        }



        // ================
        // === Response ===
        // ================

        /// Module containing response types for the Cloud API.
        ///
        /// See [`crate::model`] for more details.
        pub mod response {



            // ===============
            // === Project ===
            // ===============

            /// Module containing response types for [`Project`]-related [`Route`]s.
            ///
            /// [`Project`]: ::enso_cloud_view::project::Project
            /// [`Route`]: crate::Route
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
                    /// A list of all [`Project`]s that the user has access to. The list may be
                    /// empty, if the user has access to no [`Project`]s or no [`Project`]s have
                    /// been created.
                    ///
                    /// [`Project`]: ::enso_cloud_view::project::Project
                    pub projects: Vec<view::project::Project>,
                }



                // ===================
                // === OpenProject ===
                // ===================

                /// A response for a successful request to the [`OpenProject`] [`Route`].
                ///
                /// [`OpenProject`]: crate::Route::$open_project
                /// [`Route`]: crate::Route
                #[derive(Clone, Copy, Debug, serde::Deserialize, serde::Serialize)]
                #[allow(missing_docs)]
                pub struct $open_project;



                // ====================
                // === CloseProject ===
                // ====================

                /// A response for a successful request to the [`CloseProject`] [`Route`].
                ///
                /// [`CloseProject`]: crate::Route::$close_project
                /// [`Route`]: crate::Route
                #[derive(Clone, Copy, Debug, serde::Deserialize, serde::Serialize)]
                #[allow(missing_docs)]
                pub struct $close_project;
            }
        }
    };
}

declare_routes_requests_and_responses!(ListProjects, OpenProject, CloseProject);
