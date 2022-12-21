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



// =============
// === Route ===
// =============

/// A combination of the HTTP method and the relative URL path to an endpoint of our Cloud API.
///
/// Instead of constructing HTTP requests manually using this enum, use the convenience methods on
/// [`Client`] to send requests to the Cloud API instead -- it's simpler.
///
/// Each variant in this enum corresponds to an available endpoint in our Cloud API. The [`Display`]
/// impl of this enum provides the relative path to the endpoint. When combined with the base URL of
/// our Cloud API (see [`Client`] for details), this becomes the full URL that requests should be
/// sent to. The [`Route::method`] method provides the HTTP method that the request should be sent
/// as.
///
/// [`Client`]: crate::Client
#[derive(Clone, Copy, Debug, Display)]
pub enum Route {
    /// Endpoint for listing the user's [`Project`]s. The response type for this endpoint is
    /// [`response::project::ListProjects`].
    ///
    /// [`Project`]: ::enso_cloud_view::project::Project
    #[display(fmt = "/projects")]
    ListProjects,
    /// Endpoint for opening a [`Project`] that is not currently open. The response type for this
    /// endpoint is [`response::project::OpenProject`].
    ///
    /// [`Project`]: ::enso_cloud_view::project::Project
    #[display(fmt = "/projects/{}/open", _0)]
    OpenProject(view::id::ProjectId),
    /// Endpoint for closing a [`Project`] that is not currently closed. The response type for this
    /// endpoint is [`response::project::CloseProject`].
    ///
    /// [`Project`]: ::enso_cloud_view::project::Project
    #[display(fmt = "/projects/{}/close", _0)]
    CloseProject(view::id::ProjectId),
}


// === Main `impl` ===

impl Route {
    /// Returns the [`http::Method`] used to interact with this route.
    ///
    /// [`http::Method`]: ::http::Method
    pub fn method(&self) -> http::Method {
        match self {
            Self::ListProjects => http::Method::GET,
            Self::OpenProject(_) => http::Method::POST,
            Self::CloseProject(_) => http::Method::POST,
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

        /// A request to the [`OpenProject`] [`Route`], which starts the process of opening (i.e.,
        /// start running) a [`Project`] that is not currently [`Opened`].
        ///
        /// [`OpenProject`]: crate::Route::OpenProject
        /// [`Route`]: crate::Route
        /// [`Project`]: ::enso_cloud_view::project::Project
        /// [`Opened`]: ::enso_cloud_view::project::StateTag::Opened
        #[derive(Clone, Copy, Debug, serde::Deserialize, serde::Serialize)]
        #[allow(missing_docs)]
        pub struct OpenProject {
            /// Whether the Cloud instance (i.e., backing VM) that the [`Project`] runs on should
            /// be created from scratch. Value is `true` if the [`Project`]'s AMI is
            /// different from the default one.
            ///
            /// [`Project`]: ::enso_cloud_view::project::Project
            #[serde(default = "bool::default")]
            pub force_create: bool,
        }



        // ====================
        // === CloseProject ===
        // ====================

        /// A request to the [`CloseProject`] [`Route`], which starts the process of closing (i.e.,
        /// stop running) an [`Opened`] (or partially [`Opened`]) [`Project`] that is not currently
        /// [`Closed`].
        ///
        /// [`CloseProject`]: crate::Route::CloseProject
        /// [`Route`]: crate::Route
        /// [`Opened`]: ::enso_cloud_view::project::StateTag::Opened
        /// [`Project`]: ::enso_cloud_view::project::Project
        /// [`Closed`]: ::enso_cloud_view::project::StateTag::Closed
        #[derive(Clone, Copy, Debug, serde::Deserialize, serde::Serialize)]
        #[allow(missing_docs)]
        pub struct CloseProject;
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
        /// [`ListProjects`]: crate::Route::ListProjects
        /// [`Route`]: crate::Route
        #[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
        #[allow(missing_docs)]
        pub struct ListProjects {
            /// A list of all [`Project`]s that the user has access to. The list may be empty, if
            /// the user has access to no [`Project`]s or no [`Project`]s have been created.
            ///
            /// [`Project`]: ::enso_cloud_view::project::Project
            pub projects: Vec<view::project::Project>,
        }



        // ===================
        // === OpenProject ===
        // ===================

        /// A response for a successful request to the [`OpenProject`] [`Route`].
        ///
        /// [`OpenProject`]: crate::Route::OpenProject
        /// [`Route`]: crate::Route
        #[derive(Clone, Copy, Debug, serde::Deserialize, serde::Serialize)]
        #[allow(missing_docs)]
        pub struct OpenProject;



        // ====================
        // === CloseProject ===
        // ====================

        /// A response for a successful request to the [`CloseProject`] [`Route`].
        ///
        /// [`CloseProject`]: crate::Route::CloseProject
        /// [`Route`]: crate::Route
        #[derive(Clone, Copy, Debug, serde::Deserialize, serde::Serialize)]
        #[allow(missing_docs)]
        pub struct CloseProject;
    }
}
