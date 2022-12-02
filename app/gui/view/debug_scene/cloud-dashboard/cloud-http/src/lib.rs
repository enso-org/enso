//! Crate containing types for interacting with the Cloud dashboard.
//!
//! - [`Client`] is used to make requests to the Cloud dashboard API.
//! - Responses returned by the Cloud dashboard API are deserialized from JSON into strongly-typed
//!   structs, defined in the [`response`] module.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![deny(keyword_idents)]
#![deny(macro_use_extern_crate)]
#![deny(missing_abi)]
#![deny(pointer_structural_match)]
#![deny(unsafe_op_in_unsafe_fn)]
#![deny(unconditional_recursion)]
#![warn(missing_docs)]
#![warn(absolute_paths_not_starting_with_crate)]
#![warn(elided_lifetimes_in_paths)]
#![warn(explicit_outlives_requirements)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(noop_method_call)]
#![warn(single_use_lifetimes)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_extern_crates)]
#![warn(unused_import_braces)]
#![warn(unused_lifetimes)]
#![warn(unused_qualifications)]
#![warn(variant_size_differences)]
#![warn(unreachable_pub)]

use enso_cloud_view::prelude::*;
use enso_prelude::*;

use headers::authorization;
use response::Route;


// ==============
// === Export ===
// ==============

pub mod response;



// ==============
// === Client ===
// ==============

/// Client used to make HTTP requests to the Cloud API.
///
/// This struct provides convenience functions like [`Route::list_projects`] which let you send HTTP
/// requests to the Cloud API without building HTTP requests manually or dealing with details like
/// adding HTTP headers for authorization, etc. The convenience functions return strongly-typed
/// responses that are automatically deserialized from JSON.
#[derive(Clone, Debug)]
pub struct Client {
    /// The URL that Cloud API requests are sent to, minus the relative path to an API endpoint.
    ///
    /// [`Route`]s appended to this URL as a relative path to form the full URL of a request. Each
    /// API endpoint has an associated [`Route`].
    ///
    /// This URL is constructed differently for different environments. For example, in production
    /// this URL is usually our public domain (e.g., `https://cloud.enso.org`), in staging it is an
    /// AWS API Gateway URL (e.g., `https://12345678a.execute-api.us-west-1.amazonaws.com`), and in
    /// development it is a local URL (e.g., `http://localhost:8080`).
    ///
    /// If you are developing against a deployed environment, the AWS API Gateway URL can be
    /// looking in the Terraform output logs after a successful deployment.
    base_url: reqwest::Url,
    /// The JSON Web Token (JWT) used to authenticate requests to our API.
    token:    AccessToken,
    /// Underlying HTTP client used to make requests.
    ///
    /// This struct wraps the HTTP client and hides the details of how requestts are made, so that
    /// we can provide a simpler API to users of this crate. For example, use the
    /// [`Client::list_projects`] method to make requests to the [`Route::ListProjects`] endpoint.
    http:     reqwest::Client,
}


// === Main `impl` ===

impl Client {
    /// Creates a new instance of the Cloud API [`Client`].
    ///
    /// For more info about how the arguments are used, see the documentation of the [`Client`]
    /// struct and its fields, or the type documentation for the arguments.
    pub fn new(base_url: reqwest::Url, token: AccessToken) -> Result<Self, Error> {
        let http = reqwest::Client::new();
        Ok(Self { base_url, token, http })
    }

    /// Returns the list of [`Project`]s the user has access to.
    ///
    /// [`Project`]: ::enso_cloud_view::project::Project
    pub async fn list_projects(&self) -> Result<response::project::ListProjects, Error> {
        let route = Route::ListProjects;
        let response = self.try_request(route).await?;
        let projects = response.json().await?;
        Ok(projects)
    }
}


// === Internal `impl` ===

impl Client {
    /// Converts the [`Route`] into an HTTP [`Request`], executes it, and returns the [`Response`].
    ///
    /// # Errors
    ///
    /// Returns an [`Error`] if:
    /// - the [`Request`] could not be built,
    /// - the [`Request`] could not be executed (e.g., due to a network error),
    /// - the [`Response`] did not have a successful (i.e., 2xx) HTTP status code,
    ///
    /// [`Request`]: ::reqwest::Request
    /// [`Response`]: ::reqwest::Response
    async fn try_request(&self, route: Route) -> Result<reqwest::Response, Error> {
        let method = route.method();
        let relative_path = route.to_string();
        let mut url = self.base_url.clone();
        url.set_path(&relative_path);

        let request = self.http.request(method, url);
        let request = request.bearer_auth(&self.token);
        let request = request.build()?;

        let mut response = self.http.execute(request).await?;
        if !response.status().is_success() {
            response = handle_error_response(response).await?;
        }

        Ok(response)
    }
}

/// Creates a base URL for the Cloud Dashboard (as described in the documentation of the [`Client`]
/// struct) for an instance of the Cloud Dashboard running on AWS API Gateway.
///
/// Use this function to connect to the Cloud Dashboard API when running a staging or testing
/// deployment.
pub fn base_url_for_api_gateway(
    api_gateway_id: ApiGatewayId,
    aws_region: AwsRegion,
) -> Result<reqwest::Url, Error> {
    let url = format!("https://{api_gateway_id}.execute-api.{aws_region}.amazonaws.com").parse()?;
    Ok(url)
}

/// Converts an unsuccessful HTTP [`Response`] into an [`Error`], or returns the [`Response`].
///
/// This function exists to make user-facing errors for HTTP requests more informative by including
/// the HTTP response body in the error message, where possible.
///
/// [`Response`]: ::reqwest::Response
async fn handle_error_response(response: reqwest::Response) -> Result<reqwest::Response, Error> {
    if let Some(e) = response.error_for_status_ref().err() {
        match response.text().await {
            Ok(body) => {
                let e = format!("Error \"{e:?}\" with error message body: {body}");
                Err(e)?
            }
            Err(body_error) => {
                let e = format!("Failed to get error response body: \"{e:?}\"; {body_error}");
                Err(e)?
            }
        }
    } else {
        Ok(response)
    }
}



// ====================
// === ApiGatewayId ===
// ====================

/// Identifier of which Cloud API deployment a request is intended to go to (e.g., a development,
/// staging, production, or other deployment).
///
/// This value is the AWS API Gateway ID of the deployment. The API Gateway ID is a unique
/// identifier generated by AWS when the API is deployed for the first time. It is always available
/// in the output logs of a successful deployment via our Terraform scripts. It can also be found by
/// manually navigating to the API Gateway in the AWS web UI.
#[derive(Clone, Debug, Display)]
pub struct ApiGatewayId(pub String);



// =================
// === AwsRegion ===
// =================

/// AWS region in which the Cloud API is deployed.
///
/// This corresponds to the region of the AWS API Gateway identified by the [`ApiGatewayId`] used
/// when making requests to the Cloud API via the [`Client`]. This value can be found in our
/// Terraform output logs after a successful deployment of the Cloud API.
#[derive(Clone, Copy, Debug, Display)]
#[allow(missing_docs)]
pub enum AwsRegion {
    #[display(fmt = "eu-west-1")]
    EuWest1,
}



// ===================
// === AccessToken ===
// ===================

/// The JSON Web Token (JWT) used to authenticate requests to our API.
///
/// Requests without a token are rejected as unauthorized. A token determines which Cloud user is
/// making a request, what permissions they have, and what resources they can access.
///
/// The token can be obtained from the browser storage after the user logs in. Do so via the APIs
/// provided by the [`aws-amplify`] JavaScript library.
///
/// [`aws-amplify`]: https://docs.amplify.aws/lib/auth/getting-started/q/platform/js/
#[derive(Clone, Debug, Display)]
#[display(fmt = "{}", "bearer.token()")]
pub struct AccessToken {
    bearer: headers::Authorization<authorization::Bearer>,
}

impl AccessToken {
    /// Creates a new [`AccessToken`] from the given [`str`] slice.
    ///
    /// # Errors
    ///
    /// Returns an error if the value of the [`str`] slice is not a properly-formatted JSON Web
    /// Token (JWT). Note that we do not return an error if the token is expired, etc. We only check
    /// that it is properly formatted.
    pub fn new(token: &str) -> Result<Self, Error> {
        let bearer = headers::Authorization::bearer(token)?;
        let token = Self { bearer };
        Ok(token)
    }
}
