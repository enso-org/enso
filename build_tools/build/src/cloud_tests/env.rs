//! Environment variables commonly used by AWS services.

use ide_ci::define_env_var;



pub mod ci_config {
    use super::*;

    define_env_var! {
        /// Username for an Enso Cloud account used for running Cloud integration tests.
        ENSO_CLOUD_TEST_ACCOUNT_USERNAME, String;

        /// Password for an Enso Cloud account used for running Cloud integration tests.
        ENSO_CLOUD_TEST_ACCOUNT_PASSWORD, String;

        // The Client ID of the User Pool for Enso Cloud Cognito auth flow.
        ENSO_CLOUD_COGNITO_USER_POOL_WEB_CLIENT_ID, String;

        // The User Pool ID for Enso Cloud Cognito auth flow.
        ENSO_CLOUD_COGNITO_USER_POOL_ID, String;

        // The Region used for Cognito auth flow.
        ENSO_CLOUD_COGNITO_REGION, String;
    }
}

pub mod test_controls {
    use super::*;

    define_env_var! {
        /// Locates an Enso Cloud credentials file used in tests.
        ENSO_CLOUD_CREDENTIALS_FILE, String;

        /// Denotes the URI of the Enso Cloud API deployment to be used in tests.
        ENSO_CLOUD_API_URI, String;

        /// A flag that tells the test suite to run applicable tests on the cloud environment instead of just a mock.
        ENSO_RUN_REAL_CLOUD_TEST, String;
    }
}
