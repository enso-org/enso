//! Environment variables commonly used by AWS services.

use ide_ci::define_env_var;



define_env_var! {
    /// Username for an Enso Cloud account used for running Cloud integration tests.
    ENSO_CLOUD_TEST_ACCOUNT_USERNAME, String;

    /// Password for an Enso Cloud account used for running Cloud integration tests.
    ENSO_CLOUD_TEST_ACCOUNT_PASSWORD, String;
}
