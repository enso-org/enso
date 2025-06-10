//! Environment variables commonly used by AWS services.

use ide_ci::define_env_var;



define_env_var! {
    /// The AWS region to use.
    AWS_REGION, String;

    /// The AWS access key ID.
    AWS_ACCESS_KEY_ID, String;

    /// The AWS secret access key.
    AWS_SECRET_ACCESS_KEY, String;
}
