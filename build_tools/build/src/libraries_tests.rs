pub mod env {
    ide_ci::define_env_var! {
        /// Whether to display all test results in the CI output.
        /// (By default successful tests are hidden.)
        REPORT_ALL_TESTS, String;

        /// Whether to print commands that add GitHub annotations.
        /// This should be enabled on workflows that want to post compilation errors or warnings as annotations on PRs.
        ENSO_LINT_ENABLE_GITHUB_ANNOTATIONS, String;
    }
}

pub mod s3 {
    /// Environment variables used inside of the S3 tests.
    pub mod env {
        ide_ci::define_env_var! {
            ENSO_LIB_S3_AWS_ACCESS_KEY_ID, String;
            ENSO_LIB_S3_AWS_REGION, String;
            ENSO_LIB_S3_AWS_SECRET_ACCESS_KEY, String;
        }
    }
}

pub mod snowflake {
    /// Environment variables used inside of the Snowflake tests.
    pub mod env {
        ide_ci::define_env_var! {
            ENSO_SNOWFLAKE_ACCOUNT, String;
            ENSO_SNOWFLAKE_USER, String;
            ENSO_SNOWFLAKE_PASSWORD, String;
            ENSO_SNOWFLAKE_DATABASE, String;
            ENSO_SNOWFLAKE_SCHEMA, String;
            ENSO_SNOWFLAKE_WAREHOUSE, String;
        }
    }
}
