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
