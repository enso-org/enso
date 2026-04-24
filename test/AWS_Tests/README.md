This is a set of tests for the AWS integration for Enso.

## Testing AWS

To run the tests, you need to set 3 environment variables

Please set the following environment variables:

- 'ENSO_RUN_REAL_CLOUD_TEST' - set this to 1, it just needs to be present
- 'ENSO_LIB_S3_AWS_ACCESS_KEY_ID' - This can be found in the Enso secret manager
  under Enso CI AWS S3 Test Access Key
- 'ENSO_LIB_S3_AWS_SECRET_ACCESS_KEY' - This can be found in the Enso secret
  manager under Enso CI AWS S3 Test Access Key
