This is a set of tests for the Snowflake integration for Enso.

## Testing Snowflake

To run the tests, you need to prepare credentials for a Snowflake instance that 
can be used to run them on.

Please set the following environment variables:
- `ENSO_SNOWFLAKE_ACCOUNT` - the account name for the Snowflake instance,
- `ENSO_SNOWFLAKE_USER` - the username to use for the tests,
- `ENSO_SNOWFLAKE_PASSWORD` - the password for the user,
- `ENSO_SNOWFLAKE_DATABASE` - the name of the database to use for the tests,
- `ENSO_SNOWFLAKE_SCHEMA` - the name of the schema to use for the tests,
  (optional, defaults to `PUBLIC`),
- `ENSO_SNOWFLAKE_WAREHOUSE` - the name of the warehouse to use for the tests 
  (optional, defaults to blank value).
