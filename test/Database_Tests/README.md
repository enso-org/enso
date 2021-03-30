This is a set of tests for the `Database` library for Enso.

By default, the tests are run only for the SQLite backend since it requires no
additional setup.

The library also supports PostgreSQL. To run the test suite for PostgreSQL,
additional configuration is necessary. First a PostgreSQL server has to be
set-up, preferably with a separate user intended for this testing only and an
empty database.

Environment variables are used to pass the test configuration to the test
runner. To enable testing the PostgreSQL backend, a variable
`ENSO_DATABASE_TEST_DB_NAME` has to be defined and contain the name of the empty
database that should be used for testing.

Depending on the authentication configuration of the database, the username and
password may not need to be provided, but the set-up was only tested with
password-based authentication schemes so this approach is recommended. To
configure the credentials for testing, set `ENSO_DATABASE_TEST_DB_USER` and
`ENSO_DATABASE_TEST_DB_PASSWORD`. As keeping passwords in an environment
variable may not be the safest thing to do, it is much recommended using a
separate database account with low privileges for testing - the only privileges
that are necessary is the ability to create, modify, query and drop tables in
the `ENSO_DATABASE_TEST_DB_NAME` database.

If the database server is remote, `ENSO_DATABASE_TEST_HOST` may be set to tell
the test suite where to connect. If that variable is not set, the test suite
will attempt to connect to a local server.
