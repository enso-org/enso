This is a set of tests for the `Base` library for Enso.

The run test suite for the HTTP component requires an active `httbin` server on
the localhost. If it is present, the port it listens to should be provided by
setting the `ENSO_HTTP_TEST_HTTPBIN_URL` environment variable to a value like
`http://localhost:8080`. The URL may contain a trailing slash.
