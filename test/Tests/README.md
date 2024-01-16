This is a set of tests for the `Base` library for Enso.

## HTTP tests

The run test suite for the HTTP component requires an active helper server on
the localhost. If it is present, the port it listens to should be provided by
setting the `ENSO_HTTP_TEST_HTTPBIN_URL` environment variable to a value like
`http://localhost:8080`. The URL may contain a trailing slash.

To run the test server, you may use the following command:

```bash
sbt 'http-test-helper/run localhost 8080'
```

You can stop the server via Ctrl-C.

See [the server's documentation](../../tools/http-test-helper/README.md) for
more information.

## Cloud tests

By default, a subset of cloud tests runs whenever the
`ENSO_HTTP_TEST_HTTPBIN_URL` environment variable is set, using a mock of parts
of the cloud logic running on the helper server.

To run a full set of cloud tests against a real deployment, you need to set the
following 3 environment variables:

- `ENSO_CLOUD_API_URI` to point to the root of the cloud API,
- `ENSO_CLOUD_CREDENTIALS_FILE` to be a path to a file containing the
  credentials to use, e.g. `~/.enso/credentials`,
- `ENSO_RUN_REAL_CLOUD_TEST=1` to tell the test suite to run against a real
  cloud deployment.

Note that some cloud tests (e.g. testing secrets in HTTP requests) still require
the `ENSO_HTTP_TEST_HTTPBIN_URL` setup, even if running against a real cloud
deployment.
