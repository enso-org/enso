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

Then execute the tests in another terminal as

```bash
sbt 'runEngineDistribution --env ENSO_HTTP_TEST_HTTPBIN_URL=http://localhost:8080 --run test/Base_Tests'
```

You can stop the server via Ctrl-C.

See [the server's documentation](../../tools/http-test-helper/README.md) for
more information.

### Testing audit log batching

Currently, we only have a manual scenario for testing that log messages are
actually batched.

To test it, launch the `http-test-helper` with additional
`--enable-manual-log-batching-test` flag. For more information, see the comment
on `batchingTestModeEnabled` in
[PostLogHandler.java](../../tools/http-test-helper/src/main/java/org/enso/shttp/cloud_mock/PostLogHandler.java).
