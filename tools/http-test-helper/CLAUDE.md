# http-test-helper

A Java HTTP server that mimics [httpbin](http://httpbin.org) plus extra endpoints that simulate Enso Cloud behavior. Used as the fixture for `test/Base_Tests`.

## Run

```
sbt 'http-test-helper/run localhost 8080'
```

Then point the tests at it:
```
ENSO_HTTP_TEST_HTTPBIN_URL=http://localhost:8080
sbt 'runEngineDistribution --env ENSO_HTTP_TEST_HTTPBIN_URL=... --run test/Base_Tests'
```

Flags of note:
- `--enable-manual-log-batching-test` — used with `test/Base_Tests` to exercise the audit-log batching path (see `PostLogHandler.java`'s `batchingTestModeEnabled`).

## Adding an endpoint

Implement an `HttpHandler` under `src/main/java/org/enso/shttp/` and register it from the wiring file. Keep responses deterministic — tests diff bytes.

## Bazel

A `BUILD.bazel` target exists; SBT is the canonical build path though.
