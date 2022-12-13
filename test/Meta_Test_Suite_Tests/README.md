## Running the meta-tests

The test suite runs Enso forks to test the `Test` library using the `Test`
library. By default, it relies on the `enso` launcher being available on the
system path and will use the `enso default` version to run the tests.

If you do not want to use the launcher or want to use a different version you
can use the environment variables `ENSO_META_TEST_COMMAND` and
`ENSO_META_TEST_ARGS` to provide a path to an alternative runner command and its
arguments. The `ENSO_META_TEST_ARGS` override will only apply if also
`ENSO_META_TEST_COMMAND` is set.

For example, if you want to use just the engine runner directly to run the
tests, bypassing the launcher, you can run the following series of commands:

```
export ENSO_META_TEST_COMMAND=<path to the engine runner executable>
export ENSO_META_TEST_ARGS=--run
$ENSO_META_TEST_COMMAND --run test/Meta_Test_Suite_Tests
```

## Creating the tests

The test runner browses the `data` directory for subdirectories. Each
subdirectory is treated as a separate test. Each subdirectory is expected to
contain the following files:

- `Main.enso` - the entry point of the test that will be executed, it must
  contain a `main` method so that it can be run as a standalone script.
- `test_description.txt` - a plain text file containing a comment that will
  explain what the test does - it will be passed to the `Test.specify`
  invocation, displaying this comment in the test summary.
- `stdout.txt` - a plain text file describing the expected output of the test on
  the standard output stream.

Since parts of the test output will contain variable data - like test timing or
absolute file paths which depend on the system, any instance of three
consecutive question marks (`???`) will match any string. This can be used to
encode such varying parts of the test. The `???` will match any sequence of
characters, including newlines. All other characters in the file are matched
literally.

This is mostly a prototype / proof-of-concept, so currently, we are only
checking the standard output stream of the test. In the future we can easily
extend to also check standard error stream and exit code.
