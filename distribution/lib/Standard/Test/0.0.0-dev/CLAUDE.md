# Standard.Test

The Enso testing framework: declare test suites and groups, write assertions,
test for panics and dataflow errors, generate fake data, run benchmarks. Used by
every project under `test/` in this repo. End-user workflows rarely import this
library.

## Main entry points

- `Test.build builder` — build a `Suite` from a function that adds groups and
  specs to a builder.
- `suite_builder.group "name" group_builder->` — declare a group.
- `group_builder.specify "case" <| <expr>` — declare a single test.
- `Test.expect_panic_with action panic_type` — assert that running `action`
  panics with `panic_type`.
- `<value>.should_equal <expected>` — assertion extension method.
- `<error_value>.should_fail_with <error_type>` — assert dataflow error.
- `<text>.should_contain <substring>` — substring assertion.
- `Suite.run`, `Suite.run_with_filter` — execute a suite.
- `Faker` — generate fake test data.
- `Bench` — benchmarking framework.

## Common usage

```
from Standard.Test import all

add_specs suite_builder =
    suite_builder.group "Arithmetic" group_builder->
        group_builder.specify "addition works" <|
            (1 + 1).should_equal 2
        group_builder.specify "division by zero panics" <|
            Test.expect_panic_with (1/0) Arithmetic_Error

main = Test.build add_specs . run
```

## Layout

- `src/Test.enso` — `Test.build`, `Test.expect_panic_with`, panic helpers.
- `src/Suite.enso` — `Suite.run`, suite-builder type.
- `src/Extensions.enso` — assertion methods (`should_equal`, `should_fail_with`,
  `should_contain`, `should_be_a`, `should_be_true`, …).
- `src/Bench.enso` — benchmarking.
- `src/Faker.enso` — fake data generation.

## Things to avoid in generated code

- Calling assertion methods outside a `specify` block — they expect a test
  context.
- Importing `Standard.Test` in production code; reserve it for test projects.

## Where to read more

- `src/Test.enso` and `src/Extensions.enso` — full API with doc blocks.
- `test/Test_Tests/src/` — the framework tested against itself.
- `test/Base_Tests/src/` — extensive real-world test usage.
