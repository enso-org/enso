# Enso 0.2.1 (2021-01-15)

## Language

- Removed Uniform Function Call Syntax, making the language much more
  predictable and allowing the compiler to provide improved diagnostics for
  common mistakes.

## Interpreter/Runtime

- Return correct qualified names for modules at runtime, ensuring that the
  compiler and interpreter agree.

## Tooling

- Improve the handling of bundled components with the Project Manager, ensuring
  rapid startup and easy integration with the IDE.
- Fixed the reflection configuration for the Project Manager, fixing a bug where
  it was unable to extract archives on Windows.
- The Language Server now uses qualified names in its messages, fixing a class
  of bugs where the IDE and Tooling did not agree on what a given expression
  was.
- Fixed mis-handling of tags in the documentation parsing infrastructure.

## Libraries

- Implemented a stub file for the functionality built into the interpreter. This
  allows us to provide comprehensive documentation about this functionality for
  display in the IDE and for reading by library users.
- Added aggregation functionality to the Table library, allowing users to group
  their data.

## Known Issues

- This is a beta release, so please see the
  [issue tracker](https://github.com/enso-org/enso/issues?q=is%3Aissue+is%3Aopen+created%3A%3C2021-01-15)
  for issues opened before the release date.

# Enso 0.2.0 (2021-01-05)

## Language

The initial version of the Enso language, with most language features
functioning.

- Flexible and concise syntax for the construction of functional programs,
  including pattern matching and lambdas.
- Support for currying, named and defaulted arguments, and operator sections.
- Support for user-defined data-types with fields and dynamically-dispatched
  methods on them.
- Importing and working with Java code in a seamless fashion using polyglot
  imports.
- Functional monadic state and error handling, through the use of data errors
  and panics.
- Opt-in tail-call optimisation.

## Interpreter/Runtime

- The initial version of the interpreter and runtime.

## Type System

- Nothing.

## Tooling

- The initial version of the Enso Launcher and Project Manager, supporting:
  - Installation and management of Enso releases, and the GraalVM runtimes on
    which they depend.
  - Aggregation of logs from the various Enso service components.
  - Basic project management functionality.
  - Initialisation and set-up of a language server for a specific project.
- The initial version of the Enso Language Server, supporting:
  - Dynamic introspection and modification of the running Enso program.
  - Caching of intermediate values in computations, ensuring that only necessary
    parts of the program are recomputed on a change.
  - Intelligent suggestions based on semantic analysis of the code.
  - Attaching visualisation code to values in the running Enso program.

## Libraries

- The initial version of `Base`, the core library, supporting:
  - Functionality for working with core types like `Integer`, `Decimal`, and
    `Text`.
  - Common data structures such as `List`, `Vector`, and `Map`.
  - Support for working with `JSON` data.
  - Support for working with `HTTP` endpoints.
  - Support for interacting with files and processes on the local machine.
  - Support for working with polyglot entities.
  - Support for metaprogramming the Enso language.
- The initial version of the Enso `Table` library for working with tabular data.
- The initial version of the Enso `Test` library, containing testing and
  benchmarking utilities.

## Stabilised Features

- A list of stabilised APIs and/or features.

## Misc

- Nothing.

## Known Issues

- This is a beta release, so please see the issue tracker for issues opened
  before the release date.

## Internal Only

- Nothing
