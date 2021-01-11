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
