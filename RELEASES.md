# Enso 0.2.9 (2021-03-26)

## Tooling

- Fixed an issue where a panic would be improperly cached, resulting in no
  updates being sent to the IDE
  ([#1611](https://github.com/enso-org/enso/pull/1611)).
- Added a feature to provide searcher suggestions for types compatible with the
  type of `this` ([#1613](https://github.com/enso-org/enso/pull/1613)).

## Libraries

- Added a prototype of a library for working with images
  ([#1450](https://github.com/enso-org/enso/pull/1450)).
- Added histogram and scatter-plot visualisation support for the `Table` library
  ([#1608](https://github.com/enso-org/enso/pull/1608)).
- Fixed a bug in the implementation of `join` in the database library where it
  would join on the wrong table when doing a multiple-join
  ([#1614](https://github.com/enso-org/enso/pull/1614)).
- Fixed an outdated example for the `File.read` function.

## Known Issues

- This is a beta release, so please see the
  [issue tracker](https://github.com/enso-org/enso/issues?q=is%3Aissue+is%3Aopen+created%3A%3C2021-03-26)
  for issues opened before the release date.

# Enso 0.2.8 (2021-03-19)

## Interpreter/Runtime

- Fixed miscellaneous crashes in the interpreter
  ([#1588](https://github.com/enso-org/enso/pull/1588)).

## Tooling

- Fixed an issue where the documentation for builtins wasn't getting indexed
  ([#1575](https://github.com/enso-org/enso/pull/1575)). The docs should now
  show up in the searcher!

## Libraries

- Added support for visualising database tables to the `Database` library
  ([#1582](https://github.com/enso-org/enso/pull/1582)).
- Reworked the `Process` library to work better in the IDE
  ([#1591](https://github.com/enso-org/enso/pull/1591)).
- Added a proper visualisation for `Array` and improved the one for `Vector`
  ([#1588](https://github.com/enso-org/enso/pull/1588)).

## Known Issues

- This is a beta release, so please see the
  [issue tracker](https://github.com/enso-org/enso/issues?q=is%3Aissue+is%3Aopen+created%3A%3C2021-03-19)
  for issues opened before the release date.

# Enso 0.2.7 (2021-03-16)

## Interpreter/Runtime

- Added rudimentary support for interoperability with Python
  ([#1541](https://github.com/enso-org/enso/pull/1541)). Due to limitations of
  the underlying implementation
  ([GraalPython](https://github.com/oracle/graalpython)), this does not
  currently work on windows. We are working to have some means of supporting
  Python interop on Windows.
- Added rudimentary support for interoperability with R
  ([#1559](https://github.com/enso-org/enso/pull/1559)). Due to limitations of
  the underlying implementation ([FastR](https://github.com/oracle/fastr)), this
  does not currently work on windows. We are working to have some means of
  supporting R interop on Windows.
- Fixed a performance issue that occurred due to the interpreter observing
  deeper scopes than necessary during server-controlled execution
  ([#1564](https://github.com/enso-org/enso/pull/1564)). Execution of lambdas in
  the IDE is no longer far slower than it should be.
- Fixed an issue where interrupts during the execution of polyglot Java code
  would cause the host classloader to break, preventing further execution
  ([#1574](https://github.com/enso-org/enso/pull/1574)). _Please note_ that the
  fix that has been put in place is suboptimal, and means that we are currently
  unable to interrupt host code during its execution. We intend to fix this as
  soon as a fix for the host classloader has been merged upstream. You can track
  the associated issue in GraalVM
  [here](https://github.com/oracle/graal/issues/3273).
- Fixed an issue where the interpreter would crash due to project name shadowing
  ([#1571](https://github.com/enso-org/enso/pull/1571)).

## Tooling

- Added support for lazy initialization of the language server
  ([#1535](https://github.com/enso-org/enso/pull/1535)). This ensures that it
  behaves properly on systems where the working directories are on
  lazily-mounted NFS volumes.
- Fixed an issue where the unified logging infrastructure would disconnect,
  preventing it from gathering diagnostic logs
  ([#1563](https://github.com/enso-org/enso/pull/1563)). It now sends periodic
  keepalive messages to ensure that the connection has not timed out.
- Fixed project name validation in the project manager when renaming projects
  ([#1570](https://github.com/enso-org/enso/pull/1570)).

## Libraries

- Added support for materializing data from databases in the database library
  ([#1546](https://github.com/enso-org/enso/pull/1546)). You can now use this
  library to connect to your data sources (currently only SQLite and Postgres,
  but support for further backends is planned).
- Reorganized the standard library in order to support plans for its future
  evolution ([#1571](https://github.com/enso-org/enso/pull/1571)).

## Known Issues

- This is a beta release, so please see the
  [issue tracker](https://github.com/enso-org/enso/issues?q=is%3Aissue+is%3Aopen+created%3A%3C2021-03-16)
  for issues opened before the release date.

# Enso 0.2.6 (2021-03-02)

## Interpreter/Runtime

- Fixed another issue where the parser would crash on partial issues, causing
  issues for both the Engine and IDE
  ([#1523](https://github.com/enso-org/enso/pull/1523)).
- Made panic messages short, fixing an issue where retention would cause
  ballooned memory usage while the full message contents were waiting to be
  logged ([#1528](https://github.com/enso-org/enso/pull/1528)).

## Tooling

- Fixed an issue where dynamic dependencies were analysed incorrectly, leading
  to missing updates for the IDE
  ([#1532](https://github.com/enso-org/enso/pull/1532)).

## Known Issues

- This is a beta release, so please see the
  [issue tracker](https://github.com/enso-org/enso/issues?q=is%3Aissue+is%3Aopen+created%3A%3C2021-03-02)
  for issues opened before the release date.

# Enso 0.2.5 (2021-02-26)

## Interpreter/Runtime

- Fixed an issue where the parser would crash on partial inputs, causing issues
  for both the engine and IDE
  ([#1509](https://github.com/enso-org/enso/pull/1509)).
- Fixed a problem where `Type_Error`s would not be displayed properly when
  pretty printed ([#1504](https://github.com/enso-org/enso/pull/1504)).
- Fixed an issue with `_` desugaring where it would not desugar correctly when
  used in function position
  ([#1512](https://github.com/enso-org/enso/pull/1512)).

## Tooling

- Fixed an issue where suggestions were sometimes not being provided for modules
  other than `Base` ([#1507](https://github.com/enso-org/enso/pull/1507)).
- Fixed a few issues where expression and value updates were not sent when they
  should be ([#1516](https://github.com/enso-org/enso/pull/1516),
  [#1522](https://github.com/enso-org/enso/pull/1522), and
  [#1508](https://github.com/enso-org/enso/pull/1508)).

## Libraries

- Fixed a bug where sorting boolean columns in a `Table` would produce incorrect
  output ([#1505](https://github.com/enso-org/enso/pull/1505)).

## Known Issues

- This is a beta release, so please see the
  [issue tracker](https://github.com/enso-org/enso/issues?q=is%3Aissue+is%3Aopen+created%3A%3C2021-02-26)
  for issues opened before the release date.

# Enso 0.2.4 (2021-02-23)

## Interpreter/Runtime

- Fixed another issue where dependency analysis was operating at too fine a
  granularity ([#1495](https://github.com/enso-org/enso/pull/1495)).
- Moved all user-facing errors to in-Enso errors, allowing them to be presented
  properly in the IDE, and interacted with by users
  ([#1487](https://github.com/enso-org/enso/pull/1487)).

## Tooling

- Fixed an issue where the runtime server would not send correct expression
  payloads for dataflow errors
  ([#1484](https://github.com/enso-org/enso/pull/1484)).

## Libraries

- Added "pretty" representations to all Error types, allowing for better display
  in the IDE ([#1498](https://github.com/enso-org/enso/pull/1498)).
- Updated the Table library with a raft of additional features
  ([#1489](https://github.com/enso-org/enso/pull/1489)). This includes table
  concatenation, direct indexing and column aggregation, as well as a general
  clean-up of the API pre-stabilisation.
- Added a flexible sorting mechanism to the Table library
  ([#1471](https://github.com/enso-org/enso/pull/1471)).

## Known Issues

- This is a beta release, so please see the
  [issue tracker](https://github.com/enso-org/enso/issues?q=is%3Aissue+is%3Aopen+created%3A%3C2021-02-23)
  for issues opened before the release date.

# Enso 0.2.3 (2021-02-15)

## Tooling

- Improved the robustness of the project manager and language server in the
  presence of rapid restarts
  ([#1463](https://github.com/enso-org/enso/pull/1463)).

## Libraries

- Significantly improved the efficiency of visualising large tables through
  zero-cost translation to Enso's vectors
  ([#1476](https://github.com/enso-org/enso/pull/1476)).

## Known Issues

- This is a beta release, so please see the
  [issue tracker](https://github.com/enso-org/enso/issues?q=is%3Aissue+is%3Aopen+created%3A%3C2021-02-15)
  for issues opened before the release date.

# Enso 0.2.2 (2021-02-11)

## Language

- Significantly improved the way that dataflow errors and panics are handled in
  the interpreter. They now flow through the program in a far more seamless
  fashion, and panics are supported properly in the IDE without the whole
  program dying ([#1433](https://github.com/enso-org/enso/pull/1433)).
- Significantly improved the syntax and semantics for FFI with JVM languages,
  making it far more of a first-class citizen in Enso
  ([#1443](https://github.com/enso-org/enso/pull/1443)).
- Added support for polyglot JavaScript definitions to Enso
  ([#1451](https://github.com/enso-org/enso/pull/1451)). These allow users to
  write JavaScript inside Enso, and seamlessly call between Enso and JS code.

## Interpreter/Runtime

- Fixed an issue where executing a host value could result in a
  `NullPointerException` due to a missing null check
  ([#1413](https://github.com/enso-org/enso/pull/1413)).
- Fixed an issue where dataflow analysis was incorrectly tracking usages of
  undefined variables. This resulted in problems for the runtime server
  ([#1421](https://github.com/enso-org/enso/pull/1421)).

## Tooling

- Added support for collection of profiling information about the running
  program to the language server protocol
  ([#1407](https://github.com/enso-org/enso/pull/1407)). Initial support is for
  collection of execution-time information.
- Updated the default `main` in a new Enso project to be more IDE friendly
  ([#1419](https://github.com/enso-org/enso/pull/1419)).
- Added support for panic sentinels in the runtime instrument, allowing the
  language server to trace the expressions affected by a panic while still
  executing others ([#1436](https://github.com/enso-org/enso/pull/1436)).
- Added support for checking the Enso version for a particular project in the
  project manager, allowing the IDE to improve compatibility with multiple
  versions ([#1454](https://github.com/enso-org/enso/pull/1454)).

## Libraries

- Updated the way that we use dataflow errors in the standard libraries, making
  our `Base` functionality much more amenable to working in the IDE
  ([#1446](https://github.com/enso-org/enso/pull/1446)).

## Known Issues

- This is a beta release, so please see the
  [issue tracker](https://github.com/enso-org/enso/issues?q=is%3Aissue+is%3Aopen+created%3A%3C2021-02-10)
  for issues opened before the release date.

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
