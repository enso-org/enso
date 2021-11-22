# Repository Structure Overview

- `build` - Helper JS scripts used by `run` script in the main directory.
- `docs` - Documentation.
- `src/js` - The JS part of IDE application.
- `src/rust` - All Rust crates.

## Crates

The crates structure is currently in process of refactoring, and there are still
places where code does not fit the crate it is placed. The work is tracked in
#448 epic.

The detailed description of each crate is placed in its documentation at top of
its `lib.rs` file.

There are two main parts of our codebase: **EnsoGL library** and the **Enso IDE
application**. The crates used in common by those parts are put in
`src /rust/lib`, and should be documented at the top of their `lib.rs` file.

Other crates usually contains code shared by those two. The one noteworthy is
the **prelude** crate (`src/rust/prelude`) which gathers the commonly used
imports across all crates in our repository.

### EnsoGL (`src/rust/ensogl`)

EnsoGL is a library providing fast 2D vector rendering engine with a rich set of
primitives and high-level GUI components. Its structure will be described after
the planned refactoring in #598.

### Enso IDE (`src/rust/ide`)

The Enso IDE crate contains entry point function of application, and integrates
two main parts of it:

- IDE View (`src/rust/ide/lib/view`) - The visual part of IDE. It contains no
  logic and connections to backend - instead it delivers FRP endpoints to be
  integrated with controllers. That design allow us to measure of vis-part only
  performance.
- IDE Controllers (`src/rust/ide/lib/controller`) - The logic part of IDE, that
  is synchronizing graph a text and communication with Engine. It should not
  know anything about visual part. In the future we may consider creating some
  sort of CLI for automatic integration tests.
