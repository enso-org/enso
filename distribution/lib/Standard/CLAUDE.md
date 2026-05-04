# Standard Library

The Enso standard library sources, shipped as-is with every release. One
directory per namespace/module. Versioned under `<Module>/0.0.0-dev/` (a
placeholder that the release pipeline rewrites).

## Modules

- `Base/` — Core types, I/O, HTTP, errors, file system, datetime, Enso Cloud
  integration. Every other module imports from `Base`.
- `Table/` — Columnar data / table operations.
- `Database/` — Generic SQL abstractions with SQLite and Postgres
  implementations.
- `Generic_JDBC/` — JDBC integration layer for unsupported connections.
- `AWS/`, `Google/`, `Google_Api/`, `Microsoft/`, `Snowflake/`, `Tableau/`,
  `DuckDB/`, `Saas/` — Provider-specific integrations.
- `Image/`, `Geo/` — Media and geo types.
- `Test/` — Testing framework used by every `test/` project.
- `Examples/` — Runnable example workflows used in documentation.
- `Visualization/` — Visualization builtins used by the GUI.
- `Searcher/` — Completion-related metadata.

## Anatomy of a module

A typical module looks like:

```
<Module>/0.0.0-dev/
├── package.yaml          # name, namespace, version, component groups, SPI registrations
├── src/                  # Enso source (one .enso file per top-level entity + sub-dirs)
├── docs/                 # Module-level documentation
├── data/                 # Ship-with-the-library sample data (where applicable)
├── polyglot-sources/     # Additional polyglot assets (rare)
└── THIRD-PARTY           # License notices for transitive third-party deps
```

## How Enso source links to Java helpers

Modules call into `std-bits/<module>/` via `Polyglot.import <class>` — host
interop. There is **no compile-time check** across the boundary: a rename in
`std-bits` silently breaks the Enso source until a test catches it. Always run
`test/<Module>_Tests` when touching either side.

## Component groups

`package.yaml` declares `component-groups` — the palette the GUI shows to users.
Colors are `oklch(...)` triples. Adding a new function doesn't automatically
expose it in the palette; register it under a group or exposure is
dev-tooling-only.

## Service Provider Interfaces (SPI)

`package.yaml` also lists SPIs the module provides. This is how we add new file
formats, data links, email providers, etc. without modifying `Base`. Pattern:

```
services:
  - provides: Standard.Base.System.File_Format.File_Format_SPI
    with: Standard.Base.Data.XML.XML_Format.XML_Format
```

## Versions

`0.0.0-dev` is rewritten at release time (see
`distribution/edition.template.yaml`). Do not hardcode version strings elsewhere
in the library.

## Conventions

- Each `src/<Thing>.enso` declares one top-level type or function.
- Internal helpers go under `src/Internal/` and should not be re-exported.
- Each module has a public `Main.enso` that re-exports its public API.
- Follow `docs/style-guide/enso.md`.
