# Standard Library

The Enso standard library sources, shipped as-is with every release. One
directory per namespace/module. Versioned under `<Module>/0.0.0-dev/` (a
placeholder that the release pipeline rewrites).

Every library has a `CLAUDE.md` at `<Library>/0.0.0-dev/CLAUDE.md` documenting
its public API, common usage, and library-specific pitfalls. The local Claude
agent (see `app/electron-client/CLAUDE.md`) reads this file plus `Base`'s and
`Table`'s at session start; **before using functionality from any other library,
Read that library's `CLAUDE.md` first**. Per-library files focus on
library-specific surface only — the universal conventions below are not repeated
there.

## Library index

- `Base/` — Core types, I/O, HTTP, errors, file system, datetime, Enso Cloud
  integration. Every other library imports from `Base`.
- `Table/` — Columnar in-memory and database-backed table operations.
- `Database/` — Generic SQL abstractions (`DB_Table`); backends below ship in
  their own libraries.
- `DuckDB/` — In-process analytical SQL backend (in-memory or `.duckdb` file).
- `Snowflake/` — Snowflake cloud data warehouse backend.
- `Microsoft/` — SQL Server / Azure SQL backend, plus OneDrive and Azure Blob.
- `Generic_JDBC/` — Bridge for any other JDBC driver (in-memory results, not
  deferred).
- `AWS/` — S3, Redshift, SES.
- `Google/` — Sheets, Analytics, Google credentials.
- `Google_Api/` — **Deprecated** placeholder; use `Google` instead.
- `Saas/` — Salesforce, Strava, SMTP email.
- `Tableau/` — Tableau Hyper extract (`.hyper`) read/write.
- `Image/` — Image read/write and arithmetic via OpenCV.
- `Geo/` — Latitude/longitude, distance, GeoJSON to table.
- `Examples/` — Sample data referenced by `## Examples` doc blocks.
- `Test/` — Testing framework used in `test/` projects.
- `Visualization/` — IDE-only visualisation preprocessors. **Do not import in
  user code.**
- `Searcher/` — IDE searcher metadata. **Do not import in user code.**

## Universal conventions

These rules apply to **every** library; per-library CLAUDE.mds do not repeat
them.

### Doc blocks (`## `)

Every public entity (type, constructor, method, function) is preceded by a `## `
doc block. Doc blocks carry:

- **Description** — written for users; states what the entity does.
- `## Arguments:` — one bullet per argument explaining its role; argument types
  appear in the type signature, not in these bullets.
- `## Examples` — runnable snippets, each prefaced by a sentence describing what
  it shows. **These are the authoritative usage reference.**
- `## ---` metadata block (appears before the description on flagged entries):
  `private: true`, `advanced: true`, `group: "..."`, `icon: "..."`.

When unsure of a method's name, signature, or correct call shape, `Read` the
source file and consult its doc block. Inventing names is the dominant failure
mode.

### Public vs. private

Generated code uses only the public API:

- **Public** = everything re-exported from `<Library>/src/Main.enso`.
- `src/Internal/` is **private**. Never import from it. Internal helpers can
  change shape between releases.
- Any entity with `private: true` in its `## ---` metadata block is private.
- Any module whose first non-blank source line is `private` is a private module
  — do not import.
- `@Builtin_Method` marks internal builtins; call the public wrapper.

### Library layout

Every library follows the same shape:

```
<Library>/0.0.0-dev/
├── package.yaml          # name, version, component groups, SPI registrations
├── src/
│   ├── Main.enso         # re-exports the public API (only these names are stable)
│   ├── Internal/         # private helpers; do not import
│   └── ...               # one .enso file per top-level entity, plus subdirs
├── docs/                 # human-facing documentation
├── data/                 # sample data shipped with the library
├── polyglot-sources/     # additional polyglot assets (rare)
└── THIRD-PARTY           # license notices for transitive third-party deps
```

Per-library `Layout` sections describe only the library-specific subdirs and
notable files, not these universal entries.

### Cross-library pitfalls

Patterns that come up in more than one library:

- **Credentials.** Connectors that take credentials accept
  `Enso_Secret.get "name"` (Enso Cloud secret) — prefer that over literal
  keys/passwords.
- **Connection lifecycle.** Call `.close` on database/file connections in
  long-running workflows; do not rely on GC finalisers.
- **Float equality.** Floating-point columns/values will not compare cleanly in
  `filter`/`sort`/`aggregate` — match on integers or use a tolerance.
- **Deferred vs in-memory.** `Standard.Database` returns deferred `DB_Table` (a
  query plan, not data); call `.read` to materialise. In-memory `Standard.Table`
  and `DB_Table` don't interop without an explicit boundary (`.read` or
  `.select_into_database_table`).
- **Autoscope (`..Constructor`).** Only resolves when the parameter's expected
  type is a single type. For union-typed parameters (e.g.
  `Filter_Condition | Text`) use the qualified `Type.Constructor` form.

## How Enso source links to Java helpers

Libraries call into `std-bits/<library>/` via `Polyglot.import <class>` — host
interop. There is **no compile-time check** across the boundary: a rename in
`std-bits` silently breaks the Enso source until a test catches it. Always run
`test/<Library>_Tests` when touching either side.

## Component groups

`package.yaml` declares `component-groups` — the palette the GUI shows to users.
Colors are `oklch(...)` triples. Adding a new function doesn't automatically
expose it in the palette; register it under a group or exposure is
dev-tooling-only.

## Service Provider Interfaces (SPI)

`package.yaml` also lists SPIs the library provides. This is how we add new file
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

## Style

- One top-level type or function per `src/<Thing>.enso` file.
- Internal helpers go under `src/Internal/` and are not re-exported.
- Each library exports its public API from `Main.enso`.
- Follow `docs/style-guide/enso.md`.
