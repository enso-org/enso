# docs/

Developer documentation for the Enso Engine, not user docs. (User docs live at
https://help.enso.org.) Read `README.md` for the top-level index.

## High-value entry points

- `CONTRIBUTING.md` — Build prerequisites, project layout overview, pull request
  rules.
- `enso-philosophy.md` — The design philosophy behind the language (read before
  filing an RFC).
- `runtime-guide.md` — Practical tour of the Truffle/GraalVM side. The single
  most useful file when working on the runtime.
- `runtime-roadmap.md` — Longer-term plans (Scala→Java migration, IR rework,
  type system).
- `sbt-cheatsheet.md` — SBT command reference tailored to our project.
- `style-guide/` — Per-language style guides (scala, java, rust, typescript,
  enso, markdown, yaml, haskell).
- `rfcs/` — Design RFCs. New language or tooling changes should start here.

## Per-subsystem

- `language-server/` — LS protocol, endpoints, behavior.
- `runtime/` — Compiler IR, passes, execution model, caching, demand analysis,
  builtins.
- `parser/` — Parser design notes.
- `polyglot/` — How Enso uses Graal polyglot.
- `libraries/` — Edition/library-distribution model.
- `cloud/`, `integrations/` — Enso Cloud and external-service integrations.
- `debugger/`, `profiler/` — Debug and profiling infrastructure.
- `infrastructure/` — SBT, native image, dual-JVM, GraalVM upgrade, benchmarks,
  ydoc, logging.
- `syntax/`, `semantics/`, `types/` — Language specification.
- `security/`, `SECURITY.md` — Vulnerability reporting + policies.

## When to update

- **New feature** → add to the subsystem doc it affects; cross-link from
  `runtime-roadmap.md` if it's a long-term initiative.
- **New RFC** → add under `rfcs/` with a sequence number. The RFC process is
  described in `CONTRIBUTING.md`.
- **Protocol change** → update `language-server/<endpoint>.md` or the
  Flatbuffers schema note in `infrastructure/`.

Files use Jekyll front-matter (`layout`, `title`, `category`, `tags`, `order`) —
preserve it.
