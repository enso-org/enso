# lib/java

Java support libraries for the engine. Two shapes live together here:

1. **Third-party wrappers** (`*-wrapper`) — Shim modules that re-export a vendor
   dependency (Akka, Netty epoll, JLine, JNA, OpenCV, Apache POI, gRPC, DuckDB,
   Scala stdlib, fansi, zio, `sqlite-jdbc`, `conscrypt`, Tableau, …) with a
   `module-info.java` that makes them JPMS-friendly. The engine is a JPMS
   application, so every non-modular dep must pass through a wrapper.
2. **First-party modules** — real engine components that happen to be written in
   Java:
   - `interpreter-dsl/` — annotation processor that generates Truffle `Node`
     boilerplate (`@BuiltinMethod`, `@Specialization`, etc.).
   - `persistance/` + `persistance-dsl/` — custom serialization framework used
     by `engine/polyglot-api/` and IR caches. Annotation-driven.
   - `jvm-channel/` — typed bidirectional channel between embedded processes.
   - `jvm-interop/` — helpers for Enso ↔ Java interop.
   - `os-environment/` + `os-environment-lib/` — OS-specific process/env
     helpers.
   - `ydoc-api/`, `ydoc-server/`, `ydoc-polyfill/`, `ydoc-server-registration/`
     — Java side of the ydoc story. Lets the engine host `ydoc-server-polyglot`
     and register itself as a ydoc backend.
   - `logging-service-common/`, `logging-service-opensearch/`,
     `logging-service-telemetry/` — pluggable logging sinks.
   - `python-extract/`, `benchmarks-common/`, `runtime-utils/`, `test-utils/` —
     small helpers.

## Adding a JPMS wrapper

1. Create `lib/java/<thing>-wrapper/` with `build.sbt` project definition (see
   `build.sbt` + `project/JPMSPlugin.scala`).
2. Declare a `module-info.java` that `requires transitive` the underlying
   automatic module.
3. Register the module in the engine's module graph (`project/JPMSUtils.scala`).
4. If the original jar ships an `Automatic-Module-Name`, use that; otherwise
   open a named module and document why.

## Style

Follow `docs/style-guide/java.md`. Wrappers should contain _zero_ logic — only
module declarations.
