# engine/runner

The CLI entry point for the engine. This is the **embedder-side** code that
constructs a GraalVM polyglot `Context`, initializes `EnsoLanguage`, and runs
user code (scripts, REPLs, tests, documentation).

## Structure

- `src/main/java/` — Main class, option parsing, project execution.
- `jupyter-kernel/` — Enso Jupyter kernel built on top of the runner.

## Embedder vs language

Only one `org.graalvm.polyglot` import boundary per process. The runner is the
**only** place that may depend on `graal-sdk` for polyglot APIs. Everything it
needs from the language comes back through `Value`.

## How it talks to the runtime

- Sets runtime options via `Context.Builder` (see `OptionsHelper` /
  `RuntimeOptions` in `engine/common`).
- Passes arguments / environment as context options.
- Drives execution by calling methods through `Value` using stringly-typed
  method names that match language-side registrations (chase constants across
  the boundary when debugging).

## Native Image

The runner is compiled to a native binary via Native Image for the
`enso`/`ensoup` commands (see `docs/infrastructure/native-image.md`). Avoid
anything that's not Native-Image-friendly (e.g. dynamic class loading,
reflection without config).
