# engine/language-server

The Enso Language Server (LS). Embedder-side service that owns a GraalVM
polyglot `Context` running the Enso runtime and exposes its capabilities over
**JSON-RPC (text protocol)** and a **binary protocol** for data links.

Read `README.md` here and `docs/language-server/` for the full protocol and
design notes.

## Responsibilities

- Manage project state, open files, and Yjs-backed text sessions (via
  `ydoc-server-polyglot`).
- Expose code execution, node management, introspection, completion,
  refactoring, type interactions.
- Talk to the runtime via `polyglot-api` messages through
  `RuntimeServerInstrument` (the Truffle instrument attached to the runtime).
- Serve binary data links (file upload/download, visualization data) on a
  separate socket.

## Source layout

- `src/main/java/` — Java code: JSON-RPC endpoints, handlers, data-link
  protocol.
- `src/main/scala/` — Scala code: session/project management, higher-level
  services (text, suggestions, libraries).
- `src/main/resources/` — Static resources (logging config, protocol schemas).
- `src/main/schema/` — Flatbuffers schemas for the binary protocol. Generated
  bindings are checked in.
- `src/test/` — Scalatest + integration specs (each endpoint gets a spec).
- `src/bench/` — JMH benchmarks for hot protocol paths.

## Protocol

- **Text**: JSON-RPC 2.0 over WebSocket. Endpoints defined per feature (e.g.
  `text/applyEdit`, `session/initProtocolConnection`, `search/completion`).
- **Binary**: Flatbuffers envelopes over WebSocket. Used for bulk data
  transfers.
- **Ydoc**: Yjs updates tunneled via a binary channel. When enabled, text
  protocol is still active but most edits ride on Yjs.

See `docs/language-server/` for every endpoint's schema.

## Threading

The LS uses Akka. Each client maintains an actor per concern (session,
suggestions, text, etc.). The runtime runs on its own thread(s) via Truffle.
Don't block Akka receivers on runtime calls — dispatch via `ask` / futures.

## Running standalone

```
./run backend test          # integration tests
./run backend bench         # benches
```

Or from SBT directly:

```
sbt language-server/run
```

Configured by `--log-level trace` during debugging. Expect a lot of output.
