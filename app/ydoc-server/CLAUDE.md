# ydoc-server

Node-side Yjs server that bridges the Enso Language Server (JSON-RPC over
WebSocket) and GUI clients (Yjs/CRDT over WebSocket). It holds a Yjs document
per opened Enso module and reconciles local edits with LS file state.

## Role in the architecture

```
GUI (ydoc client) <--Yjs-->  ydoc-server  <--LS JSON-RPC-->  Language Server
```

The GUI never touches the raw LS text protocol — it only mutates a Yjs document.
This server:

- Accepts Yjs updates from clients via `y-protocols` / `y-websocket`.
- Diffs them against its in-memory copy of the file, emits `text/applyEdit` to
  the LS, and serializes LS notifications (`text/didChange`, file open/close)
  back into Yjs updates.
- Acts as the collaborative merge point: multiple GUI clients pointing at the
  same project converge here.

## Companion packages

- `ydoc-shared/` — types and the AST module shared with clients.
- `ydoc-channel/` — thin Yjs transport wrapper used by both sides.
- `ydoc-server-polyglot/` — this very server, but re-bundled for GraalJS so it
  can run _inside_ the engine's JVM process. If you change server behavior,
  build/test both variants.
- `ydoc-inspect/` — dev proxy that dumps all traffic; use it when debugging sync
  bugs.

## Source layout

- `src/ydoc.ts` — entry, sets up the WebSocket listener and per-doc session
  state.
- `src/languageServerSession.ts` — one session per open project; owns the LS
  JSON-RPC client.
- `src/edits.ts` — translation between Yjs operations and LS `TextEdit`s (via
  `fast-diff`).
- `src/serialization.ts` — binary ydoc wire format.
- `src/auth.ts` — token validation; shared with the cloud.
- `src/fileFormat.ts` — Enso `.enso` file serialization.
- `src/YjsBinaryChannel.ts` — custom binary channel over Yjs (for metadata the
  engine doesn't parse).
- `src/inspect.ts` — developer instrumentation; gated by env vars and used by
  `ydoc-inspect`.

## Running

- In dev: `corepack pnpm run -r compile` from repo root, then the GUI's
  `dev:vite` spawns a server via its Vite middleware. Override via
  `ENSO_IDE_YDOC_SERVER_URL=ws://host:port`.
- Standalone: `pnpm run dev:inspect` runs the server through `ydoc-inspect`.
- Polyglot (inside engine): see `ydoc-server-polyglot/`.

## Gotchas

- The LS is authoritative for file contents at rest — Yjs is authoritative only
  while a session is live. On reconnect, the server may need to re-seed the Yjs
  doc from LS state; see `languageServerSession.ts`.
- Binary messages use a custom framing (`YjsBinaryChannel`); don't assume plain
  Yjs awareness protocol works end-to-end.
- The Polyglot variant runs on GraalJS — only a subset of Node APIs is
  available. Check that any new dep works in both `node` and `graaljs` before
  importing.
