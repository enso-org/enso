# ydoc-inspect

Developer proxy that sits between a Yjs client and a `ydoc-server`, logging
every message. Used when debugging sync/merge bugs or protocol drift. Not
shipped to users.

Run from the repo root: `corepack pnpm run dev:inspect`.

Depends only on `ws`, `y-websocket`, and `yjs` — no workspace deps.
