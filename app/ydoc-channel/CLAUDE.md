# ydoc-channel

Small Yjs-based **bidirectional message channel** library shared by
`ydoc-server`, `ydoc-shared`, and the GUI. Wraps a Yjs doc and a transport
(WebSocket or otherwise) into a typed request/response / event channel.

Not a reimplementation of `y-protocols` — it sits alongside it for control-plane
messages that shouldn't live inside the synced `Y.Doc`'s root types.

Kept framework-free and dependency-light on purpose (`lib0` + `yjs` only) so
both Node and GraalJS bundles can consume it.
