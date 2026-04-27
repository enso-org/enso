# enso4igv / enso4vscode

Dual-purpose module:
- **VS Code extension** (`enso4vscode`) — Enso language support, debugger integration for `.enso` files.
- **IGV plugin** — Enso language support for the GraalVM Ideal Graph Visualizer. Makes Truffle compilation graphs human-readable when debugging performance. See `IGV.md`.

The package is published as a `.vsix` via `vsce`; the IGV plugin is built via `pom.xml` (Maven). See `README.md`.

Not on the hot path — only touched when debugging GraalVM compilation output or updating VSCode tooling.

Uses the NetBeans Java extension under the hood (`asf.apache-netbeans-java`) for Java + Enso debugging support.
