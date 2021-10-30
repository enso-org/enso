# The Enso Engine

The Enso engine is the codebase responsible for compiling and executing Enso
code, as well as providing language server functionality to users of the
language. It is subdivided into two major components:

- [Language Server](./language-server): The Enso language service.
- [Polyglot API](./polyglot-api): The truffle-boundary safe API for
  communication between the language server and the runtime.
- [Runner](./runner): The command-line interface for Enso.
- [Runtime](./runtime): The compiler and interpreter for Enso.
