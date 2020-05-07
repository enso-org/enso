# Enso Polyglot at Runtime
This document deals with runtime implementation details of the polyglot
functionality of Enso. 

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Finding Polyglot Bindings](#finding-polyglot-bindings)

<!-- /MarkdownTOC -->

## Finding Polyglot Bindings
Polyglot objects for various languages are found in the `polyglot` subdirectory
of an Enso project. This folder is subdivided into directories based on the
polyglot language. The name of each subdirectory must match the language
identifier used in the source code.

Inside each directory is an implementation-defined structure, with the polyglot
implementation for that particular language needing to specify it. Please see
the language-specific documentation for details.
