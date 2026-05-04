# @codemirror/lang-markdown

**Vendored fork** of the upstream `@codemirror/lang-markdown` package. Reason for forking: we need upstream patches that haven't shipped yet, and we pair it with our fork of `@lezer/markdown` for Enso-specific nodes (e.g. inline widgets). Version numbering tracks upstream; license stays MIT.

Keep diffs against upstream small and documented. Rebase on upstream changes during library refreshes rather than diverging further.

Consumed by `enso-gui` for the documentation-panel editor.
