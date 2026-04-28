# @lezer/markdown

**Vendored fork** of the upstream Lezer markdown grammar. Paired with our fork of `@codemirror/lang-markdown`. We fork so we can add Enso-specific markdown constructs (e.g. inline widgets used in documentation) without waiting on upstream.

Keep deltas against upstream minimal and well-annotated in `CHANGELOG.md`. When upstream ships changes relevant to us, rebase rather than diverge further.

Build via `node build.js` (uses `@marijn/buildtool`). MIT licensed.
