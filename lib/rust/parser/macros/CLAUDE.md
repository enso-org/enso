# enso-parser-macros

Proc-macro crate used **only** by `enso-parser`. Hosts derives and function-like
macros that the parser uses to express its pattern/segment/macro-match-tree
types concisely.

Keep proc-macro logic minimal and well-tested — a single typo here surfaces as
inscrutable compiler errors several crates away.
