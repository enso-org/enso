# enso-macro-utils

Published (`crates.io`) helpers for writing proc-macros against `syn` / `quote`
/ `proc-macro2`. Used by the rest of our proc-macro crates (`enso-macros`,
`enso-reflect-macros`, `enso-parser-syntax-tree-visitor`,
`enso-build-macros-lib`, …).

No Enso-specific logic lives here — keep it general enough that the crate could
stand alone.
