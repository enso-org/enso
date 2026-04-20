# enso-metamodel-lexpr

S-expression (`lexpr` crate) serialization of values described by
`enso-metamodel`. Used by `enso-parser-debug` to pretty-print parser output for
humans and for `insta` snapshot tests.

Also handles `bincode`-serialized round-trips in tests (cross-check that the
reflected shape matches the hand-rolled `Deserialize`).
