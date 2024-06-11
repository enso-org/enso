# Next Release

#### Enso IDE

- [Arrows navigation][10179] selected nodes may be moved around, or entire scene
  if no node is selected.
- [Copy-pasting multiple nodes][10194].
- The documentation editor has [formatting toolbars][10064].
- The documentation editor supports [rendering images][10205].

[10064]: https://github.com/enso-org/enso/pull/10064
[10179]: https://github.com/enso-org/enso/pull/10179
[10194]: https://github.com/enso-org/enso/pull/10194
[10205]: https://github.com/enso-org/enso/pull/10205

#### Enso Standard Library

- [Added Statistic.Product][10122]
- [Added Encoding.Default that tries to detect UTF-8 or UTF-16 encoding based on
  BOM][10130]
- [Added `Decimal` column to the in-memory database, with some arithmetic
  operations.][9950]
- [Implemented `.cast` to and from `Decimal` columns for the in-memory
  database.][10206]
- [Implemented fallback to Windows-1252 encoding for `Encoding.Default`.][10190]

[debug-shortcuts]:

[9950]: https://github.com/enso-org/enso/pull/9950
[10122]: https://github.com/enso-org/enso/pull/10122
[10130]: https://github.com/enso-org/enso/pull/10130
[10206]: https://github.com/enso-org/enso/pull/10206
[10190]: https://github.com/enso-org/enso/pull/10190

<br/>![Release Notes](/docs/assets/tags/release_notes.svg)

#### Anonymous Data Collection

Please note that this release collects anonymous usage data which will be used
to improve Enso and prepare it for a stable release. We will switch to opt-in
data collection in stable version releases. The usage data will not contain your
code (expressions above nodes), however, reported errors may contain brief
snippets of out of context code that specifically leads to the error, like "the
method 'foo' does not exist on Number". The following data will be collected:

- Session length.
- Project management events (project open, close, rename).
- Errors (IDE crashes, Project Manager errors, Language Server errors, Compiler
  errors).
