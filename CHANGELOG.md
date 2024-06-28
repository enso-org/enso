# Next Release

#### Enso IDE

#### Enso IDE

# Enso 2024.1

#### Enso IDE

- [Arrows navigation][10179] selected nodes may be moved around, or entire scene
  if no node is selected.
- [Added a limit for dropdown width][10198], implemented ellipsis and scrolling
  for long labels when hovered.
- [Copy-pasting multiple nodes][10194].
- The documentation editor has [formatting toolbars][10064].
- The documentation editor supports [rendering images][10205].
- [Project may be renamed in Project View][10243]
- [Fixed a bug where drop-down were not displayed for some arguments][10297].
  For example, `locale` parameter of `Equal_Ignore_Case` kind in join component.
- [Node previews][10310]: Node may be previewed by hovering output port while
  pressing <kbd>Ctrl</kbd> key (<kbd>Cmd</kbd> on macOS).
- [Google Sheets clipboard support][10327]: Create a Table component when cells
  are pasted from Google Sheets.
- [Fixed issue with two arrows being visible at once in drop-down
  widget.][10337]
- [Fixed issue where picking "<Numeric literal>" variant in some ports
  disallowed changing it again.][10337]
- [Added click through on table and vector visualisation][10340] clicking on
  index column will select row or value in seperate node
- [Added support for links in documentation panels][10353].

[10064]: https://github.com/enso-org/enso/pull/10064
[10179]: https://github.com/enso-org/enso/pull/10179
[10194]: https://github.com/enso-org/enso/pull/10194
[10198]: https://github.com/enso-org/enso/pull/10198
[10205]: https://github.com/enso-org/enso/pull/10205
[10243]: https://github.com/enso-org/enso/pull/10243
[10297]: https://github.com/enso-org/enso/pull/10297
[10310]: https://github.com/enso-org/enso/pull/10310
[10327]: https://github.com/enso-org/enso/pull/10327
[10337]: https://github.com/enso-org/enso/pull/10337
[10340]: https://github.com/enso-org/enso/pull/10340
[10353]: https://github.com/enso-org/enso/pull/10353

#### Enso Standard Library

- [Added Statistic.Product][10122]
- [Added Encoding.Default that tries to detect UTF-8 or UTF-16 encoding based on
  BOM][10130]
- [Added `Decimal` column to the in-memory database, with some arithmetic
  operations.][9950]
- [Implemented `.cast` to and from `Decimal` columns for the in-memory
  database.][10206]
- [Implemented fallback to Windows-1252 encoding for `Encoding.Default`.][10190]
- [Added Table.duplicates component][10323]
- [Renamed `Table.order_by` to `Table.sort`][10372]

[debug-shortcuts]:

[9950]: https://github.com/enso-org/enso/pull/9950
[10122]: https://github.com/enso-org/enso/pull/10122
[10130]: https://github.com/enso-org/enso/pull/10130
[10206]: https://github.com/enso-org/enso/pull/10206
[10190]: https://github.com/enso-org/enso/pull/10190
[10323]: https://github.com/enso-org/enso/pull/10323
[10372]: https://github.com/enso-org/enso/pull/10372

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
