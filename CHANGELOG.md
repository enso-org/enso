# Next Release

#### Visual Environment

- [Long names on the Node Searcher's list are truncated.][3373] The part of the
  name that doesn't fit in the Searcher's window is replaced with an ellipsis
  character ("…").
- [Magnet Alignment algorithm is used while placing new nodes][3366]. When we
  find an available free space for a new node, the node gets aligned with the
  surrounding nodes horizontally and vertically. This helps to preserve a nice
  grid-like layout for all the nodes.
- [Nodes created via the <kbd>TAB</kbd> key or by clicking the (+) button on the
  screen are now placed below all the selected nodes when more than one node is
  selected.][3361] (Previously, they were placed below the first node that was
  selected.) This makes it easier to achieve a compact, vertical layout of the
  graph.
- [Nodes created near existing nodes via the <kbd>TAB</kbd> key or by dropping a
  connection are now repositioned and aligned to existing nodes.][3301] This is
  to make the resulting graph prettier and avoid overlapping. In such cases,
  created nodes will be placed below an existing node or on the bottom-left
  diagonal if there is no space underneath.
- [Nodes can be added to the graph by double-clicking the output ports of
  existing nodes (or by clicking them with the right mouse button).][3346]
- [Node Searcher preserves its zoom factor.][3327] The visible size of the node
  searcher and edited node is now fixed. It simplifies node editing on
  non-standard zoom levels.
- [Nodes can be added to the graph by clicking (+) button on the screen][3278].
  The button is in the bottom-left corner. Node is added at the center or pushed
  down if the center is already occupied by nodes.
- [Maximum zoom factor is limited to 1.0x if IDE is not in Debug Mode.][3273]
- [Debug Mode for Graph Editor can be activated/deactivated using a
  shortcut.][3264] It allows access to a set of restricted features. See
  [debug-shortcuts].
- [New nodes can be created by dragging and dropping a connection on the
  scene.][3231]
- [Node connections can be dropped by pressing the Esc key while dragging
  them.][3231]
- [Added support of source maps for JS-based visualizations.][3208]
- [Fixed the alignment of newly created nodes to existing nodes with
  visualizations enabled.][3361] When applicable, new nodes are now placed below
  visualizations. (Previously, they were placed to the left of the
  visualizations.)
- [Fixed histograms coloring and added a color legend.][3153]
- [Fixed broken node whose expression contains non-ASCII characters.][3166]
- [Fixed developer console warnings about views being created but not
  registered.][3181]
- [Fixed developer console errors related to Language Server (mentioning code
  3003 and "Invalid version"), occurring during project opening and after new
  node cration.][3186]
- [Fixed developer console error about failing to decode a notification
  "executionContext/visualisationEvaluationFailed"][3193]

#### EnsoGL (rendering engine)

- [You can change font and set letters bold in the <code>text::Area</code>
  component][3385]. Use the <code>set_font</code> and
  <code>set_bold_bytes</code> respectively.

#### Enso Standard Library

- [Implemented `Vector.distinct` allowing to remove duplicate elements from a
  Vector][3224]
- [Implemented `Duration.time_execution` allowing timing of the execution of an
  expression within the UI][3229]
- [Improved performance of `Vector.filter` and `Vector.each`; implemented
  `Vector.filter_with_index`. Made `Vector.at` accept negative indices and
  ensured it fails with a dataflow error on out of bounds access instead of an
  internal Java exception.][3232]
- [Implemented the `Table.select_columns` operation.][3230]
- [Implemented the `Table.remove_columns` and `Table.reorder_columns`
  operations.][3240]
- [Implemented the `Table.sort_columns` operation.][3250]
- [Fixed `Vector.sort` to handle tail-recursive comparators][3256]
- [Implemented `Range.find`, `Table.rename_columns` and
  `Table.use_first_row_as_names` operations][3249]
- [Implemented `Text.at` and `Text.is_digit` methods][3269]
- [Implemented `Runtime.get_stack_trace` together with some utilities to process
  stack traces and code locations][3271]
- [Implemented `Vector.flatten`][3259]
- [Significant performance improvement in `Natural_Order` and new `Faker`
  methods added to `Standard.Test`][3276]
- [Implemented `Integer.parse`][3283]
- [Made `Text.compare_to` correctly handle Unicode normalization][3282]
- [Extend `Text.contains` API to support regex and case insensitive
  search.][3285]
- [Implemented new `Text.take` and `Text.drop` functions, replacing existing
  functions][3287]
- [Implemented new `Text.starts_with` and `Text.ends_with` functions, replacing
  existing functions][3292]
- [Implemented `Text.to_case`, replacing `Text.to_lower_case` and
  `Text.to_upper_case`][3302]
- [Implemented initial `Table.group_by` function on Standard.Table][3305]
- [Implemented `Text.pad` and `Text.trim`][3309]
- [Updated `Text.repeat` and added `*` operator shorthand][3310]
- [General improved Vector performance and new `Vector.each_with_index`,
  `Vector.fold_with_index` and `Vector.take` methods.][3236]
- [Implemented new `Text.insert` method][3311]
- [Implemented `Bool.compare_to` method][3317]
- [Implemented `Map.first`, `Map.last` functions. Expanded `Table.group_by` to
  also compute mode, percentile, minimum, maximum.][3318]
- [Implemented `Text.location_of` and `Text.location_of_all` methods.][3324]
- [Replaced `Table.group_by` with `Table.aggregate`][3339]
- [Implemented `Panic.catch` and helper functions for handling errors. Added a
  type parameter to `Panic.recover` to recover specific types of errors.][3344]
- [Added warning handling to `Table.aggregate`][3349]
- [Improved performance of `Table.aggregate` and full warnings
  implementation][3364]
- [Implemented `Text.reverse`][3377]
- [Implemented support for most Table aggregations in the Database
  backend.][3383]
- [Update `Text.replace` to new API.][3393]
- [Add encoding support to `Text.bytes` and `Text.from_bytes`. Renamed and added
  encoding to `File.read_text`. New `File.read` API.][3390]
- [Improved the `Range` type. Added a `down_to` counterpart to `up_to` and
  `with_step` allowing to change the range step.][3408]
- [Aligned `Text.split` API with other methods and added `Text.lines`.][3415]

[debug-shortcuts]:
  https://github.com/enso-org/enso/blob/develop/app/gui/docs/product/shortcuts.md#debug
[3153]: https://github.com/enso-org/enso/pull/3153
[3166]: https://github.com/enso-org/enso/pull/3166
[3181]: https://github.com/enso-org/enso/pull/3181
[3186]: https://github.com/enso-org/enso/pull/3186
[3193]: https://github.com/enso-org/enso/pull/3193
[3208]: https://github.com/enso-org/enso/pull/3208
[3224]: https://github.com/enso-org/enso/pull/3224
[3229]: https://github.com/enso-org/enso/pull/3229
[3231]: https://github.com/enso-org/enso/pull/3231
[3232]: https://github.com/enso-org/enso/pull/3232
[3230]: https://github.com/enso-org/enso/pull/3230
[3240]: https://github.com/enso-org/enso/pull/3240
[3250]: https://github.com/enso-org/enso/pull/3250
[3256]: https://github.com/enso-org/enso/pull/3256
[3249]: https://github.com/enso-org/enso/pull/3249
[3264]: https://github.com/enso-org/enso/pull/3264
[3269]: https://github.com/enso-org/enso/pull/3269
[3271]: https://github.com/enso-org/enso/pull/3271
[3259]: https://github.com/enso-org/enso/pull/3259
[3273]: https://github.com/enso-org/enso/pull/3273
[3276]: https://github.com/enso-org/enso/pull/3276
[3278]: https://github.com/enso-org/enso/pull/3278
[3283]: https://github.com/enso-org/enso/pull/3283
[3282]: https://github.com/enso-org/enso/pull/3282
[3285]: https://github.com/enso-org/enso/pull/3285
[3287]: https://github.com/enso-org/enso/pull/3287
[3292]: https://github.com/enso-org/enso/pull/3292
[3301]: https://github.com/enso-org/enso/pull/3301
[3302]: https://github.com/enso-org/enso/pull/3302
[3305]: https://github.com/enso-org/enso/pull/3305
[3309]: https://github.com/enso-org/enso/pull/3309
[3310]: https://github.com/enso-org/enso/pull/3310
[3316]: https://github.com/enso-org/enso/pull/3316
[3236]: https://github.com/enso-org/enso/pull/3236
[3311]: https://github.com/enso-org/enso/pull/3311
[3317]: https://github.com/enso-org/enso/pull/3317
[3318]: https://github.com/enso-org/enso/pull/3318
[3324]: https://github.com/enso-org/enso/pull/3324
[3327]: https://github.com/enso-org/enso/pull/3327
[3339]: https://github.com/enso-org/enso/pull/3339
[3344]: https://github.com/enso-org/enso/pull/3344
[3346]: https://github.com/enso-org/enso/pull/3346
[3349]: https://github.com/enso-org/enso/pull/3349
[3361]: https://github.com/enso-org/enso/pull/3361
[3364]: https://github.com/enso-org/enso/pull/3364
[3373]: https://github.com/enso-org/enso/pull/3373
[3377]: https://github.com/enso-org/enso/pull/3377
[3366]: https://github.com/enso-org/enso/pull/3366
[3379]: https://github.com/enso-org/enso/pull/3379
[3381]: https://github.com/enso-org/enso/pull/3381
[3391]: https://github.com/enso-org/enso/pull/3391
[3383]: https://github.com/enso-org/enso/pull/3383
[3385]: https://github.com/enso-org/enso/pull/3385
[3392]: https://github.com/enso-org/enso/pull/3392
[3393]: https://github.com/enso-org/enso/pull/3393
[3390]: https://github.com/enso-org/enso/pull/3390
[3408]: https://github.com/enso-org/enso/pull/3408
[3415]: https://github.com/enso-org/enso/pull/3415

#### Enso Compiler

- [Added overloaded `from` conversions.][3227]
- [Upgraded to Graal VM 21.3.0][3258]
- [Added the ability to decorate values with warnings.][3248]
- [Fixed issues related to constructors' default arguments][3330]
- [Fixed compiler issue related to module cache.][3367]
- [Fixed execution of defaulted arguments of Atom Constructors][3358]
- [Converting Enso Date to java.time.LocalDate and back][3374]
- [Functions with all-defaulted arguments now execute automatically][3414]

[3227]: https://github.com/enso-org/enso/pull/3227
[3248]: https://github.com/enso-org/enso/pull/3248
[3258]: https://github.com/enso-org/enso/pull/3258
[3330]: https://github.com/enso-org/enso/pull/3330
[3358]: https://github.com/enso-org/enso/pull/3358
[3360]: https://github.com/enso-org/enso/pull/3360
[3367]: https://github.com/enso-org/enso/pull/3367
[3374]: https://github.com/enso-org/enso/pull/3374
[3412]: https://github.com/enso-org/enso/pull/3412
[3414]: https://github.com/enso-org/enso/pull/3414
[3417]: https://github.com/enso-org/enso/pull/3417

# Enso 2.0.0-alpha.18 (2021-10-12)

<br/>![New Features](/docs/assets/tags/new_features.svg)

#### Enso Compiler

- [Updated Enso engine to version 0.2.30][engine-0.2.31]. If you're interested
  in the enhancements and fixes made to the Enso compiler, you can find their
  release notes
  [here](https://github.com/enso-org/enso/blob/develop/RELEASES.md).

<br/>![Bug Fixes](/docs/assets/tags/bug_fixes.svg)

#### Visual Environment

- [Fixed freezing after inactivity.][1776] When the IDE window was minimized or
  covered by other windows or invisible for any other reason for a duration
  around one minute or longer then it would often be frozen for some seconds on
  return. Now it is possible to interact with the IDE instantly, no matter how
  long it had been inactive.

<br/>

[1776]: https://github.com/enso-org/ide/pull/1776

# Enso 2.0.0-alpha.17 (2021-09-23)

<br/>![Bug Fixes](/docs/assets/tags/bug_fixes.svg)

#### Visual Environment

- [Correct handling of command-line flags.][1815] Command line arguments of the
  form `--backend=false` or `--backend false` are now handled as expected and
  turn off the "backend" option. The same fix has been applied to all other
  boolean command-line options as well.
- [Visualizations will be attached after project is ready.][1825] This addresses
  a rare issue when initially opened visualizations were automatically closed
  rather than filled with data.

<br/>

[1815]: https://github.com/enso-org/ide/pull/1815
[1825]: https://github.com/enso-org/ide/pull/1825

<br/>![New Features](/docs/assets/tags/new_features.svg)

#### Enso Compiler

- [Updated Enso engine to version 0.2.30][engine-0.2.30]. If you're interested
  in the enhancements and fixes made to the Enso compiler, you can find their
  release notes
  [here](https://github.com/enso-org/enso/blob/develop/RELEASES.md).

[engine-0.2.30]: https://github.com/enso-org/enso/blob/develop/RELEASES.md

# Enso 2.0.0-alpha.16 (2021-09-16)

<br/>![New Features](/docs/assets/tags/new_features.svg)

#### Visual Environment

- [Auto-layout for new nodes.][1755] When a node is selected and a new node gets
  created below using <kbd>Tab</kbd> then the new node is automatically
  positioned far enough to the right to find sufficient space and avoid
  overlapping with existing nodes.

[1755]: https://github.com/enso-org/ide/pull/1755

#### Enso Compiler

- [Updated Enso engine to version 0.2.29][engine-0.2.29]. If you're interested
  in the enhancements and fixes made to the Enso compiler, you can find their
  release notes
  [here](https://github.com/enso-org/enso/blob/develop/RELEASES.md).

[engine-0.2.29]: https://github.com/enso-org/enso/blob/develop/RELEASES.md

<br/>![Bug Fixes](/docs/assets/tags/bug_fixes.svg)

#### Visual Environment

- [Sharp rendering on screens with fractional pixel ratios.][1820]

[1820]: https://github.com/enso-org/ide/pull/1820

<br/>

# Enso 2.0.0-alpha.15 (2021-09-09)

<br/>![Bug Fixes](/docs/assets/tags/bug_fixes.svg)

#### Visual Environment

- [Fixed parsing of the `--no-data-gathering` command line option.][1831] Flag's
  name has been changed to `--data-gathering`, so now `--data-gathering=false`
  and `--data-gathering=true` are supported as well.

[1831]: https://github.com/enso-org/ide/pull/1831

# Enso 2.0.0-alpha.14 (2021-09-02)

<br/>![New Features](/docs/assets/tags/new_features.svg)

#### Visual Environment

- [Visualization previews are disabled.][1817] Previously, hovering over a
  node's output port for more than four seconds would temporarily reveal the
  node's visualization. This behavior is disabled now.

[1817]: https://github.com/enso-org/ide/pull/1817

#### Enso Compiler

- [Updated Enso engine to version 0.2.28][1829]. If you're interested in the
  enhancements and fixes made to the Enso compiler, you can find their release
  notes [here](https://github.com/enso-org/enso/blob/develop/RELEASES.md).

[1829]: https://github.com/enso-org/ide/pull/1829

# Enso 2.0.0-alpha.13 (2021-08-27)

<br/>![New Features](/docs/assets/tags/new_features.svg)

#### Enso Compiler

- [Updated Enso engine to version 0.2.27][1811]. If you're interested in the
  enhancements and fixes made to the Enso compiler, you can find their release
  notes [here](https://github.com/enso-org/enso/blob/develop/RELEASES.md).

[1811]: https://github.com/enso-org/ide/pull/1811

# Enso 2.0.0-alpha.12 (2021-08-13)

<br/>![New Features](/docs/assets/tags/new_features.svg)

#### Visual Environment

- [Improvements to visualization handling][1804]. These improvements are fixing
  possible performance issues around attaching and detaching visualizations.
- [GeoMap visualization will ignore points with `null` coordinates][1775]. Now
  the presence of such points in the dataset will not break initial map
  positioning.

#### Enso Compiler

- [Updated Enso engine to version 0.2.26][1801]. If you're interested in the
  enhancements and fixes made to the Enso compiler, you can find their release
  notes [here](https://github.com/enso-org/enso/blob/develop/RELEASES.md).

[1801]: https://github.com/enso-org/ide/pull/1801
[1775]: https://github.com/enso-org/ide/pull/1775
[1798]: https://github.com/enso-org/ide/pull/1798
[1804]: https://github.com/enso-org/ide/pull/1804

# Enso 2.0.0-alpha.11 (2021-08-09)

This update contains major performance improvements and exposes new privacy user
settings. We will work towards stabilizing it in the next weeks in order to make
these updates be shipped in a stable release before the end of the year.

<br/>![New Features](/docs/assets/tags/new_features.svg)

#### Visual Environment

- [New look of open project dialog][1700]. Now it has a "Open project" title at
  the top.
- [Documentation coments are displayed next to the nodes.][1744].

#### Enso Compiler

- [Updated Enso engine to version 0.2.22][1762]. If you are interested in the
  enhancements and fixes made to the Enso compiler, you can find out more
  details in
  [the engine release notes](https://github.com/enso-org/enso/blob/develop/RELEASES.md).

<br/>![Bug Fixes](/docs/assets/tags/bug_fixes.svg)

#### Visual Environment

- [Fixed a bug where edited node expression was sometimes altered.][1743] When
  editing node expression, the changes were occasionally reverted, or the
  grayed-out parameter names were added to the actual expression. <br/>

[1700]: https://github.com/enso-org/ide/pull/1700
[1742]: https://github.com/enso-org/ide/pull/1742
[1726]: https://github.com/enso-org/ide/pull/1762
[1743]: https://github.com/enso-org/ide/pull/1743
[1744]: https://github.com/enso-org/ide/pull/1744

# Enso 2.0.0-alpha.10 (2021-07-23)

<br/>![New Features](/docs/assets/tags/new_features.svg)

#### Enso Compiler

- [Updated Enso engine to version 0.2.15][1710]. If you're interested in the
  enhancements and fixes made to the Enso compiler, you can find out more
  details in
  [the engine release notes](https://github.com/enso-org/enso/blob/develop/RELEASES.md).

<br/>

[1710]: https://github.com/enso-org/ide/pull/1710

# Enso 2.0.0-alpha.9 (2021-07-16)

<br/>![New Features](/docs/assets/tags/new_features.svg)

#### Visual Environment

- [Improved undo-redo][1653]. Node selection, enabling/disabling visualisations
  and entering a node are now affected by undo/redo and are restored on project
  startup.

<br/>

[1640]: https://github.com/enso-org/ide/pull/1653

# Enso 2.0.0-alpha.8 (2021-06-09)

<br/>![New Features](/docs/assets/tags/new_features.svg)

#### Enso Compiler

- [Updated Enso engine to version 0.2.12][1640]. If you're interested in the
  enhancements and fixes made to the Enso compiler, you can find out more
  details in
  [the engine release notes](https://github.com/enso-org/enso/blob/develop/RELEASES.md).

[1640]: https://github.com/enso-org/ide/pull/1640

<br/>

# Enso 2.0.0-alpha.7 (2021-06-06)

<br/>![New Features](/docs/assets/tags/new_features.svg)

#### Visual Environment

- [User Authentication][1653]. Users can sign in to Enso using Google, GitHub or
  email accounts.

<br/>![Bug Fixes](/docs/assets/tags/bug_fixes.svg)

#### Visual Environment

- [Fix node selection bug ][1664]. Fix nodes not being deselected correctly in
  some circumstances. This would lead to nodes moving too fast when dragged
  [1650] or the internal state of the project being inconsistent [1626].

[1653]: https://github.com/enso-org/ide/pull/1653
[1664]: https://github.com/enso-org/ide/pull/1664

<br/>

# Enso 2.0.0-alpha.6 (2021-06-28)

<br/>![New Features](/docs/assets/tags/new_features.svg)

#### Visual Environment

- [Profling mode.][1546] The IDE contains a profiling mode now which can be
  entered through a button in the top-right corner or through the keybinding
  <kbd>ctrl</kbd>+<kbd>p</kbd>. This mode does not display any information yet.
  In the future, it will display the running times of nodes and maybe more
  useful statistics.
- [Area selection][1588]. You can now select multiple nodes at once. Just click
  and drag on the background of your graph and see the beauty of the area
  selection appear.
- [Opening projects in application graphical interface][1587]. Press `cmd`+`o`
  to bring the list of projects. Select a project on the list to open it.
- [Initial support for undo-redo][1602]. Press <kbd>cmd</kbd>+<kbd>z</kbd> to
  undo last action and <kbd>cmd</kbd>+<kbd>z</kbd> to redo last undone action.
  This version of undo redo does not have proper support for text editor and
  undoing UI changes (like selecting nodes).

#### EnsoGL (rendering engine)

<br/>![Bug Fixes](/docs/assets/tags/bug_fixes.svg)

#### Visual Environment

- [Nodes in graph no longer overlap panels][1577]. The Searcher, project name,
  breadcrumbs and status bar are displayed "above" nodes.

#### Enso Compiler

[1588]: https://github.com/enso-org/ide/pull/1588
[1577]: https://github.com/enso-org/ide/pull/1577
[1587]: https://github.com/enso-org/ide/pull/1587
[1602]: https://github.com/enso-org/ide/pull/1602
[1602]: https://github.com/enso-org/ide/pull/1664
[1602]: https://github.com/enso-org/ide/pull/1650
[1602]: https://github.com/enso-org/ide/pull/1626

# Enso 2.0.0-alpha.5 (2021-05-14)

<br/>![New Features](/docs/assets/tags/new_features.svg)

#### Visual Environment

- [Create New Project action in Searcher][1566]. When you bring the searcher
  with tab having no node selected, a new action will be available next to the
  examples and code suggestions: `Create New Project`. When you choose it by
  clicking with mouse or selecting and pressing enter, a new unnamed project
  will be created and opened in the application. Then you can give a name to
  this project.
- [Signed builds.][1366] Our builds are signed and will avoid warnings from the
  operating system about being untrusted.

#### EnsoGL (rendering engine)

- [Components for picking numbers and ranges.][1524]. We now have some internal
  re-usable UI components for selecting numbers or a range. Stay tuned for them
  appearing in the IDE.

<br/>![Bug Fixes](/docs/assets/tags/bug_fixes.svg)

#### Visual Environment

- [Delete key will delete selected nodes][1538]. Only the non-intuitive
  backspace key was assigned to this action before.
- [It is possible to move around after deleting a node with a selected
  visualization][1556]. Deleting a node while its attached visualization was
  selected made it impossible to pan or zoom around the stage afterwards. This
  error is fixed now.
- [Fixed an internal error that would make the IDE fail on some browser.][1561].
  Instead of crashing on browser that don't support the feature we use, we are
  now just start a little bit slower.

#### Enso Compiler

- [Updated Enso engine to version 0.2.11][1541].

If you're interested in the enhancements and fixes made to the Enso compiler,
you can find their release notes
[here](https://github.com/enso-org/enso/blob/develop/RELEASES.md).

[1366]: https://github.com/enso-org/ide/pull/1366
[1541]: https://github.com/enso-org/ide/pull/1541
[1538]: https://github.com/enso-org/ide/pull/1538
[1524]: https://github.com/enso-org/ide/pull/1524
[1556]: https://github.com/enso-org/ide/pull/1556
[1561]: https://github.com/enso-org/ide/pull/1561
[1566]: https://github.com/enso-org/ide/pull/1566

<br/>

# Enso 2.0.0-alpha.4 (2021-05-04)

<br/>![New Features](/docs/assets/tags/new_features.svg)

#### Visual Environment

- [Window management buttons.][1511]. The IDE now has components for
  "fullscreen" and "close" buttons. They will when running IDE in a cloud
  environment where no native window buttons are available.
- [Customizable backend options][1531]. When invoking Enso IDE through command
  line interface, it is possible to add the `--` argument separator. All
  arguments following the separator will be passed to the backend.
- [Added `--verbose` parameter][1531]. If `--verbose` is given as command line
  argument, the IDE and the backend will produce more detailed logs.

<br/>![Bug Fixes](/docs/assets/tags/bug_fixes.svg)

#### Visual Environment

- [Some command line arguments were not applied correctly in the IDE][1536].
  Some arguments were not passed correctly to the IDE leading to erroneous
  behavior or appearance of the electron app. This is now fixed.

#### Enso Compiler

If you're interested in the enhancements and fixes made to the Enso compiler,
you can find their release notes
[here](https://github.com/enso-org/enso/blob/develop/RELEASES.md).

[1511]: https://github.com/enso-org/ide/pull/1511
[1536]: https://github.com/enso-org/ide/pull/1536
[1531]: https://github.com/enso-org/ide/pull/1531

<br/>

# Enso 2.0.0-alpha.3 (2020-04-13)

<br/>![New Learning Resources](/docs/assets/tags/new_learning_resources.svg)

<br/>![New Features](/docs/assets/tags/new_features.svg)

#### Visual Environment

- [The status bar reports connectivity issues][1316]. The IDE maintains a
  connection to the Enso Language Server. If this connection is lost, any
  unsaved and further work will be lost. In this build we have added a
  notification in the status bar to signal that the connection has been lost and
  that the IDE must be restarted. In future, the IDE will try to automatically
  reconnect.
- [Visualizations can now be maximised to fill the screen][1355] by selecting
  the node and pressing space twice. To quit this view, press space again.
- [Visualizations are previewed when you hover over an output port.][1363] There
  is now a quick preview for visualizations and error descriptions. Hovering
  over a node output will first show a tooltip with the type information and
  then, after some time, will show the visualization of the node. This preview
  visualization will be located above other nodes, whereas the normal view, will
  be shown below nodes. Errors will show the preview visualization immediately.
  Nodes without type information will also show the visualization immediately.
  You can enter a quick preview mode by pressing ctrl (or command on macOS),
  which will show the preview visualization immediately when hovering above a
  node's output port.
- [Database Visualizations][1335]. Visualizations for the Database library have
  been added. The Table visualization now automatically executes the underlying
  query to display its results as a table. In addition, the SQL Query
  visualization allows the user to see the query that is going to be run against
  the database.
- [Histogram and Scatter Plot now support Dataframes.][1377] The `Table` and
  `Column` datatypes are properly visualized. Scatter Plot can display points of
  different colors, shapes and sizes, all as defined by the data within the
  `Table`.
- [Many small visual improvements.][1419] See the source issue for more details.
- The dark theme is officially supported now. You can start the IDE with the
  `--theme=dark` option to enable it.
- You can hide the node labels with the `--no-node-labels` option. This is
  useful when creating demo videos.
- [Added a Heatmap visualization.][1438] Just as for the Scatter Plot, it
  supports visualizing `Table`, but also `Vector`.
- [Add a background to the status bar][1447].
- [Display breadcrumbs behind nodes and other objects][1471].
- [Image visualization.][1367]. Visualizations for the Enso Image library. Now
  you can display the `Image` type and a string with an image encoded in base64.
  The histogram visualization has been adjusted, allowing you to display the
  values of the precomputed bins, which is useful when the dataset is relatively
  big, and it's cheaper to send the precomputed bins rather than the entire
  dataset.
- [Output type labels.][1427] The labels, that show the output type of a node on
  hover, appear now in a fixed position right below the node, instead of a
  pop-up, as they did before.

<br/>![Bug Fixes](/docs/assets/tags/bug_fixes.svg)

#### Visual Environment

- [Not adding spurious imports][1209]. Fixed cases where the IDE was adding
  unnecessary library imports when selecting hints from the node searcher. This
  makes the generated textual code much easier to read, and reduces the
  likelihood of accidental name collisions.
- [Hovering over an output port shows a pop-up with the result type of a
  node][1312]. This allows easy discovery of the result type of a node, which
  can help with both debugging and development.
- [Visualizations can define the context for preprocessor evaluation][1291].
  Users can now decide which module's context should be used for visualization
  preprocessor. This allows providing visualizations with standard library
  functionalities or defining utilities that are shared between multiple
  visualizations.
- [Fixed an issue with multiple instances of the IDE running.][1314] This fixes
  an issue where multiple instances of the IDE (or even other applications)
  could lead to the IDE not working.
- [Allow JS to log arbitrary objects.][1313] Previously using `console.log` in a
  visualisation or during development would crash the IDE. Now it correctly logs
  the string representation of the object. This is great for debugging custom
  visualizations.
- [Fix the mouse cursor offset on systems with fractional display
  scaling][1064]. The cursor now works with any display scaling, instead of
  there being an offset between the visible cursor and the cursor selection.
- [Disable area selection][1318]. The area selection was visible despite being
  non-functional. To avoid confusion, area selection has been disabled until it
  is [correctly implemented][479].
- [Fix an error after adding a node][1332]. Sometimes, after picking a
  suggestion, the inserted node was spuriously annotated with "The name could
  not be found" error.
- [Handle syntax errors in custom-defined visualizations][1341]. The IDE is now
  able to run properly, even if some of the custom visualizations inside a
  project contain syntax errors.
- [Fix issues with pasting multi-line text into single-line text fields][1348].
  The line in the copied text will be inserted and all additional lines will be
  ignored.
- [Users can opt out of anonymous data gathering.][1328] This can be done with
  the `--no-data-gathering` command-line flag when starting the IDE.
- [Provide a theming API for JavaScript visualizations][1358]. It is now
  possible to use the Enso theming engine while developing custom visualizations
  in JavaScript. You can query it for all IDE colors, including the colors used
  to represent types.
- [You can now start the IDE service without a window again.][1353] The command
  line argument `--no-window` now starts all the required backend services
  again, and prints the port on the command line. This allows you to open the
  IDE in a web browser of your choice.
- [JS visualizations have gestures consistent with the IDE][1291]. Panning and
  zooming now works just as expected using both a trackpad and mouse.
- [Running `watch` command works on first try.][1395]. Running the build command
  `run watch` would fail if it was run as the first command on a clean
  repository. This now works.
- [The `inputType` field of visualizations is actually taken into
  consideration][1384]. The visualization chooser shows only the entries that
  work properly for the node's output type.
- [Fix applying the output of the selected node to the expression of a new
  node][1385]. For example, having selected a node with `Table` output and
  adding a new node with expression `at "x" == "y"`, the selected node was
  applied to the right side of `==`: `at "x" == operator1."y"` instead of
  `operator1.at "x" == "y"`.
- [`Enso_Project.data` is visible in the searcher][1393].
- [The Geo Map visualization recognizes columns regardless of the case of their
  name][1392]. This allows visualizing tables with columns like `LONGITUDE` or
  `Longitude`, where previously only `longitude` was recognized.
- [It is possible now to switch themes][1390]. Additionally, the theme manager
  was integrated with the FRP event engine, which has been a long-standing issue
  in the IDE. Themes management was exposed to JavaScript with the
  `window.theme` variable. It is even possible to change and develop themes live
  by editing theme variables directly in the Chrome Inspector. Use the following
  command to give this a go:
  `theme.snapshot("t1"); theme.get("t1").interactiveMode()`.
- [The active visualization is highlighted.][1412] Now it is clearly visible
  when the mouse events are passed to the visualization.
- [Fixed an issue where projects containing certain language constructs failed
  to load.][1413]
- [Fixed a case where IDE could lose connection to the backend after some
  time.][1428]
- [Improved the performance of the graph editor, particularly when opening a
  project for the first time.][1445]

#### EnsoGL (rendering engine)

- [Unified shadow generation][1411]. Added a toolset to create shadows for
  arbitrary UI components.

#### Enso Compiler

If you're interested in the enhancements and fixes made to the Enso compiler,
you can find their release notes
[here](https://github.com/enso-org/enso/blob/develop/RELEASES.md#enso-0210-2021-04-07).

[1064]: https://github.com/enso-org/ide/pull/1064
[1209]: https://github.com/enso-org/ide/pull/1209
[1291]: https://github.com/enso-org/ide/pull/1291
[1311]: https://github.com/enso-org/ide/pull/1311
[1313]: https://github.com/enso-org/ide/pull/1313
[1314]: https://github.com/enso-org/ide/pull/1314
[1316]: https://github.com/enso-org/ide/pull/1316
[1318]: https://github.com/enso-org/ide/pull/1318
[1328]: https://github.com/enso-org/ide/pull/1328
[1355]: https://github.com/enso-org/ide/pull/1355
[1332]: https://github.com/enso-org/ide/pull/1332
[1341]: https://github.com/enso-org/ide/pull/1341
[1341]: https://github.com/enso-org/ide/pull/1341
[1348]: https://github.com/enso-org/ide/pull/1348
[1353]: https://github.com/enso-org/ide/pull/1353
[1395]: https://github.com/enso-org/ide/pull/1395
[1363]: https://github.com/enso-org/ide/pull/1363
[1384]: https://github.com/enso-org/ide/pull/1384
[1385]: https://github.com/enso-org/ide/pull/1385
[1390]: https://github.com/enso-org/ide/pull/1390
[1392]: https://github.com/enso-org/ide/pull/1392
[1393]: https://github.com/enso-org/ide/pull/1393
[479]: https://github.com/enso-org/ide/issues/479
[1335]: https://github.com/enso-org/ide/pull/1335
[1358]: https://github.com/enso-org/ide/pull/1358
[1377]: https://github.com/enso-org/ide/pull/1377
[1411]: https://github.com/enso-org/ide/pull/1411
[1412]: https://github.com/enso-org/ide/pull/1412
[1419]: https://github.com/enso-org/ide/pull/1419
[1413]: https://github.com/enso-org/ide/pull/1413
[1428]: https://github.com/enso-org/ide/pull/1428
[1438]: https://github.com/enso-org/ide/pull/1438
[1367]: https://github.com/enso-org/ide/pull/1367
[1445]: https://github.com/enso-org/ide/pull/1445
[1447]: https://github.com/enso-org/ide/pull/1447
[1471]: https://github.com/enso-org/ide/pull/1471
[1511]: https://github.com/enso-org/ide/pull/1511

<br/>

# Enso 2.0.0-alpha.2 (2020-03-04)

This is a release focused on bug-fixing, stability, and performance. It improves
the performance of workflows and visualizations, and improves the look and feel
of the graphical interface. In addition, the graphical interface now informs the
users about errors and where they originate.

<br/>![New Learning Resources](/docs/assets/tags/new_learning_resources.svg)

- [Learn how to define custom data visualizations in
  Enso][podcast-custom-visualizations].
- [Learn how to use Java libraries in Enso, to build a
  webserver][podcast-java-interop].
- [Learn how to use Javascript libraries in Enso, to build custom server-side
  website rendering][podcast-http-server].
- [Discover why Enso Compiler is so fast and how it was built to support a
  dual-representation language][podcast-compiler-internals].
- [Learn more about the vision behind Enso and about its planned
  future][podcast-future-of-enso].

<br/>![New Features](/docs/assets/tags/new_features.svg)

#### Visual Environment

- [Errors in workflows are now displayed in the graphical interface][1215].
  Previously, these errors were silently skipped, which was non-intuitive and
  hard to understand. Now, the IDE displays both dataflow errors and panics in a
  nice and descriptive fashion.
- [Added geographic map support for Tables (data frames).][1187] Tables that
  have `latitude`, `longitude`, and optionally `label` columns can now be shown
  as points on a map.
- [Added a shortcut for live reloading of visualization files.][1190] This
  drastically improves how quickly new visualizations can be tested during their
  development. This is _currently_ limited in that, after reloading
  visualization definitions, the currently visible visualizations must be
  switched to another and switched back to refresh their content. See the [video
  podcast about building custom visualizations][podcast-custom-visualizations]
  to learn more.
- [Added a visual indicator of the ongoing standard library compilation][1264].
  Currently, each time IDE is started, the backend needs to compile the standard
  library before it can provide IDE with type information and values. Because of
  that, not all functionalities are ready to work directly after starting the
  IDE. Now, there is a visible indication of the ongoing background process.
- [Added the ability to reposition visualisations.][1096] There is now an icon
  in the visualization action bar that allows dragging the visualization away
  from a node. Once the visualization has been moved, another icon appears that
  can pin the visualization back to the node.
- [There is now an API to show Version Control System (like Git) status for
  nodes][1160].

<br/>![Bug Fixes](/docs/assets/tags/bug_fixes.svg)

#### Visual Environment

- [You can now use the table visualization to display data frames][1181]. Please
  note, that large tables will get truncated to 2000 entries. This limitation
  will be lifted in future releases.
- [Performance improvements during visual workflow][1067]. Nodes added with the
  searcher will have their values automatically assigned to newly generated
  variables, which allows the Enso Engine to cache intermediate values and hence
  improve visualization performance.
- [Minor documentation rendering fixes][1098]. Fixed cases where text would be
  misinterpreted as a tag, added support for new tag types, added support for
  more common characters, properly renders overflowing text.
- [Improved handling of projects created with other IDE versions][1214]. The IDE
  is now better at dealing with incompatible metadata in files, which stores
  node visual position information, the history of chosen searcher suggestions,
  etc. This will allow IDE to correctly open projects that were created using a
  different IDE version and prevent unnecessary loss of metadata.
- Pressing and holding up and down arrow keys make the list view selection move
  continuously.
- The shortcuts to close the application and to toggle the developer tools at
  runtime now work on all supported platforms.
- [The loading progress indicator remains visible while IDE initializes][1237].
  Previously the loading progress indicator completed too quickly and stopped
  spinning before the IDE was ready. Now it stays active, giving a visual
  indication that the initialization is still in progress.
- [Fixed visual glitch where a node's text was displayed as white on a white
  background][1264]. Most notably this occurred with the output node of a
  function generated using the node collapse refactoring.
- Many visual glitches were fixed, including small "pixel-like" artifacts
  appearing on the screen.
- [Several parser improvements][1274]. The parser used in the IDE has been
  updated to the latest version. This resolves several issues with language
  constructs like `import`, lambdas, and parentheses, whereupon typing certain
  text the edit could be automatically reverted.
- [The auto-import functionality was improved][1279]. Libraries' `Main` modules
  are omitted in expressions inserted by the searcher. For example, the `point`
  method of `Geo` library will be displayed as `Geo.point` and will insert
  import `Geo` instead of `Geo.Main`.
- Cursors in text editors behave correctly now (they are not affected by scene
  pan and zoom). This was possible because of the new multi-camera management
  system implemented in EnsoGL.
- [Fixed method names highlighted in pink.][1408] There was a bug introduced
  after one of the latest Engine updates, that sent `Unresolved_symbol` types,
  which made all methods pink. This is fixed now.

#### EnsoGL (rendering engine)

- A new multi-camera management system, allowing the same shape systems to be
  rendered on different layers from different cameras. The implementation
  automatically caches the same shape system definitions per scene layer in
  order to minimize the amount of WebGL draw calls and hence improve
  performance.
- A new depth-ordering mechanism for symbols and shapes. It is now possible to
  define depth order dependencies between symbols, shapes, and shape systems.
- Various performance improvements, especially for the text rendering engine.
- Display objects handle visibility correctly now. Display objects are not
  visible by default and need to be attached to a visible parent to be shown on
  the screen.

#### Enso Compiler

If you're interested in the enhancements and fixes made to the Enso compiler,
you can find their release notes
[here](https://github.com/enso-org/enso/blob/develop/RELEASES.md#enso-026-2021-03-02).

[1067]: https://github.com/enso-org/ide/pull/1067
[1096]: https://github.com/enso-org/ide/pull/1096
[1098]: https://github.com/enso-org/ide/pull/1098
[1181]: https://github.com/enso-org/ide/pull/1181
[1215]: https://github.com/enso-org/ide/pull/1215
[1160]: https://github.com/enso-org/ide/pull/1160
[1190]: https://github.com/enso-org/ide/pull/1190
[1187]: https://github.com/enso-org/ide/pull/1187
[1068]: https://github.com/enso-org/ide/pull/1068
[1214]: https://github.com/enso-org/ide/pull/1214
[1237]: https://github.com/enso-org/ide/pull/1237
[1264]: https://github.com/enso-org/ide/pull/1264
[1274]: https://github.com/enso-org/ide/pull/1274
[1279]: https://github.com/enso-org/ide/pull/1279
[podcast-java-interop]:
  https://www.youtube.com/watch?v=bcpOEX1x06I&t=468s&ab_channel=Enso
[podcast-compiler-internals]:
  https://www.youtube.com/watch?v=BibjcUjdkO4&ab_channel=Enso
[podcast-custom-visualizations]:
  https://www.youtube.com/watch?v=wFkh5LgAZTs&t=5439s&ab_channel=Enso
[podcast-http-server]:
  https://www.youtube.com/watch?v=BYUAL4ksEgY&ab_channel=Enso
[podcast-future-of-enso]:
  https://www.youtube.com/watch?v=rF8DuJPOfTs&t=1863s&ab_channel=Enso
[1312]: https://github.com/enso-org/ide/pull/1312
[1408]: https://github.com/enso-org/ide/pull/1408

<br/>

# Enso 2.0.0-alpha.1 (2020-01-26)

This is the first release of Enso, a general-purpose programming language and
environment for interactive data processing. It is a tool that spans the entire
stack, going from high-level visualization and communication to the nitty-gritty
of backend services, all in a single language.

<br/>![Release Notes](/docs/assets/tags/release_notes.svg)

#### Anonymous Data Collection

Please note that this release collects anonymous usage data which will be used
to improve Enso and prepare it for a stable release. We will switch to opt-in
data collection in stable version releases. The usage data will not contain your
code (expressions above nodes), however, reported errors may contain brief
snippets of out of context code that specifically leads to the error, like "the
method 'foo' does not exist on Number". The following data will be collected:

- Session length.
- Graph editing events (node create, dele, position change, connect, disconnect,
  collapse, edit start, edit end). This will not include any information about
  node expressions used.
- Navigation events (camera movement, scope change).
- Visualization events (visualization open, close, switch). This will not
  include any information about the displayed data nor the rendered
  visualization itself.
- Project management events (project open, close, rename).
- Errors (IDE crashes, WASM panics, Project Manager errors, Language Server
  errors, Compiler errors).
- Performance statistics (minimum, maximum, average GUI refresh rate).
