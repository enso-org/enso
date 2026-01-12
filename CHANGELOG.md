# Next Release

#### Enso IDE

- [Add component alignment options][14590].

[14590]: https://github.com/enso-org/enso/pull/14590

#### Enso Standard Library

- [Multiline if_then_else][14522].
- [Using dual JVM mode for Standard.Microsoft][14476].
- [Standard.Test pending field is lazy][14536].
- [Using dual JVM mode for Standard.AWS][14568].
- [Polishing Standard.Test API][14599].

[14522]: https://github.com/enso-org/enso/pull/14522
[14476]: https://github.com/enso-org/enso/pull/14476
[14536]: https://github.com/enso-org/enso/pull/14536
[14568]: https://github.com/enso-org/enso/pull/14568
[14599]: https://github.com/enso-org/enso/pull/14599

#### Enso Language & Runtime

- [`Panic.rethrow` keeps original location][14480]
- [Use `State.get if_missing` to avoid too frequent `Panic.throw`][14490]
- [Lazily initialized local variables with Ref.memoize][14554].
- [Flush system caches via Runtime.gc][14557]

[14480]: https://github.com/enso-org/enso/pull/14480
[14490]: https://github.com/enso-org/enso/pull/14490
[14536]: https://github.com/enso-org/enso/pull/14554
[14557]: https://github.com/enso-org/enso/pull/14557

# Enso 2025.3

#### Enso IDE

- [Resizing the right panel no longer causes content reflow][13976].
- [Allow selecting target type for Any.to method][13685].
- [Command Palette to search for arbitrary actions][13658]
- [Fix mouse interactions with JSON visualization][13971]
- [There is no need for adding `--no-sandbox` flag in Ubuntu anymore][13990].
- [The documentation panel opens to the scroll position at last close][13647]
- [Autocompletion for Column methods in table expressions][13797]
- [Autocompletion for Column names in table expressions][13848]
- [When connecting to port a value with additional type, a necessary type cast
  is included][14028]
- [Function docs in autocomplete in table expressions][14059]
- [Many CLI arguments removed][14069]
- [Support opening project by clicking on link in documentation][14136]
- [Images may be added to assets descriptions][14247]
- [Dragging edges from plus button on nodes is now possible][14246]
- [JSON and SQL visualizations' content may be now selected and copied][14262]
- [SQL visualization displays interpolated parameters properly][14262]
- [Added immediate rename of user created components][14209]
- [Multiple opened projects' tabs are now allowed][14215]
- [Required arguments now have an arrow displayed on top of the port][14270]
- [Connections between components may be selected with click][14311] and deleted
  with <kbd>Delete</kbd> key.
- [Improved visuals of high components][14267]. Their contents is aligned to
  top.
- [Context menu for connections][14325].
- [Warnings and Errors no longer become transparent][14388]
- [`--headless` flag to run a project without the User Interface][14310]
- ["Delete and Connect Around" option in node's menu][14403]
- [GeoMap visualization is now working without need of Mapbox Token in
  environment][14429]
- [Fix non-visible button and wrong layout in GeoMap Visualization][14443]
- [Added additonal file and site options for MS365 credentials][14477]
- [Maximum height of the file browser is slightly reduced][14467]
- [Fix mispositioned bottom panel][14506]
- ["Paste component" option in graph menu][14504]
- [Fix bug where Table Expressions weren't saved after finishing edit by mouse
  click][14500]
- [Fix "What's new", "Documentation" and similar buttons opening external page
  twice][14507]

[13685]: https://github.com/enso-org/enso/pull/13685
[13658]: https://github.com/enso-org/enso/pull/13658
[13971]: https://github.com/enso-org/enso/pull/13971
[13990]: https://github.com/enso-org/enso/pull/13990
[13647]: https://github.com/enso-org/enso/pull/13647
[13797]: https://github.com/enso-org/enso/pull/13797
[13848]: https://github.com/enso-org/enso/pull/13848
[14028]: https://github.com/enso-org/enso/pull/14028
[13976]: https://github.com/enso-org/enso/pull/13976
[14059]: https://github.com/enso-org/enso/pull/14059
[14069]: https://github.com/enso-org/enso/pull/14069
[14136]: https://github.com/enso-org/enso/pull/14136
[14247]: https://github.com/enso-org/enso/pull/14247
[14262]: https://github.com/enso-org/enso/pull/14262
[14246]: https://github.com/enso-org/enso/pull/14246
[14209]: https://github.com/enso-org/enso/pull/14209
[14215]: https://github.com/enso-org/enso/pull/14215
[14270]: https://github.com/enso-org/enso/pull/14270
[14310]: https://github.com/enso-org/enso/pull/14310
[14311]: https://github.com/enso-org/enso/pull/14311
[14267]: https://github.com/enso-org/enso/pull/14267
[14325]: https://github.com/enso-org/enso/pull/14325
[14388]: https://github.com/enso-org/enso/pull/14388
[14403]: https://github.com/enso-org/enso/pull/14403
[14429]: https://github.com/enso-org/enso/pull/14429
[14443]: https://github.com/enso-org/enso/pull/14443
[14477]: https://github.com/enso-org/enso/pull/14477
[14467]: https://github.com/enso-org/enso/pull/14467
[14506]: https://github.com/enso-org/enso/pull/14506
[14504]: https://github.com/enso-org/enso/pull/14504
[14500]: https://github.com/enso-org/enso/pull/14500
[14507]: https://github.com/enso-org/enso/pull/14507

#### Enso Standard Library

- [Implement `Text_Column` for in-memory backend.][13769]
- [Implement `Text_Column.to_case` for in-memory backend.][13769]
- [Add support for gzip encoded return from web APIs.][14026]
- [Fixes for JDBC connection and schema support for SQL Server.][14039]
- [Change Column.length to Column.row_count.][14085]
- [Add Text_Column.length and Text_Column.char_length.][14091]
- [Add EDI_Format and support for reading ANSI X12 EDI files.][14158]
- [Add support for reading xlsb Excel file format][14157]
- [Add Text_Column.upper and Text_Column.lower.][14179]
- [Microsoft 365 OAuth support.][14135]
- [Add Text_Column.proper and Rename Case.Title->Case.Proper.][14184]
- [Snowflake stage support for reading and writing files.][14210]
- [Snowflake file format and copy into support.][14221]
- [Snowflake bulk load API.][14230]
- [Add error_on_missing_columns to more methods.][14236]
- [Add `OneDrive_File` to Microsoft 365 implementation.][14237]
- [Progress of `Runtime.sleep` visualized.][14275]
- [Add `Email.send`][14258]
- [Full DuckDB Dialect.][14298]
- [Initial Spatial support within DuckDB.][14331]
- [Add email SMTP support.][14350]
- [Read files into DuckDB both spatial and not.][14367]
- [Implement Text_Column to_case for DB backends.][14386]
- [Implement bulk loading to DuckDB.][14402]
- [Implement `Text_Column.text_mid` for in-memory and database backends.][14420]
- [Initial file writing from DuckDB.][14421]
- [Parquet file reading and writing, DuckDB formats.][14427]
- [Add Text_Column.index_of][14428]
- [Trigonometry and other maths function on Column.][14433]
- [Implement `Text_Column.left, right, and mid` for in-memory and database
  backends.][14420]
- [Support for reading JSON lines files.][14439]
- [Add Custom SQL to in database aggregates.][14472]
- [Add spatial functions and `write_spatial_file` to DuckDB.][14488]
- [Add spatial function support to expressions.][14492]
- [Add `Date_Column` class, and `first_of_month` and `end_of_month`.][14485]
- [Add `OneDrive.write` allowing a first API to write to OneDrive.][14494]
- [Add `Grouping_Method.Equal_Sum` for database backends.][14528]
- [Add `Grouping_Method.Standard_Deviation` for database backends.][14546]

[13769]: https://github.com/enso-org/enso/pull/13769
[14026]: https://github.com/enso-org/enso/pull/14026
[14039]: https://github.com/enso-org/enso/pull/14039
[14085]: https://github.com/enso-org/enso/pull/14085
[14091]: https://github.com/enso-org/enso/pull/14091
[14135]: https://github.com/enso-org/enso/pull/14135
[14158]: https://github.com/enso-org/enso/pull/14158
[14157]: https://github.com/enso-org/enso/pull/14157
[14179]: https://github.com/enso-org/enso/pull/14179
[14184]: https://github.com/enso-org/enso/pull/14184
[14210]: https://github.com/enso-org/enso/pull/14210
[14221]: https://github.com/enso-org/enso/pull/14221
[14230]: https://github.com/enso-org/enso/pull/14230
[14236]: https://github.com/enso-org/enso/pull/14236
[14237]: https://github.com/enso-org/enso/pull/14237
[14275]: https://github.com/enso-org/enso/pull/14275
[14258]: https://github.com/enso-org/enso/pull/14258
[14298]: https://github.com/enso-org/enso/pull/14298
[14331]: https://github.com/enso-org/enso/pull/14331
[14350]: https://github.com/enso-org/enso/pull/14350
[14367]: https://github.com/enso-org/enso/pull/14367
[14386]: https://github.com/enso-org/enso/pull/14386
[14402]: https://github.com/enso-org/enso/pull/14402
[14420]: https://github.com/enso-org/enso/pull/14420
[14421]: https://github.com/enso-org/enso/pull/14421
[14427]: https://github.com/enso-org/enso/pull/14427
[14428]: https://github.com/enso-org/enso/pull/14428
[14433]: https://github.com/enso-org/enso/pull/14433
[14420]: https://github.com/enso-org/enso/pull/14420
[14439]: https://github.com/enso-org/enso/pull/14439
[14472]: https://github.com/enso-org/enso/pull/14472
[14485]: https://github.com/enso-org/enso/pull/14485
[14488]: https://github.com/enso-org/enso/pull/14488
[14492]: https://github.com/enso-org/enso/pull/14492
[14494]: https://github.com/enso-org/enso/pull/14494
[14528]: https://github.com/enso-org/enso/pull/14528
[14546]: https://github.com/enso-org/enso/pull/14528

#### Enso Language & Runtime

- [Special handling of if ... then ... else construct][11365]
- [Enso is "conversion and equality oriented" language][14133]
- [Moving >, >=, <, <= to types where such operators make sense][14017]
- [Moving warning releated methods outside of `Any`][13978]
- [Moving error relelated methods outside of `Any`][14003]
- [Defining to_text & co. as extension methods][14050]
- [Register and `lookup_services` in package.yaml][11868]
- [Open type check `Type&Any` lets all visible types thru][13225]
- [Removal of --no-global-cache option][13909]
- [Launching ydoc-server together with language-server][13178]
- [Autocompletion for table expression builtin functions in table
  expressions][13914]
- [Autocompletion for table expression operators and operands][13917]
- [Update to GraalVM 25.0.1][14233]
- [Apply block argument to non-application expression][14249]

[11365]: https://github.com/enso-org/enso/pull/11365
[14133]: https://github.com/enso-org/enso/pull/14133
[14017]: https://github.com/enso-org/enso/pull/14017
[14003]: https://github.com/enso-org/enso/pull/14003
[13978]: https://github.com/enso-org/enso/pull/13978
[14050]: https://github.com/enso-org/enso/pull/14050
[11868]: https://github.com/enso-org/enso/pull/11868
[13225]: https://github.com/enso-org/enso/pull/13225
[13909]: https://github.com/enso-org/enso/pull/13909
[13178]: https://github.com/enso-org/enso/pull/13178
[13914]: https://github.com/enso-org/enso/pull/13914
[13917]: https://github.com/enso-org/enso/pull/13917
[14233]: https://github.com/enso-org/enso/pull/14233
[14249]: https://github.com/enso-org/enso/pull/14249

# Enso 2025.2

#### Enso IDE

- [Add syntax highlighting for Table expressions][12778]
- [Allow adding and reordering component group arguments][12850]
- [Display component evaluation progress][12913]
- [Editing multiline text literals is now supported][12774]
- [Type annotations are now visible in the graph editor][12751]
- [Component Browser shows methods with respect to possible type casts][12751]
- [Add option to browse cloud for secret values][12953]
- [Add option to create a new secret in the graph editor's cloud browser][12985]
- [Allow editing grouped component argument names.][13014]
- [Add ability to inspect column, row and value from right click on table
  viz][12986]
- [Add option to browse cloud for folders][13117]
- [File Browser Widget: Add ability to filter files by extension][13048]
- [Add keyboard shortcuts for formatting documentation][13134]
- [New right-side panel][13135], unified between tabs.
- [Allow selecting expected types for arguments of grouped components.][13161]
- [Component documentation now uses Markdown instead of a custom Markdown-like
  format][13203]
- [Methods for ‘intersection’ types are now visible in Component
  Browser.][13266]
- [Allow marking grouped component arguments as required or providing a default
  value.][13254]
- [New Right Panel Tab with Markdown Description Editor][13347]
- [Panic on unresolved type checks in the IDE][13467]
- [Fixed a bug, where "Free plan" user was redirected to Cloud directory,
  resulting in an Error page without option of returning back to Local.][13366]
- ["Grouped Components" are renamed to "User Defined Components"][13389]
- [Allow displaying cloud images using enso:// url in documentation][13419]
- [File Browser Widget is used when editing file paths to datalinks][13439]
- [When creating a node using the button on the port, the button can be dragged
  to set its location][13598]
- [Graph is not moved when showing/resizing side panels.][13557]
- [Add "Invite" button to the top bar when using a team or higher plan][13522]
- ["Welcome Project" is automatically opened for new users][13479]
- [Project and Setting tab may be now closed with shortcut][13604]. On
  Windows/Linux <kbd>Ctrl</kbd>+<kbd>W</kbd> or <kbd>Ctrl</kbd> + <kbd>F4</kbd>;
  on macOS: <kbd>⌘</kbd> + <kbd>W</kbd>.
- [Improved Graph Editor's Top Bar responsiveness to width change.][13726]
- [Changed animation of tabs highlight.][13726]
- [Fixed hybrid project progress being lost in some circumstances][14066]
- [Add rand, uuid, tomorrow, yesterday and randbetween][14071]

[12774]: https://github.com/enso-org/enso/pull/12774
[12778]: https://github.com/enso-org/enso/pull/12778
[12850]: https://github.com/enso-org/enso/pull/12850
[12913]: https://github.com/enso-org/enso/pull/12913
[12751]: https://github.com/enso-org/enso/pull/12751
[12953]: https://github.com/enso-org/enso/pull/12953
[12985]: https://github.com/enso-org/enso/pull/12985
[13014]: https://github.com/enso-org/enso/pull/13014
[12986]: https://github.com/enso-org/enso/pull/12986
[13117]: https://github.com/enso-org/enso/pull/13117
[13048]: https://github.com/enso-org/enso/pull/13048
[13134]: https://github.com/enso-org/enso/pull/13134
[13135]: https://github.com/enso-org/enso/pull/13135
[13161]: https://github.com/enso-org/enso/pull/13161
[13203]: https://github.com/enso-org/enso/pull/13203
[13266]: https://github.com/enso-org/enso/pull/13266
[13254]: https://github.com/enso-org/enso/pull/13254
[13347]: https://github.com/enso-org/enso/pull/13347
[13467]: https://github.com/enso-org/enso/pull/13467
[13366]: https://github.com/enso-org/enso/pull/13366
[13389]: https://github.com/enso-org/enso/pull/13389
[13419]: https://github.com/enso-org/enso/pull/13419
[13439]: https://github.com/enso-org/enso/pull/13439
[13598]: https://github.com/enso-org/enso/pull/13598
[13557]: https://github.com/enso-org/enso/pull/13557
[13522]: https://github.com/enso-org/enso/pull/13522
[13479]: https://github.com/enso-org/enso/pull/13479
[13604]: https://github.com/enso-org/enso/pull/13604
[13726]: https://github.com/enso-org/enso/pull/13726
[14066]: https://github.com/enso-org/enso/pull/14066
[14071]: https://github.com/enso-org/enso/pull/14071

#### Enso Standard Library

- [Support for reading fixed-width-column data files.][12726]
- [Added `row_limit` parameter to the `Fixed_Width` format.][12950]
- [Add Tableau Hyper write support][12900]
- [Support character encodings when reading fixed-width files.][13138]
- [Initial Microsoft Azure support][13144]
- [Column inference for fixed-width files.][13240]
- [Specify fixed-width file layout with a `Vector` or `Column` of
  widths.][13240]
- [Use `Filter_Condition.Predicate` to provide custom condition.][13460]
- [Support `skip_rows` and `on_empty_field` for fixed-width files.][13240]
- [Add Google_Sheets.read][13307]
- [Align the Generic JDBC Connection with the main Connection type][13365]
- [Add `execute_query` to the `Connection` types.][13415]
- [`Meta.meta` recognizes functions as `Meta.Function`][13443]
- [`Meta.Unresolved_Symbol` renamed to `Meta.Unresolved`][13443]
- [Remane Google_Api library to Google][13436]
- [Data.read_many now returns the read path as a char field][13475]
- [Add prototype `find_group_number` function to Table][13487]
- [Add support for custom and empty line endings to the fixed-width file format]
  [13554]
- [In-memory table `add_group_number` supports `Equal_Sum`.][13819]
- [In-memory table `add_group_number` supports `Standard_Deviation`.][13895]
- [Added `Data.fetch_many` to allow throttle fetching of APIs.][13999]

[12726]: https://github.com/enso-org/enso/pull/12726
[12950]: https://github.com/enso-org/enso/pull/12950
[12900]: https://github.com/enso-org/enso/pull/12900
[13138]: https://github.com/enso-org/enso/pull/13138
[13144]: https://github.com/enso-org/enso/pull/13144
[13240]: https://github.com/enso-org/enso/pull/13240
[13307]: https://github.com/enso-org/enso/pull/13307
[13460]: https://github.com/enso-org/enso/pull/13460
[13365]: https://github.com/enso-org/enso/pull/13365
[13415]: https://github.com/enso-org/enso/pull/13415
[13415]: https://github.com/enso-org/enso/pull/13443
[13436]: https://github.com/enso-org/enso/pull/13436
[13443]: https://github.com/enso-org/enso/pull/13443
[13475]: https://github.com/enso-org/enso/pull/13475
[13487]: https://github.com/enso-org/enso/pull/13487
[13554]: https://github.com/enso-org/enso/pull/13554
[13819]: https://github.com/enso-org/enso/pull/13819
[13895]: https://github.com/enso-org/enso/pull/13895
[13999]: https://github.com/enso-org/enso/pull/13999

#### Enso Language & Runtime

- [Allow optional path for `--jvm` option of `project-manager`][13225]
- [Prevent `Meta` access to private constructors and private fields][12905]
- [Encapsulating Private_Access constructor][12976]
- [Upgrading Truffle][12500] (including its
  [JavaScript](https://www.graalvm.org/javascript/) and
  [Python](https://www.graalvm.org/python/)) to version `24.2.0`.
- [Upgrade GraalVM from JDK 21 to JDK 24][12855]
- [Use JAVA_TOOL_OPTIONS env variable to alter JVM arguments][13256]
- [Check private access at resolution, not on invocation][13392]

[12500]: https://github.com/enso-org/enso/pull/12500
[12976]: https://github.com/enso-org/enso/pull/12976
[12855]: https://github.com/enso-org/enso/pull/12855
[12905]: https://github.com/enso-org/enso/pull/12905
[13225]: https://github.com/enso-org/enso/pull/13225
[13256]: https://github.com/enso-org/enso/pull/13256
[13392]: https://github.com/enso-org/enso/pull/13392

# Enso 2025.1

#### Enso IDE

- [ENSO_IDE_MAPBOX_API_TOKEN environment variable should be provided to enable
  GeoMap visualization][11889].
- [Round ‘Add component’ button under the component menu replaced by a small
  button protruding from the output port.][11836].
- [Quick Fix Import Button][12051].
- [Fixed nodes being selected after deleting other nodes or connections.][11902]
- [Redo stack is no longer lost when interacting with text literals][11908].
- [Copy button on error message is fixed][12133].
- [Tooltips are hidden when clicking on a button][12067].
- [Fixed bug when clicking header in Table Editor Widget didn't start editing
  it][12064]
- [When editing cells or header names in Table Editor Widget, `tab` and `enter`
  keys jumps to next cell/ next row respectively.][12129]
- [Fixed bugs occurring after renaming project from within graph editor][12106].
- [Users having "Team" plan or above may now access shared directories in Cloud
  File Browser][12208]
- [Added support for rendering numbered and nested lists][12190].
- [Added buttons for editing top-level markdown elements in the documentation
  panel][12217].
- [Removed `#` from default colum name][12222]
- [Cloud File Browser will display input for file name in components writing to
  (new) files.][12228]
- [Cloud File Browser allows adding new directory in "writing"
  components][12275]
- [In the documentation panel, text can now be made bold or italic using
  buttons][12341]
- [Cloud File Browser allows renaming existing directories in "writing"
  components][12323]
- [New Component Browser displaying list of groups][12386]
- ["Insert link" button added to documentation panel][12365]
- [Cloud File Browser, when opened first time after opening project, shows and
  highlights the currently set file][12184]
- [It's easier to write numeric/text nodes in Component Browser][12420]. When
  typing digits only, any names containing digits are not the best match
  anymore. Also unclosed text literals will be automatically closed.
- [Use server side filtering and sorting in table viz][12272]. Table viz rows
  are lazly loaded and filtering/sorting is done serverside improving experience
  for large datasets.
- [Disable heatmap and histogram viz][12475].
- [Component Browser displays short summary of component's documentation][12459]
- [Fixed color picker for selected nodes][12508]
- [Updated Top Bar actions menu and made zoom controls always visible.][12496]
- [Warning and Error messages does not obstruct output port][12482] - they
  become semi-transparent on port hover and pass all mouse interactions.
- [For some types, Component Browser display special "suggestions" group][12477]
- [Nested dropdowns are supported for Selection widget.][12548]
- [Native Image mode is now on by default][12515]
- [join_kind dropdown has icons to show how joins work][12502]
- [Output port ‘plus’ button is hidden if there are existing outgoing
  connections][12576]
- [Navigate up button for file browser and collapsed components][12603]
- [Resizing visualization in Table.input component resizes the table widget
  too][12643]
- [File Browser Widget warns when trying to override an existing file][12644]
- [Add right-click context menu to the graph editor background][12601]
- [Fixed appearance of Cloud Browser scrollbars][12831]

[11889]: https://github.com/enso-org/enso/pull/11889
[11836]: https://github.com/enso-org/enso/pull/11836
[12051]: https://github.com/enso-org/enso/pull/12051
[11902]: https://github.com/enso-org/enso/pull/11902
[11908]: https://github.com/enso-org/enso/pull/11908
[12133]: https://github.com/enso-org/enso/pull/12133
[12067]: https://github.com/enso-org/enso/pull/12067
[12064]: https://github.com/enso-org/enso/pull/12064
[12129]: https://github.com/enso-org/enso/pull/12129
[12106]: https://github.com/enso-org/enso/pull/12106
[12208]: https://github.com/enso-org/enso/pull/12208
[12190]: https://github.com/enso-org/enso/pull/12190
[12222]: https://github.com/enso-org/enso/pull/12222
[12228]: https://github.com/enso-org/enso/pull/12228
[12217]: https://github.com/enso-org/enso/pull/12217
[12275]: https://github.com/enso-org/enso/pull/12275
[12341]: https://github.com/enso-org/enso/pull/12341
[12323]: https://github.com/enso-org/enso/pull/12323
[12386]: https://github.com/enso-org/enso/pull/12386
[12365]: https://github.com/enso-org/enso/pull/12365
[12184]: https://github.com/enso-org/enso/pull/12184
[12420]: https://github.com/enso-org/enso/pull/12420
[12272]: https://github.com/enso-org/enso/pull/12272
[12475]: https://github.com/enso-org/enso/pull/12475
[12459]: https://github.com/enso-org/enso/pull/12459
[12508]: https://github.com/enso-org/enso/pull/12508
[12496]: https://github.com/enso-org/enso/pull/12496
[12482]: https://github.com/enso-org/enso/pull/12482
[12477]: https://github.com/enso-org/enso/pull/12477
[12548]: https://github.com/enso-org/enso/pull/12548
[12515]: https://github.com/enso-org/enso/pull/12515
[12502]: https://github.com/enso-org/enso/pull/12502
[12576]: https://github.com/enso-org/enso/pull/12576
[12603]: https://github.com/enso-org/enso/pull/12603
[12643]: https://github.com/enso-org/enso/pull/12643
[12644]: https://github.com/enso-org/enso/pull/12644
[12601]: https://github.com/enso-org/enso/pull/12603
[12831]: https://github.com/enso-org/enso/pull/12831

#### Enso Standard Library

- [Allow using `/` to access files inside a directory reached through a data
  link.][11926]
- [Reducing helper methods in `Standard.Base.Meta`.][12031]
- [Added Table.offset][12071]
- [Implemented Generic JDBC connections.][12073]
- [Added Column.offset][12092]
- [Progress API][12163]
- [When reading a Delimited file, if a row with more columns than expected is
  encountered, extra columns can be added to the result.][12231]
  - In `Delimited` format, the `keep_invalid_rows` setting has been renamed to
    `on_invalid_rows`. The default behaviour was also changed to add any extra
    columns instead of discarding them.
- [Added DB_Table.offset for SQLServer][12206]
- [Added DB_Table.offset for Snowflake, Postgres, SQLite][12251]
- [Support for key-pair authentication in Snowflake connector.][12247]
- [Support for generic JDBC connections through external drivers.][12300]
- [Support for basic arithmetic operations as numbers in Expressions.][12297]
- [Support for Regular Expressions in Enso Expressions.][12320]
- [Support for pi() and e() in Enso Expressions.][12367]
- [xlsx reader now does not read empty rows from the end of a worksheet][12345]
- [Generic JDBC connections can be created with `Database.connect`.][12331]
- [Added Table.generate_rows][12413]
- [Added Regex_match to filter. Added Column.regex_match. Support for
  regex_match in the expression language][12492]
- [Added `add_group_number` for Postgres and SQLite.][12574]
- [Added `add_group_number` for Snowflake and SQLServer.][12590]
- [Added `skip_nothing` and `report_unmatched` arguments to `Vector.zip`][12626]
- [Changed in-memory `Column.zip` to match `Table.zip`, removing the `function`
  parameter][12626]
- [Added Regex_match for Postgres][12663]
- [Added Regex_match for Snowflake][12671]
- [Replace `use_bankers` flag with `Rounding_Mode` in all `round`
  methods][12641]

[11926]: https://github.com/enso-org/enso/pull/11926
[12031]: https://github.com/enso-org/enso/pull/12031
[12071]: https://github.com/enso-org/enso/pull/12071
[12073]: https://github.com/enso-org/enso/pull/12073
[12092]: https://github.com/enso-org/enso/pull/12092
[12163]: https://github.com/enso-org/enso/pull/12163
[12231]: https://github.com/enso-org/enso/pull/12231
[12206]: https://github.com/enso-org/enso/pull/12206
[12251]: https://github.com/enso-org/enso/pull/12251
[12247]: https://github.com/enso-org/enso/pull/12247
[12300]: https://github.com/enso-org/enso/pull/12300
[12297]: https://github.com/enso-org/enso/pull/12297
[12320]: https://github.com/enso-org/enso/pull/12320
[12331]: https://github.com/enso-org/enso/pull/12331
[12367]: https://github.com/enso-org/enso/pull/12367
[12345]: https://github.com/enso-org/enso/pull/12345
[12413]: https://github.com/enso-org/enso/pull/12413
[12492]: https://github.com/enso-org/enso/pull/12492
[12574]: https://github.com/enso-org/enso/pull/12574
[12590]: https://github.com/enso-org/enso/pull/12590
[12626]: https://github.com/enso-org/enso/pull/12626
[12663]: https://github.com/enso-org/enso/pull/12663
[12671]: https://github.com/enso-org/enso/pull/12671
[12641]: https://github.com/enso-org/enso/pull/12641

#### Enso Language & Runtime

- [Promote broken values instead of ignoring them][11777].
- [Intersection types & type checks][11600]
- A constructor or type definition with a single inline argument definition was
  previously allowed to use spaces in the argument definition without
  parentheses. [This is now a syntax error.][11856]
- [Native libraries of projects can be added to `polyglot/lib` directory][11874]
- [Prefer module methods over `Any` instance ones][12048]
- [Keep intersection type's self when dispatching Any instance methods][12170]
- [Types without constructors can be public][12052]
- Symetric, transitive and reflexive [equality for intersection types][11897]
- [IR definitions are generated by an annotation processor][11770]
- [Use fn... to reference any module function][12128]
- [Improve error message for mismatched named argument application][12238]
- [Registering a value as multiple managed resources is now an error.][12395]
- [An operator block now applies to the whole preceding expression][12505],
  rather than the last term on the line.

[11777]: https://github.com/enso-org/enso/pull/11777
[11600]: https://github.com/enso-org/enso/pull/11600
[11856]: https://github.com/enso-org/enso/pull/11856
[11874]: https://github.com/enso-org/enso/pull/11874
[12048]: https://github.com/enso-org/enso/pull/12048
[12170]: https://github.com/enso-org/enso/pull/12170
[12052]: https://github.com/enso-org/enso/pull/12052
[11897]: https://github.com/enso-org/enso/pull/11897
[11770]: https://github.com/enso-org/enso/pull/11770
[12128]: https://github.com/enso-org/enso/pull/12128
[12238]: https://github.com/enso-org/enso/pull/12238
[12395]: https://github.com/enso-org/enso/pull/12395
[12505]: https://github.com/enso-org/enso/pull/12505

# Enso 2024.5

#### Enso IDE

- [Rows and Columns may be now removed in Table Input Widget][11151]. The option
  is available in right-click context menu.
- [Rows and Columns may be now reordered by dragging in Table Input
  Widget][11271]
- [Copying and pasting in Table Editor Widget now works properly][11332]
- [Fix invisible selection in Table Input Widget][11358]
- [Enable cloud file browser in local projects][11383]
- [Changed the way of adding new column in Table Input Widget][11388]. The
  "virtual column" is replaced with an explicit (+) button.
- [New dropdown-based component menu][11398].
- [Methods defined on Standard.Base.Any type are now visible on all
  components][11451].
- [Undo/redo buttons in the top bar][11433].
- [Size of Table Input Widget is preserved and restored after project
  re-opening][11435]
- [Added application version to the title bar.][11446]
- [Added "open grouped components" action to the context menu.][11447]
- [Table Input Widget has now a limit of 256 cells.][11448]
- [Added an error message screen displayed when viewing a deleted
  component.][11452]
- [New documentation editor provides improved Markdown editing experience, and
  paves the way for new documentation features.][11469]
- [You can now add images to documentation panel][11547] by pasting them from
  clipboard or by drag'n'dropping image files.
- ["Write" button in component menu allows to evaluate it separately from the
  rest of the workflow][11523].
- [The documentation editor can now display tables][11564]
- [The documentation editor supports the Markdown URL syntax, and uses it to
  render pasted URLs as links][11597]
- [Table Input Widget is now matched for Table.input method instead of
  Table.new. Values must be string literals, and their content is parsed to the
  suitable type][11612].
- [Added dedicated function signature viewer and editor in the right-side
  panel][11655].
- [Visualizations on components are slightly transparent when not
  focused][11582].
- [New design for vector-editing widget][11620]
- [The component menu can be opened by right-click; supports operations on
  multiple components; has a 'Copy Component' button][11690]
- [New design for vector-editing widget][11620].
- [Default values on widgets are displayed in italic][11666].
- [Fixed bug causing Table Visualization to show wrong data][11684].
- [Pasting tabular data now creates Table.input expressions][11695].
- [No halo is displayed around components when hovering][11715].
- [The hover area of the component output port extended twice its size][11715].
- [The documentation editor and comment documentation support opening links with
  a key pressed, or via a popup when editing][11753].
- [Fixed a rare bug where the component position wasn't persisted after closing
  project][11761]
- [In the table visualization and table widget, the table context menu can now
  be opened on OS X][11755].
- [Fix some UI elements drawing on top of visualization toolbar dropdown
  menus][11768].
- [Edges are now colored based on their source component.][11810]
- [Highlight missing required arguments][11803].
- [Arrows in some drop-down buttons are now clearly visible][11800]

[11151]: https://github.com/enso-org/enso/pull/11151
[11271]: https://github.com/enso-org/enso/pull/11271
[11332]: https://github.com/enso-org/enso/pull/11332
[11358]: https://github.com/enso-org/enso/pull/11358
[11383]: https://github.com/enso-org/enso/pull/11383
[11388]: https://github.com/enso-org/enso/pull/11388
[11398]: https://github.com/enso-org/enso/pull/11398
[11451]: https://github.com/enso-org/enso/pull/11451
[11433]: https://github.com/enso-org/enso/pull/11433
[11435]: https://github.com/enso-org/enso/pull/11435
[11446]: https://github.com/enso-org/enso/pull/11446
[11447]: https://github.com/enso-org/enso/pull/11447
[11448]: https://github.com/enso-org/enso/pull/11448
[11452]: https://github.com/enso-org/enso/pull/11452
[11469]: https://github.com/enso-org/enso/pull/11469
[11547]: https://github.com/enso-org/enso/pull/11547
[11523]: https://github.com/enso-org/enso/pull/11523
[11564]: https://github.com/enso-org/enso/pull/11564
[11582]: https://github.com/enso-org/enso/pull/11582
[11597]: https://github.com/enso-org/enso/pull/11597
[11612]: https://github.com/enso-org/enso/pull/11612
[11655]: https://github.com/enso-org/enso/pull/11655
[11582]: https://github.com/enso-org/enso/pull/11582
[11620]: https://github.com/enso-org/enso/pull/11620
[11666]: https://github.com/enso-org/enso/pull/11666
[11690]: https://github.com/enso-org/enso/pull/11690
[11684]: https://github.com/enso-org/enso/pull/11684
[11695]: https://github.com/enso-org/enso/pull/11695
[11715]: https://github.com/enso-org/enso/pull/11715
[11753]: https://github.com/enso-org/enso/pull/11753
[11755]: https://github.com/enso-org/enso/pull/11755
[11761]: https://github.com/enso-org/enso/pull/11761
[11768]: https://github.com/enso-org/enso/pull/11768
[11810]: https://github.com/enso-org/enso/pull/11810
[11803]: https://github.com/enso-org/enso/pull/11803
[11800]: https://github.com/enso-org/enso/pull/11800

#### Enso Standard Library

- [The `enso://~` path now resolves to user's home directory in the
  cloud.][11235]
- [The user may set description and labels of an Enso Cloud asset
  programmatically.][11255]
- [DB_Table may be saved as a Data Link.][11371]
- [Support for dates before 1900 in Excel and signed AWS requests.][11373]
- [Added `Data.read_many` that allows to read a list of files in a single
  operation.][11490]
- [Added `Table.input` allowing creation of typed tables from vectors of data,
  including auto parsing text columns.][11562]
- [Enhance Managed_Resource to allow implementation of in-memory caches][11577]
- [Added `add_group_number` to the in-memory database.[11818]
- [The reload button clears the HTTP cache.][11673]
- [SQL Server Support for Aggregate][11811]
- [Added `Download_Mode` parameter to `Data.download`.][12017]
- [Added `Table.geo_distance` to calculate the distance between two
  points.][12393]
- [The reload button clears the Enso Cloud request cache.][12526]
- [The reload button clears the AuthenticationProvider, EnsoSecretReader and
  AuditLog caches.][12541]

[11235]: https://github.com/enso-org/enso/pull/11235
[11255]: https://github.com/enso-org/enso/pull/11255
[11371]: https://github.com/enso-org/enso/pull/11371
[11373]: https://github.com/enso-org/enso/pull/11373
[11490]: https://github.com/enso-org/enso/pull/11490
[11562]: https://github.com/enso-org/enso/pull/11562
[11577]: https://github.com/enso-org/enso/pull/11577
[11818]: https://github.com/enso-org/enso/pull/11818
[11673]: https://github.com/enso-org/enso/pull/11673
[11811]: https://github.com/enso-org/enso/pull/11811
[12017]: https://github.com/enso-org/enso/pull/12017
[12393]: https://github.com/enso-org/enso/pull/12393
[12526]: https://github.com/enso-org/enso/pull/12526
[12541]: https://github.com/enso-org/enso/pull/12526

#### Enso Language & Runtime

- [Arguments in constructor definitions may now be on their own lines][11374]
- [The `:` type operator can now be chained][11671].

[11374]: https://github.com/enso-org/enso/pull/11374
[11671]: https://github.com/enso-org/enso/pull/11671

# Enso 2024.4

#### Enso IDE

- [Table Editor Widget][10774] displayed in `Table.new` component.
- [New design of Component Browser][10814] - the component list is under the
  input and shown only in the initial "component browsing" mode - in this mode
  the entire input is a filtering pattern (it is not interpreted as parts of
  code). After picking any suggestion with Tab or new button the mode is
  switched to "code editing", where visualization preview is displayed instead.
  Also the component browser help is now displayed in the right-side dock panel.
- [Drilldown for XML][10824]
- [Fixed issue where switching edited widget with <kbd>tab</kbd> key did not
  updated actual code][10857]
- [Added fullscreen modes to documentation editor and code editor][10876]
- [Fixed issue with node name assignment when uploading multiple files.][10979]
- [Cloud file browser inserts `enso:` paths][11001]
- [Fixed issue where drag'n'dropped files were not uploaded in cloud
  projects.][11014]
- [Fixed files associations not properly registered on Windows][11030]
- [Input components corresponding to function arguments are now
  displayed.][11165]
- [Fixed "rename project" button being broken after not changing project
  name][11103]
- [Numbers starting with dot (`.5`) are accepted in Numeric Widget][11108]
- [Add support for interacting with graph editor using touch devices.][11056]

[10774]: https://github.com/enso-org/enso/pull/10774
[10814]: https://github.com/enso-org/enso/pull/10814
[10824]: https://github.com/enso-org/enso/pull/10824
[10857]: https://github.com/enso-org/enso/pull/10857
[10876]: https://github.com/enso-org/enso/pull/10876
[10979]: https://github.com/enso-org/enso/pull/10979
[11001]: https://github.com/enso-org/enso/pull/11001
[11014]: https://github.com/enso-org/enso/pull/11014
[11030]: https://github.com/enso-org/enso/pull/11030
[11165]: https://github.com/enso-org/enso/pull/11165
[11103]: https://github.com/enso-org/enso/pull/11103
[11108]: https://github.com/enso-org/enso/pull/11108
[11056]: https://github.com/enso-org/enso/pull/11056

#### Enso Standard Library

- [Implemented in-memory and database mixed `Decimal` column
  comparisons.][10614]
- [Relative paths are now resolved relative to the project location, also in the
  Cloud.][10660]
- [Added Newline option to Text_Cleanse/Text_Replace.][10761]
- [Support for reading from Tableau Hyper files.][10733]
- [Mixed Decimal/Float arithmetic now throws an error; mixed comparisons now
  attach warnings.][10725]
- [Support for creating Atoms in expressions.][10820]
- [IO.print without new line][10858]
- [Add `Text.to_decimal`.][10874]
- [Added `floor`, `ceil`, `trunc` to the in-memory `Decimal` column.][10887]
- [Added vectorized .round to the in-memory `Decimal` column.][10912]
- [`select_into_database_table` no longer defaults the primary key to the first
  column.][11120]
- [Extend the range of `floor`, `ceil`, `trunc` to values outside the `Long`
  range.][11135]
- [Added `format` parameter to `Decimal.parse`.][11205]
- [Added `format` parameter to `Float.parse`.][11229]
- [Implemented a cache for HTTP data requests, as well as a per-file response
  size limit.][11342]
- [Overhauled Google Analytics APIs.][11484]

[10614]: https://github.com/enso-org/enso/pull/10614
[10660]: https://github.com/enso-org/enso/pull/10660
[10761]: https://github.com/enso-org/enso/pull/10761
[10733]: https://github.com/enso-org/enso/pull/10733
[10725]: https://github.com/enso-org/enso/pull/10725
[10820]: https://github.com/enso-org/enso/pull/10820
[10858]: https://github.com/enso-org/enso/pull/10858
[10874]: https://github.com/enso-org/enso/pull/10874
[10887]: https://github.com/enso-org/enso/pull/10887
[10912]: https://github.com/enso-org/enso/pull/10912
[11120]: https://github.com/enso-org/enso/pull/11120
[11135]: https://github.com/enso-org/enso/pull/11135
[11205]: https://github.com/enso-org/enso/pull/11205
[11229]: https://github.com/enso-org/enso/pull/11229
[11342]: https://github.com/enso-org/enso/pull/11342
[11484]: https://github.com/enso-org/enso/pull/11484

#### Enso Language & Runtime

- [Print out warnings associated with local variables][10842]

[10842]: https://github.com/enso-org/enso/pull/10842

# Enso 2024.3

#### Enso Language & Runtime

- [Enforce conversion method return type][10468]
- [Renaming launcher executable to ensoup][10535]
- [Space-precedence does not apply to value-level operators][10597]
- [Must specify `--repl` to enable debug server][10709]
- [Improved parser error reporting and performance][10734]

[10468]: https://github.com/enso-org/enso/pull/10468
[10535]: https://github.com/enso-org/enso/pull/10535
[10597]: https://github.com/enso-org/enso/pull/10597
[10709]: https://github.com/enso-org/enso/pull/10709
[10734]: https://github.com/enso-org/enso/pull/10734

#### Enso IDE

- ["Add node" button is not obscured by output port][10433]
- [Numeric Widget does not accept non-numeric input][10457]. This is to prevent
  node being completely altered by accidental code put to the widget.
- [Redesigned "record control" panel][10509]. Now it contains more intuitive
  "refresh" and "write all" buttons.
- [Warning messages do not obscure visualization buttons][10546].
- [Output component in collapsed function changed][10577]. It cannot be deleted
  anymore, except by directily editing the code.
- [Improved handling of spacing around rounded node widgets][10599], added
  support for widgets of arbitrary sizes.
- [Multiselect drop-down widget visuals are improved][10607].
- [Text displayed in monospace and whitespace rendered as symbols][10563].

[10433]: https://github.com/enso-org/enso/pull/10443
[10457]: https://github.com/enso-org/enso/pull/10457
[10509]: https://github.com/enso-org/enso/pull/10509
[10546]: https://github.com/enso-org/enso/pull/10546
[10577]: https://github.com/enso-org/enso/pull/10577
[10599]: https://github.com/enso-org/enso/pull/10599
[10607]: https://github.com/enso-org/enso/pull/10607
[10563]: https://github.com/enso-org/enso/pull/10563

#### Enso Standard Library

- [Renamed `Data.list_directory` to `Data.list`. Removed list support from read
  methods.][10434]
- [Renamed `Location.Start` to `Location.Left` and `Location.End` to
  `Location.Right`.][10445]
- [Renamed `Postgres_Details.Postgres` to `Postgres.Server`.][10466]
- [Remove `First` and `Last` from namespace, use auto-scoped.][10467]
- [Rename `Map` to `Dictionary` and `Set` to `Hashset`.][10474]
- [Compare two objects with `Ordering.compare` and define comparator with
  `Comparable.new`][10468]
- [Added `dec` construction function for creating `Decimal`s.][10517]
- [Added initial read support for SQLServer][10324]
- [Upgraded SQLite to version 3.46.1.][10911]

[10434]: https://github.com/enso-org/enso/pull/10434
[10445]: https://github.com/enso-org/enso/pull/10445
[10466]: https://github.com/enso-org/enso/pull/10466
[10467]: https://github.com/enso-org/enso/pull/10467
[10474]: https://github.com/enso-org/enso/pull/10474
[10517]: https://github.com/enso-org/enso/pull/10517
[10324]: https://github.com/enso-org/enso/pull/10324
[10911]: https://github.com/enso-org/enso/pull/10911

# Enso 2024.2

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
- [Copied table-viz range pastes as Table component][10352]
- [Added support for links in documentation panels][10353].
- [Added support for opening documentation in an external browser][10396].
- Added a [cloud file browser][10513].

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
[10352]: https://github.com/enso-org/enso/pull/10352
[10353]: https://github.com/enso-org/enso/pull/10353
[10396]: https://github.com/enso-org/enso/pull/10396
[10513]: https://github.com/enso-org/enso/pull/10513

#### Enso Language & Runtime

- Support for [explicit --jvm option][10374] when launching `enso` CLI

[10374]: https://github.com/enso-org/enso/pull/10374

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
- [Implemented `Decimal` support for Postgres backend.][10216]

[debug-shortcuts]:

[9950]: https://github.com/enso-org/enso/pull/9950
[10122]: https://github.com/enso-org/enso/pull/10122
[10130]: https://github.com/enso-org/enso/pull/10130
[10206]: https://github.com/enso-org/enso/pull/10206
[10190]: https://github.com/enso-org/enso/pull/10190
[10323]: https://github.com/enso-org/enso/pull/10323
[10372]: https://github.com/enso-org/enso/pull/10372
[10216]: https://github.com/enso-org/enso/pull/10216

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
