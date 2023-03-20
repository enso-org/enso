# Summary

This document provides a guide on how to achieve and maintain high-performance
evaluation of Enso (IDE, Engine, Language Server, and other services). It is
divided into the following chapters:

- [**Profiling**](#epic-1-profiler-implementation)  
  Discovering performance bottlenecks and understanding their causes.
- [**Performance goals**](#epic-1-performance-goals-and-reference-hardware)  
  Desired performance and comparison to other products.
- [**Performance improvements plan**](#epic-2-performance-improvements-planning)  
  Strategies on improving performance of the selected components.
- [**Continuous performance monitoring**](#epic-3-continuous-performance-monitoring)  
  Maintenance of high performance of all components and early discovery of new
  performance issues.

# Profiling

To understand, improve, and maintain performance of the IDE, we need to be able
to reliably measure the performance of actions it performs.

## Measurables

Measurements we make will fall into one of two categories:

- **Objective** measurements refer to intervals directly observable by the user,
  e.g. _time from opening the app until the loading spinner gets out of the
  way_.
- **Internal** measurements provide insight into how time is spent while
  performing the **objective**s, e.g. _time spent compiling ide.wasm_.

### Objectives to measure

The following **objective**s should be measured by the framework; it is part of
this task to identify the most important user-visible measurements. **Internal**
measurements will be defined as-needed to understand how time is spent
performing **objective**s.

- **App startup**  
  Time between double-clicking the app icon and having functional hello screen
  of recent projects. Sub-processes:

  - **Hello Screen initialization**  
    Time between double-clicking the app icon and displaying the loading
    spinner:
    - **App window initialization**  
      Time between double-clicking the app and displaying the user log-in
      prompt.

- **Project startup**  
  Time from selecting a project from the Hello Screen to a working graph view of
  the project. See also [engine startup](engine-startup.md) for non-IDE details
  on a similar topic. Sub-processes:

  - **Graph Editor initialization**  
    Time needed to display fully-functional graph editor (not colored).
    Profiling should include:
    - Time needed for GUI Backend to receive and process the required
      information from the Engine.
    - FRP events propagation. This should mainly focus on the public Graph
      Editor FRP API (used by GUI backend):
      - Time needed to propagate FRP events through the system.
      - Count of FRP events of a particular type, e.g. how many times a text
        refresh was requested.
    - Time needed to create each node component (model, display object, etc.).
    - FPS statistics (explained later).
  - **Type information propagation**  
    Time needed to color the graph. Profiling should include:
    - Count of FRP events of a particular type. For example, how many times a
      text refresh was requested.
    - Time needed to update each particular node.
    - FPS statistics (explained later).

- **Working with a project**  
  This section should contain various objectives, including:

  - Creating/moving/editing/removing a node.
  - Connecting nodes.
  - Editing expressions (incl. typing a letter).
  - Entering nodes.
  - Opening/changing/closing a visualization.
  - Saving the project.
  - Switching projects.
  - Opening node searcher.
  - Switching selection in node searcher.

- **Idle performance**  
  The profiling statistics for opened project that does not receive any
  user-interactions.

Also, please browse the
[performance-related issues](https://enso.youtrack.cloud/issue/IDE-3209) to
check if they are not describing processes missing from the above list. In
particular, please check the
[IDE Performance Issues issue](https://enso.youtrack.cloud/issue/IDE-2571).

### Measurement relationships

The data model for measurements is hierarchical, to enable use ranging from
high-level overviews to fine-grained analysis of details.

Each of the trees comprising the hierarchy is rooted at an
[objective measurement](#measurables), because we only measure intervals that
contribute to a user-observable delay.

The parent-child relationship of the hierarchy indicates that the child was
started by the parent. Usually, but not always, a task's parent cannot complete
until the child does.

Beside start and end times, the framework logs when tasks are paused or resumed
(e.g. before and after an `.await`); this provides enough information to define
a _current profiler_: The current profiler is the most-recently
started-or-resumed profiler that is not currently paused and has not yet ended.

Some profiling data such as _FRP events_ and _per-frame statistics_ is
implicitly attributed to the current profiler [these terms will be elaborated on
in a future revision of this document].

## Collecting the data

### Interactively

Users may capture profiling information at any time during an interactive
session of the application (usage in debug scenes is not yet supported). The
procedure to collect data is:

- Compile Enso with the desired [profiling level](#profiling-levels) enabled.
  The default (which is used in release builds) is to record only
  [objective measurements](#measurables), which includes information about
  performance, but little detail for identifying causes. A greater level of
  detail can be obtained by passing the `profiling-level` argument to the
  [build script (see "Development" subheading)](https://enso.org/docs/developer/ide/CONTRIBUTING.html);
  for maximum detail, use `--profiling-level=debug`. For example:
  ```shell
  ./run ide build --profiling-level=detail
  ```
- To profile in Electron:
  - Start Enso with the `profile.save` option, e.g.:
    ```shell
    ./distribution/bin/enso --profile.save=your_profile.json
    ```
  - Perform any activity the profiler should record (the profiler is always
    running).
  - Press Ctrl+Alt+Q to save profile (to the path specified earlier) and quit.
- To profile in Chrome:
  - Open Enso in Chrome.
  - Perform any activity the profiler should record (the profiler is always
    running).
  - Press Ctrl+Alt+P to end profile recording.
  - Open the Browser Console by pressing Ctrl+Alt+J (do not do this before
    profiling;
    [having the console open can degrade performance](https://developer.chrome.com/blog/wasm-debugging-2020/#profiling)
    of WASM applications.)
  - Near the end of the Console output, there will be a JSON object printed; it
    will usually be large, and truncated. Click the Copy button below it.
  - Paste the log into a .json file.

### Batch mode

A _batch mode_ supports recording profiling information for predefined workflows
without user-interaction. It can be run as follows:

```shell
# First build with full profiling information.
./run ide build --profiling-level=debug
# Now run the generated binary in batch mode.
./distribution/bin/enso --startup.entry=profile --profile.workflow=create_node --profile.save=create_node.json
```

The `profile` entry point selects batch mode; when it is used, the
`profile.workflow` argument is mandatory. `profile.save` specifies an output
filename; `profile.workflow` selects from a list of defined batch-mode workflows
to measure. Supported workflows include:

- `new_project`: This workflow includes starting the application, opening an
  empty project, and closing the application.
- `collapse_nodes`: Measures selecting and collapsing a group of nodes.
- `create_node`: Measures adding a new node to the graph.
- `enter_collapsed_node`: Measures entering a collapsed node.
- `open_visualization`: Measures enabling a visualization for a node.
- `open_project_orders`: Measures opening the project called `Orders`.

To see a full list of currently-implemented workflows, run the `enso` executable
with the arguments `--entry-point=profile`, and `--workflow=help`. Additional
workflows will be implemented in
[the future](#epic-1-performance-goals-and-reference-hardware).

### The profiling environment

An important consideration when profiling is controlling the environment. In
particular, setting a fixed CPU frequency is important for measurement
consistency (with Turbo Boost enabled I have been seeing up to a 2x difference
between consecutive runs; with it disabled, it is only around 20%). Because
achieving this depends on hardware, it is up to the user to do so; it is
recommended to create a script that automatically disables turbo boost before
running benchmarks rather that do so manually, since a manual approach is
error-prone. On Linux, frequency scaling can be controlled by writing to /sys
values (which values depends on processor); on AWS instances, it can be set with
a kernel command line
(https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/processor_state_control.html#baseline-perf).
In any environment where frequency cannot be disabled (or when running with it
enabled to obtain realistic measurements of reference hardware), high variance
will make it more difficult to detect performance changes.

## Examining the data

### Run-graphs

A "run graph" tool provides a high-level overview of the data in the profile.
The upper part of the view is an icicle plot[^1] showing data that is similar to
a callgraph[^2], but rather than show function scopes it shows profiler scopes
(these often coincide). The lower part of the view, sharing the (horizontal)
time axis, is a graph of async tasks, showing their active and inactive
intervals. [^1]:
https://www.cs.middlebury.edu/~candrews/showcase/infovis_techniques_s16/icicle_plots/icicleplots.html
[^2]: https://en.wikipedia.org/wiki/Call_graph

Invocation:

```shell
./distribution/bin/enso --startup.entry=profiling-run-graph --profile.load=profile.json
```

It takes as input the profiling data representing a single run
([averaged data would not be suitable for graphical representation](#averaging-profile-data)).

### Command-line tools

- `intervals`
  (`cargo run -p enso-profiler-data --bin intervals < profile.json`): This tool
  produces a table showing which profilers spent the most time running.
- `measurements`
  (`cargo run -p enso-profiler-data --bin measurements < profile.json`): This
  tool produces a textual representation of all the data in a profile, including
  the hierarchy and lifetimes of async profilers, sync profilers, and metadata.
  It supports fine-grained examination of profile contents, and `profiler`-based
  debugging.

### Format conversion

- `devtools`
  (`cargo run -p enso-profiler-data --bin devtools < input.json > output.json`):
  This tool converts a profile from Enso's profile format to the format used by
  Chrome Dev Tools, for compatibility with third-party tools.

### Examining frontend-backend timing breakdowns of tasks

Some additional tools support examining how different processes are contributing
to the latency of different tasks, and examining the communication between
processes (`processes` in enso-profiler-data`, and message_beanpoles` in
`enso-profiler-enso-data`).

### Averaging profile data

Given multiple recorded profiles of the same workflow, it is possible to compute
averages of specific measurements; this is essential for reducing variance to
support A/B testing (see [#future-extensions]), above. However, there is no way
to produce a complete "averaged profile" (suitable for visualization) from
multiple individual profiles, for various reasons. The average of multiple
profiles is hard to even define: A particular measurement may repeat a different
number of times in one profile from another. Even ignoring that, an "average"
profile wouldn't necessarily represent a set of timings that could occur in a
real profile: For example, a property may hold in all profiles that Task A and
Task B never occur at the same time; yet if they occur in different orders in
individual profiles, their intervals could overlap in an averaged profile.

## Epic 1: Profiler Implementation

Because some measurements need to start before WASM is initialized, the
profiling framework should provide an implementation with two interfaces – for
JavaScript and Rust. Some API syntax will differ, but both APIs should support
the same functionality; when the data is emitted, profiling entries from
JavaScript and Rust will be merged into one log. There should be no difference
in the behavior between calling APIs in any of the languages.

To minimize performance impact of profiling, instrumentation should do not
require any more time-consuming operations than obtaining a timestamp (with
`performance.now`) and pushing an entry to a buffer. These entries will be
emitted at process exit.

### Profiling levels

Fine-grained profiling (like per-frame rendering profiling) is useful for
understanding the performance characteristics, but it can cause significant
performance overhead. That is why the API should define a few profiling levels:

- **objective**  
  Measurements that correspond directly to aspects of the user experience. An
  **objective** can contain other **objective**s, e.g. _GUI initialization_
  (which might be defined as: time from opening the app until the app is ready
  to receive user input) contains _time until the loading spinner finishes_.
- **task**  
  Coarse-grained tasks, such as app window initialization, GUI downloading, or
  WASM compilation. A **task** can contain other **task**s e.g. GUI
  initialization contains GUI downloading.
- **details**  
  All processes which can be removed in compile-time for the official stable
  release for users. We might provide some users with special releases with
  enabled **detailed** profiling, however, it should be possible to debug and
  understand most of user-provided logs with disabled **details** view.
- **debug**  
  All processes which should be removed in compile-time by default even during
  app development. It applies to every heavy-usage of the profiling framework,
  such as per-frame rendering profiling.

When a profiling level is disabled, all instrumentation associated with that
level is eliminated at compile time. In order to avoid passing real profiler
values down the call stack when they are not needed by enabled profiling, each
type of profiler will conditionally compile to either a real object or a
zero-sized type with no-op implementations. This conditional compilation is
controlled with the `profiling-level` family of Cargo feature flags, and
integrated with the build script, e.g. `./run watch --profiling-level=task`.

### Rust API

The primary interface is a `profile` attribute macro. When applied to a function
definition, it inserts instrumentation to measure the function's entry and exit
times. The measurement is logged under the name of the function, with file/line
information attached.

```rust
// The argument to the macro identifies the profiling level at which this measurement is enabled.
#[profile(Detail)]
fn compute_four() -> u32 {
    2 + 2
}
```

#### Low-level API

A lower-level API is available for special purposes. The `#[profile]` API above
is preferred, especially for non-`async` measurements.

```rust
// Sometimes a block that isn't a whole function may need to be measured; in that case, a RAII
// interface is available.
//
// A trait `profiler::Parent<T>` supports accepting as an argument any object representing a
// profiler that could be a parent of a profiler type T. This includes profilers of the same or
// higher profiling level, and their corresponding RAII objects (described below).
//
// Usage note:
// When the RAII guard is implicitly dropped at the end of a block, it should be created as the
// first line of the block; it measures one full block, and the line containing `start_X!` acts as a
// title for the block.
fn big_computation(prof: impl profiler::Parent<profiler::Task>) {
    match some_condition {
        // When task-level profiling is enabled, the `start_task!` line will create a measurement
        // named "condition_a". The measurement will extend from the line to the end of its
        // enclosing scope, when the __profiler object is dropped.
        Condition::A => {
            let _profiler = profiler::start_task!(prof, "condition_a");

            // If detail-level profiling is also enabled, "small_computation" will log a
            // child of the "condition_a" task.
            let foo = small_computation(_profiler);
            ...
        }
    }
}

// `await` in a context profiled by the low-level API should be done through a wrapper that logs the
// beginning and end of the await interval. This is necessary to have accurate information about
// what tasks are running when.
//
// If this is not done, tools interpreting profiling data will be able to detect the oversight and
// identify the profiler at fault, as long as the callee, or any async task that runs during the
// await period, is correctly profiled.
//
// However if the above detectable case does not occur, and during the await period an unprofiled
// async task starts an implicitly-parented profiled task, then some profiler(s) that should be
// root-level may be interpreted as children of this profiler.
async fn awaiting(prof: impl profiler::Parent<profiler::Task>) {
    profiler::await_!(some_future(), prof);
}

// Sometimes we need multiple measurements from the same start time; to support this pattern, there
// is an interface to create a child profiler that starts at the same time as its parent.
fn same_start_child(prof: impl profiler::Parent<profiler::Task>) {
    let _profiler = profiler::task_with_same_start!(prof, "same_start_child");
    ...
}

// The hierarchy of measurements starts at a const Objective object, APP_LIFETIME.
//
// Since APP_LIFETIME is not a true objective measurement (i.e. we aren't, in general, trying to
// minimize its length), its direct children must be objective measurements, and form the logical
// roots of the measurement hierarchy.
fn a_root_objective() {
    let _profiler = profiler::start_objective!(profiler::APP_LIFETIME, "a_root_objective");
    ...
}

// Some measurements need to start when the app starts (the "time origin" in Web terminology);
// APP_LIFETIME can be used with with_same_start.
fn starts_at_beginning() {
    let _profiler = profiler::objective_with_same_start!(
        profiler::APP_LIFETIME,
        "starts_at_beginning"
    );
    ...
}

// Rarely, a single block may need to be divided into multiple measurements.
// This can be accomplished by creating sub-blocks, and using the same RAII interface as above.
fn multipart_fn(prof: impl profiler::Parent<profiler::Task>) {
    let foo;
    let bar;
    {
        let _profiler = profiler::start_task!(prof, "first_part");

        foo = calculate_foo();
        bar = calculate_bar();
    }
    {
        let _profiler = profiler::start_task!(prof, "second_part");

        do_something(foo, bar);
    }
}

// Occasionally, we need to measure an interval that doesn't start and end in any one scope.
//
// This can be accomplished by storing the RAII object returned by `start_X!` and ensuring it is
// dropped at the time the measurement should end.
//
// In this example, the measurement lasts for the life of the containing `Baz`.
struct Baz {
    ..,
    // profiler::Started<profiler::Objective> holds a profiler::Objective, and a start time.
    // When it is dropped, the end time will be set and the measurement will be logged.
    profiler: profiler::Started<profiler::Objective>,
}
impl Baz {
    fn new(parent: impl profiler::Parent<profiler::Objective>) -> Self {
        let profiler = profiler::start_objective!(parent, "doing Baz");
        Baz { .., profiler }
    }
}
```

### JavaScript API

The API in JavaScript supports the same functionality as the Rust low-level API
above, but is used a bit differently because we can't support RAII in JS.

```js
// Higher-level API, for scoped measurements: Measure a block by passing it as a closure.
await profiler.objective.measure(profiler.APP_LIFETIME, "init-gui", async (subprof) => {
    ...
});

// Lower-level API: Start and end explicitly.
const prof = profiler.objective.start(profiler.APP_LIFETIME, "a root objective");
const subprof = profiler.task.with_same_start(prof, "a subtask");
subprof.end();
prof.end();
```

### Profiling data

#### File format

After profiling data is [captured](#collecting-the-data), it is written to a
file. The data model of this file is an _event log_ that reflects the raw data
collected by the profiling framework. It is a low-level format, considered
internal to the `profiler` crates.

Some properties of the format are important to document. Its data can be divided
into two categories:

- The _interval data_ is information about the _profilers_ themselves: their
  lifetimes, state changes, and relationships.
- _Metadata_ provides information about the application, usually associated with
  specific profilers.

The definitions of the interval data will not change often; metadata is expected
to change as more data-logging is supported. It is desirable that when a
changeset alters a metadata definition, we can still tell whether that changeset
introduces a performance regression in the interval data; more generally, we can
check performance against a baseline even if metadata changes have happened
since the baseline was recorded. To ensure this:

- The definitions of _interval data_ should be as stable as possible.
- The format is designed and implemented such that when a metadata type's
  definition changes, we can still read at least the intervals (We will also
  still be able to read metadata of other types, although this is not
  essential).

In the initial implementation, there is no explicit versioning to detect changes
to the datatype definitions; we rely on the self-describing format (JSON) to
recognize changes. Because of this:

- Developers altering metadata definitions should try not to introduce an
  important change to the semantics of a field without any concurrent structural
  change. (E.g. If a field's meaning has changed, change its name too.)
- Developers interpreting profiling data should be aware that tools may
  misinterpret some metadata if producer and consumer are subtly incompatible
  versions.

#### Measurement API

The `profiler` framework includes a `data` library which interprets the
low-level _event log_ data, and makes the information available through a
higher-level API.

### Open questions

There are open questions that should be answered as part of implementation of
this design idea. Please note, that you should not answer them by yourself.
Instead, you talk with other team members who have broad domain-specific
knowledge in these areas, please. Questions:

- What will [the interface to profiling data](#measurement-api) be?
- What tools will be implemented to support working with profile files? (Need to
  support: comparing mean timings between profiles; including variance between
  sets of profiles)

## Epic 1: Performance Goals and Reference Hardware

To know where we stand in terms of performance, we need to have references. This
means we have to look at how other software behaves and compare how slow/fast
Enso is for _objective_ measures.

### Hardware

The tests should be conducted on the following hardware types:

- The newest MacBook Pro.
- An "average client laptop" without dedicated GPU, which should be a "common"
  hardware used in enterprises nowadays.
- A "minimal-spec laptop".

### Open questions

There are several open questions that should be answered as part of
implementation of this design idea. Please note, that you should not answer them
by yourself. Instead, you talk with other team members who have broad
domain-specific knowledge in these areas, please. Questions:

- What is the exact hardware specification that we should consider "average
  client laptop" and "minimal-spec laptop" described above?
- What is the time needed for other software to perform the _objective_-level
  actions and which software we should compare Enso to (e.g. Alteryx, Knime,
  Tableau, ...).

## Epic 2: Performance improvements planning

Once we have determined our performance goals and created the tools to measure
them, we can focus on improving the most significant performance bottlenecks.
The process would be as follows:

- We will compare the current results with results of other software, and we
  will describe our performance goals.
- We will analyze the profiling information and check which processes need
  improvements in order for the goals to be matched.
- We will create user-stories (tasks) for improving these components, and we
  will plan their implementation. All tasks will become sub-tasks of one
  performance-related epic.

## Epic 3: Continuous performance monitoring

As a last step, we should take what we have learned, and ensure that we do not
introduce regressions in the future. We should add a CI step that fails if we
introduce new performance regressions.

To achieve this we should add a command line interface that makes a reference
report, tests the application, and returns whether the test was worse in any of
the measurements than the reference report (including some margin of error to
avoid spurious failures).

We then add a reference performance report to the build system and compare new
PRs against this report. If they are introducing regression, CI will fail and
the performance of the PR will need to be checked/improved.

Moreover, the CI should generate time reports over time, in a similar fashion to
the [performance reports in Rust](https://perf.rust-lang.org).

![image](https://user-images.githubusercontent.com/1623053/141743832-f55123f4-4d32-4859-8b8b-26df21958200.png)

In order to allow high sensitivity without excess false positives and with a
minimum of required testing resources, we should probably have a way of tagging
issues that may affect specific objective-level measures. For example, the most
expensive measurement to take is time to initialize the app and open a project;
looking at the most recent PRs, most could safely be assumed to have no impact
on startup time. (Of course, it is important to err on the side of safety
here--but I think the chance of an issue having a completely unforeseeable
performance impact is lower than the overhead associated with running this test
for every PR; and if we run it too often, we'll get used to seeing false
positives, which reduces the value of testing).

# Future extensions

## A/B-testing tool

## Additional profiling information

The time profiling is a good start for understanding application performance
metrics, however, it is not enough to analyze causes of bottlenecks, especially
in async-first applications, like graphical user interfaces.

The profiling toolkit should also collect per-frame statistics for the duration
of each of the processes and report their summary, including:

- Graph Editor public API (used by GUI backend) FRP events count, e.g. how many
  times the Graph Editor was asked to create a node or modify an expression.
- Min, max, and average rendering statistics:
  - WASM memory usage.
  - GPU memory usage.
  - Sprites count.
  - Shaders count.
  - ... other statistics currently collected by the GUI performance monitor
    implemented in `gui/src/rust/ensogl/lib/core/src/debug/stats.rs` (you can
    open it by `CMD/CTRL+ALT+<TILDE>`, where `<TILDE>` is the button below the
    escape key and because of a bug, can be different from tilde on some
    keyboard layouts).

Another important type of measurement is cold-start performance: the amount of
time it takes to reach important startup milestones on a freshly-booted machine,
when the app's resources are not yet cached to memory and the processor has not
recently run the process (so no branch prediction data etc. is cached yet).
Because it requires many repeated measurements to control for high variance (and
a reboot for each measurement), this data will take a lot of time to collect; it
is not feasible to run it in regular CI tests. However, it reflects important
aspects of real-world usage, so it is important data to check on a regular
basis. (To be determined: how frequently.)

## Production monitoring

It's important to know how performance benchmarks compare to actual user
experience. In production builds, we could collect objective-level measurements,
aggregate them into statistical reports, and submit them through the
anonymized-data facilities. The details of this are outside the initial scope of
this design.

## More advanced run graphs

Run-graphs could support rendering callgraph data collected by V8 alongside the
measurements generated by our instrumentation.

Run-graphs could support interactive functionality, such as collapsing the
lowest-level profiling data until it is clicked.
