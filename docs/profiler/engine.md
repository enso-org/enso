# Summary

The main objective for the Enso engine (e.g. the backend of the Enso GUI) is to
deliver satisfactory user experience when using Enso via the GUI. When
operations get sluggish, we need tools to analyze what's going on.

### Use JVM Mode

A major obstacle to understanding performance since the 2025.3 version is the
[Dual JVM mode](../infrastructure/dual_jvm.md). While _beneficial_ to overall
system performance, it also _complicates the analysis_ of performance issues.
The heterogeneous nature of the Enso process requires different techniques to
profile the _native image_ part and different to profile the _HotSpot JVM_ part.
That may become quite complicated. To simplify this task **it is recommended to
start** analysis in _JVM only mode_ - e.g. by passing `--jvm` flag to the `enso`
executable, by specifying `jvm: true` in project's `package.yaml` file or by
other means.

Luckily most of the problems are generic enough to manifest themselves in all
the modes Enso is able to operate in. Hence the first recommended step is to
check the _JVM Only mode_. Only when the issue is clearly _Dual JVM_ specific,
resort to more advanced techniques.

### Analyze with VisualVM

GraalVM, Truffle and the JVM performance overall can be greatly analyzed by
**VisualVM** - [download from its website](https://visualvm.github.io/). Enso
(because of building on top of Truffle) is well integrated with VisualVM's _heap
analysis_ as well as _polyglot sampler_ (more below). Enso offers various tools
that make integration with VisualVM even better.

## Sampling

**VisualVM** offers _polyglot sampler_. To use it one has to execute `enso` in
_JVM Only mode_ and also open up access to `org.graalvm.polyglot` module - as
the sampler needs to connect to the _HotSpot JVM_ and find out all existing
`Engine` instances. To do so one has to modify the
[launching instructions](../CONTRIBUTING.md#running-ide) a bit:

```bash
enso$ ENSO_ENGINE_ARGS=--jvm \
    JAVA_TOOL_OPTIONS=--add-opens=org.graalvm.polyglot/org.graalvm.polyglot=ALL-UNNAMED \
    corepack pnpm run dev:gui
```

Specifying `ENSO_ENGINE_ARGS` forces the _JVM Only mode_ and `JAVA_TOOL_OPTIONS`
provides access to the `org.graalvm.polyglot` module internals. Now open a
project, start VisualVM and attach to the Java process:

<img width="480" height="192" alt="attach to process"
  src="https://github.com/user-attachments/assets/e997dd7a-2856-489a-a344-31d330829839"
/>

Once connected, start the polyglot CPU sampler and then modify something in the
GUI. Each code modification results in an execution of the Enso code. The
sampler slowly collects the profiling info and presents the results for manual
inspection:

<img width="480" height="192" alt="start polyglot sampler"
  src="https://github.com/user-attachments/assets/da9167e3-f44e-4682-b90e-b45888f8f9fc"
/>

## Startup

Getting quickly ready to work is essential for good user experience. There have
been tremendous improvements over the last few years especially due to
compilation of Enso core into _native executable_ - more details in a
[Launching Enso Instantly](https://github.com/orgs/enso-org/discussions/10121)
discussion. Running Enso as a _native image_ removed all the HotSpot JVM
initialization overhead.

However that doesn't mean there are not inefficiencies in the Enso code itself.
To analyze them it is still useful to run in _JVM only mode_. Thus let this
document describe how to measure, record and analyze the startup of the Enso
engine in _JVM only mode_.

## Collecting the data via the runner

Runner executable also supports so called _self profiling_ with the
`--profiling-path` option. For example the
[launching instructions](../CONTRIBUTING.md#running-ide) can be modified to run
with the profiling enabled:

```
enso$ ENSO_ENGINE_ARGS="--jvm --profiling-path /tmp/run.npss" corepack pnpm run dev:gui
```

Open a project, work with it, then close it. The `/tmp/run.npss` file is
generated together with a sibling `/tmp/run.log` file. They can be both opened
in **VisualVM** with following command:

```
$ visualvm --openfile /tmp/run.npss
```

Unlike the _polyglot sampler_ this profiling works on the level of Java methods
and classes. It is not suitable for analyzing the behavior of Enso code, only
the supporting Java code.

### Interactively Analyze

VisualVM offers two timelines. A "stackdepth" one and also _"UI Actions"_ line.
Hovering over boxes in _"UI Actions"_ shows the messages describing what happens
in the engine - what has been logged into `start.log`. One can then select an
interval and get profiling information for that interval:

![VisualVM](https://user-images.githubusercontent.com/26887752/216099011-33866c1d-06ab-48dc-936d-b9190e80b9fb.png)

This picture shows that 2.7s is spend in `EnsoCompiledJob` task. Overall the
goal is to log enough information to help us navigate thru the long startup
sequence. Select appropriate interval based on the displayed _UI Actions_ - e.g.
logged events - and analyze what has happened there based on the sampling of JVM
stack traces.
