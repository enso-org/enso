---
layout: developer-doc
title: Contributing to Enso
category: summary
tags: [summary, contributing]
order: 2
---

# Contributing to Enso

Thank you for your interest in contributing to Enso! We believe that only
through community involvement can Enso be the best it can be! There are a whole
host of ways to contribute, and every single one is appreciated. The major
sections of this document are linked below:

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [The Contributor License Agreement](#the-contributor-license-agreement)
- [Issues](#issues)
- [Feature Enhancements](#feature-enhancements)
- [Bug Reports](#bug-reports)
- [Hacking on Enso](#hacking-on-enso)
  - [Design Documentation](#design-documentation)
  - [System Requirements](#system-requirements)
  - [Getting the Sources](#getting-the-sources)
  - Configuration
    - [Set Up Rust](#getting-set-up-rust)
    - [Set Up Documentation](#getting-set-up-documentation)
  - Enso Engine CLI
    - [Building Enso Engine](#building-enso-engine)
    - [Running Enso](#running-enso-from-cli)
    - [Testing Enso](#testing-enso-libraries)
  - [Running Enso IDE](#running-ide)
- [Pull Requests](#pull-requests)
- [Documentation](#documentation)
- [Issue Triage](#issue-triage)
- [Out-of-Tree Contributions](#out-of-tree-contributions)
- [Helpful Documentation and Links](#helpful-documentation-and-links)

<!-- /MarkdownTOC -->

All contributions to Enso should be in keeping with our
[Code of Conduct](./CODE_OF_CONDUCT.md).

## The Contributor License Agreement

As part of your first contribution to this repository, you need to accept the
Contributor License Agreement. You will automatically be asked to sign the CLA
when you make your first pull request.

Any work intentionally submitted for inclusion in Enso shall be licensed under
this CLA.

The CLA you sign applies to all repositories associated with the Enso project,
so you will only have to sign it once at the start of your contributions.

## Issues

If you're wanting to get involved with Enso's development and are looking for
somewhere to start, you can check out the following tags in our issues:

- [Good First Issue](https://github.com/enso-org/enso/labels/Status%3A%20Good%20First%20Issue)
- [Help Wanted](https://github.com/enso-org/enso/labels/Status%3A%20Help%20Wanted)

You can use the "Size" and "Difficulty" labels that should be assigned to every
issue to get a better idea of how much work a given issue might be.

## Feature Enhancements

If you feel like you have a suggestion for a change to the way that Enso works
as a language, please take a look at the [Enso RFC process](./rfcs/README.md) to
learn how to file an RFC for the project.

In essence, the RFC process provides a way to propose major changes to the
language, the compiler, and the runtime in a way that ensures that they get seen
and discussed by all the major stakeholders involved.

If, on the other hand, you're asking for a smaller feature, please feel free to
submit a
[feature request](https://github.com/enso-org/enso/issues/new?assignees=&labels=Type%3A+Enhancement&template=feature-request.md&title=)
to the repository.

## Bug Reports

While it's never great to find a bug, they are a reality of software and
software development! We can't fix or improve on the things that we don't know
about, so report as many bugs as you can! If you're not sure whether something
is a bug, file it anyway!

**If you are concerned that your bug publicly presents a security risk to the
users of Enso, please look at our [security guidelines](./SECURITY.md).**

Even though GitHub search can be a bit hard to use sometimes, we'd appreciate if
you could
[search](https://github.com/enso-org/enso/search?q=&type=Issues&utf8=%E2%9C%93)
for your issue before filing a bug as it's possible that someone else has
already reported the issue. We know the search isn't the best, and it can be
hard to know what to search for, so we really don't mind if you do submit a
duplicate!

Opening an issue is as easy as following
[this link](https://github.com/enso-org/enso/issues/new?template=bug-report.md)
and filling out the fields. The template is intended to collect all the
information we need to best diagnose the issue, so please take the time to fill
it out accurately.

The reproduction steps are particularly important, as the more easily we can
reproduce it, the faster we can fix the bug! It's also helpful to have the
output of `enso --version`, as that will let us know if the bug is Operating
System or Architecture specific.

### Turning on verbose logs

Sometimes, it is helpful to attach a verbose log to your bug report. The way to
enable verbose logging depends on which version of Enso you are using. For a
standalone distribution (`.exe` on Windows, `.AppImage` on Linux), you can
enable verbose logging by passing `-debug.verbose` option. If you are starting
the `project-manager`, or language server separately, then pass
`--log-level trace` option. With verbose logging, there are a lot of messages
printed to the standard output, and it is possible that on slower terminal
emulators this will clog the terminal and hence the whole backend. To avoid
this, we recommend redirecting the output to `/dev/null`, via a command like
`enso -debug.verbose > /dev/null 2>&1`.

The logs are kept in a central location `$ENSO_DATA_DIRECTORY/log` - on Linux,
they are in `$XDG_DATA_HOME/enso/log` (usually `~/.local/share/enso/log`), and
on Windows they are in `%APPDATA%\enso\log`, see
[distribution.md](distribution/distribution.md) for details. The log level name
consists of the timestamp of the log file creation. There is no automatic log
rotation, so you may want to delete the old logs from time to time.

## Hacking on Enso

This will get you up and running for Enso development, with only a minimal
amount of setup required. Enso's build system is fairly simple, allowing you to
bootstrap the compiler as long as you have a minimal set of tools.

### Design Documentation

If you're going to start contributing to Enso, it is often a good idea to take a
look at the design documentation for the language. These files explain provide
both a rigorous specification of Enso's design, but also insight into the _why_
behind the decisions that have been made.

These can be found in [`docs/`](README.md), and are organised by the part of the
compiler that they relate to.

### System Requirements

The following operating systems are supported for developing Enso:

- Windows 10
- macOS 10.14 and above
- Linux 4.4 and above

Currently, we support `x86_64` (all mentioned OS) and `arm64` (Mac only)
architectures. You may be able to develop Enso on other systems, but issues
arising from unsupported configurations will not be fixed by the core team.

In order to build and run Enso you will need the following tools:

- [NodeJS](https://nodejs.org/) with the latest LTS version. We recommend
  installing
  [a Node version manager that automatically picks up the correct version](https://github.com/shadowspawn/node-version-usage#supporting-products),
  like [fnm](https://github.com/Schniz/fnm).
- [sbt](https://www.scala-sbt.org/) with the same version as specified in
  [`project/build.properties`](../project/build.properties).
- [Maven](https://maven.apache.org/) with version at least 3.6.3.
- [GraalVM](https://www.graalvm.org/) with the same version as described in the
  [`build.sbt`](../build.sbt) file, configured as your default JVM.
- [Flatbuffers Compiler](https://google.github.io/flatbuffers) with version
  24.3.25. It is automatically downloaded when using the `run` command. For
  direct `sbt` usage, you can download the `flatc` binary from the
  [release assets](https://github.com/google/flatbuffers/releases/tag/v24.3.25).
- [Rustup](https://rustup.rs), the rust toolchain management utility.
- On MacOS and Linux, the `tar` command is required for running some tests. It
  should be installed by default on most distributions.
- On Windows, the `run` command must be run in the latest version of
  `Powershell` or in `cmd`.
- If you want to be able to build the `ensoup` Native Image, you will need a
  native C compiler for your platform as described in the
  [Native Image Prerequisites](https://www.graalvm.org/reference-manual/native-image/#prerequisites).
  On Linux that will be `gcc`, on macOS you may need `xcode` and on Windows you
  need to configure the Developer Command Prompt for Microsoft Visual C++ for
  the x64 architecture.

Managing multiple JVM installations can be a pain, so you can consider using
helper tools for that. We recommend:

- [Jenv](http://www.jenv.be/)
- or [sdkman](https://sdkman.io/)

**On Windows**: you need a few additional requirements:

1. `Developer Mode` must be enabled to support creating filesystem symlinks.
   E.g., using this instruction:
   https://pureinfotech.com/enable-developer-mode-windows-11/
2. You must create either `.bazelrc.local` at repository root, or
   `%USERPROFILE%\.bazelrc` (consult https://bazel.build/run/bazelrc for more
   possible locations), containing the following:

```
# Use different drive letter if needed, but the path must be SHORT
startup --output_base=C:/_bzl
common --disk_cache=C:/_bzl-disk
common --repository_cache=C:/_bzl-repo
```

3. You need to have `bash.exe` available in `PATH`. `bash.exe` from WSL is
   **not** suitable so either:

- install MSYS2 (https://www.msys2.org/)
- or use bash executable provided with Git by adding the following to your
  `PATH` variable: `C:\Program Files\Git\bin`, if you have it installed.
- or configure Git installation selecting the third option:
  ![Git installation settings](https://github.com/user-attachments/assets/def189fa-985b-47f3-8c8b-153c0f39fa26)
- or have `BAZEL_SH` environment variable set to exact path to `bash.exe`,
  whatever way you have it installed.

**For users of M1 Mac**: installing GraalVM on M1 Mac requires manual actions,
please refer to a [dedicated documentation](./graalvm-m1-mac.md).

### Getting the Sources

Given you've probably been reading this document on GitHub, you might have an
inkling where to look!. You can clone Enso using two methods:

- **Via HTTPS:** We recommend you only use HTTPS if checking out the sources as
  read-only.

```
git clone https://github.com/enso-org/enso.git
```

- **Via SSH:** For those who plan on regularly making direct commits, cloning
  over SSH may provide a better user experience (but requires setting up your
  SSH Keys with GitHub).

```
git clone git@github.com:enso-org/enso.git
```

The recommended IDE for working with the sources is
[VSCode with Enso extension](../tools/enso4igv/README.md). Alternativelly, use
the IntelliJ with the official
[Scala plugin](https://plugins.jetbrains.com/plugin/1347-scala) and import the
`sbt` project.

### Getting Set Up (Rust)

The Rust code in this repository requires a specific nightly rust toolchain, as
defined by [rust-toolchain](../rust-toolchain.toml) override file. The `rustup`
will automatically download the appropriate compiler version along with the
necessary components.

```bash
enso$ rustup toolchain install stable   # Stable toolchain required for the following tools.
```

The previous three steps shall be enough to build the IDE via `./run ide build`.

### Getting Set Up (Documentation)

We enforce automated formatting of all of our documentation and configuration
using the fairly common [prettier](https://prettier.io) automatic formatter. You
can install prettier for our project by running the following command:

```bash
enso$ corepack pnpm install
```

This does, however, mean that you have to have node installed on your system.
Please follow the guidelines [above](#getting-set-up-rust) to install node if
you have not already done so.

The version if prettier is forced by our
[`package-lock.json`](../package-lock.json) in order for us to make formatting
bumps all at once.

You can format all of our documentation and configuration as follows:

```bash
enso$ corepack pnpm run format
```

### Building Enso Engine

There are multiple projects in the main Enso repository. All of the engine parts
can be built, run and tested using
[Scala Build Tool](https://www.scala-sbt.org/). As long as your configuration is
correct, with the correct versions of `sbt`, Rust and GraalVM, the same steps
can be followed on all of our supported platforms (Linux, MacOS and Windows).

```sbt
$ sbt
sbt:enso> buildEngineDistribution
```

The language interpreter CLI distribution is generated into `built-distribution`
directory with `bin/enso` launcher script/executable located inside. `sbt`
handles downloading and building library dependencies as needed, meaning that
you don't need to handle any of this manually.

One can use the generated binaries directly, but the suggested workflow is to
continue using [sbt for execution](#running-enso-from-cli) as well.

<!--
#### Building Enso Components

In order to build a specific component (e.g. `runtime`), please follow the
following steps.

1. Enter the sbt shell in the repository root directory by typing `sbt`.
2. Change to the project you are concerned with (in our case `runtime`) by
   executing `project runtime`.
3. Execute `compile` in order to compile the project in question. This will
   compile the project and all its dependencies as necessary.

You can substitute both `bench` and `test` for `compile` in step 3, and the sbt
shell will execute the appropriate thing. Furthermore we have `testOnly` and
`benchOnly` that accept a glob pattern that delineates some subset of the tests
or benchmarks to run (e.g. `testOnly *FunctionArguments*`).

#### Passing Debug Options

GraalVM provides some useful debugging options, including the ability to output
the compilation graph during JIT optimisation, and the ASM generated by the JIT.

However, as we don't want these things polluting our standard builds, we provide
a helper SBT command `withDebug` to allow for passing these options. It supports
the following flags:

- `--dumpGraphs`: This dumps the IGV (read about
  [Enso tooling for IGV](../tools/enso4igv/README.md)) graphs for the program to
  allow for manual analysis and discovery of optimisation failures.
- `--showCompilations`: Prints the truffle compilation trace information.
- `--printAssembly`: Prints the assembly output from the HotSpot JIT tier.
- `--debugger`: Launches the JVM with the remote debugger enabled.

For more information on this sbt command, please see
[WithDebugCommand.scala](../project/WithDebugCommand.scala).

It is used as an addendum to the basic sbt command you want to run (e.g. `test`
from above). The format is `withDebug COMMAND [OPTIONS...]`, and if you need to
pass any additional options to `COMMAND` you must do so following a `--`. For
example:

```
withDebug run --dumpGraphs --printAssembly -- --run MyFile.enso
withDebug benchOnly --showCompilations -- RecursionBenchmark
```

Step by step debugging can be triggered as

```
sbt:runtime> withDebug testOnly --debugger -- *FavoriteTest*
```

One can debug `project-manager` code by executing

```
sbt:project-manager> withDebug run --debugger
```

read more about [debugging Java & Enso code](debugger/README.md).

#### Working with Assembly

In order to examine the assembly generated by GraalVM and HotSpot you need to
provide your JVM install with a dynamic library that supports the dumping of
assembly. It can be acquired for MacOS and Linux
[here](https://github.com/liuzhengyang/hsdis/), and for windows from
[here](http://fcml-lib.com/). There are other methods to acquire it, as well, so
please choose one best suited for you.

Once you have a copy of the dynamic library, it needs to be placed in
`$JVM_HOME/lib/server`.
-->

### Running Enso from CLI

The language interpreter CLI distribution can be
[built by `sbt`](#building-enso-engine). To _execute the Enso interpreter_ use
`sbt` as well:

```sbt
$ sbt
sbt:enso> runEngineDistribution --help
```

The `runEngineDistribution` task takes care of everything. It builds the
distribution in the `built-distribution` directory (with the help of
`buildEngineDistribution` command) and launches the generated Enso launcher
executable.

Detailed information on the flags it supports can be shown with the `--help`
flag, but the primary functionality is as follows:

- `--new PATH`: Creates a new Enso project at the location specified by `PATH`.
- `--run PATH`: Executes the interpreter on the Enso source specified by `PATH`.
  In this case, `PATH` must point to either a standalone Enso file or an Enso
  project.

```sbt
sbt:enso> runEngineDistribution --new Hello
```

The above command generates project in directory `Hello` with the main source
file being at `Hello/src/Main.enso`. One can edit the file and then execute it:

```sbt
sbt:enso> runEngineDistribution --run Hello
```

### Testing Enso Libraries

To run the tests inside sbt you can use the following command:

```sbt
sbt:enso> runEngineDistribution --run test/Table_Tests/
```

This builds the library code and runs all the tests in the specified folder. And
is the fastest way to build the code and run the tests.

Alternatively to run a single file of tests you can specify the file to run

```sbt
sbt:enso> runEngineDistribution --run test/Base_Tests/src/Data/Time/Duration_Spec.enso
```

Or you can pattern match against the test name using this syntax

```sbt
sbt:enso> runEngineDistribution --run test/Base_Tests/src/Data/Time/Duration_Spec.enso should.normalize
```

Or even control additional _environment variables_ of the running process. The
Base tests rely in a few places on the system language controlled (on Linux) by
a value of `LANG` environment variable. To change value of `LANG` environment
variable to `C` run the tests with `--env` option and name/value pair:

```sbt
sbt:enso> runEngineDistribution --env LANG=C --run test/BASE_Tests
```

This runs all tests in Duration_Spec.enso that have 'should normalize' the their
name.

To run with a debugger first start the debugger listening on 5005, add a
breakpoint in a test then run with

```sbt
sbt:enso> runEngineDistribution --run test/Base_Tests --debug
```

Then connect your development environment to the 5005 port. More
[details on debugging](./debugger/README.md) are available in a separate
document.

When using the `runEngineDistribution` command of `sbt` the Java assertions
(`-ea` JVM option) as well as Enso assertions are enabled.

<!--
Alternatively to run the test outisde of sbt you need to first build the engine,
the easiest way to do so is to run `sbt buildEngineDistributionNoIndex`. That
will create a distribution in the directory `built-distribution`. The engine
runner that can be used for running the tests is located at:

- on Windows
  `built-distribution/enso-engine<VERSION>-windows-amd64/enso-<VERSION>/bin/enso.bat`
- on ARM mac -
  `built-distribution/enso-engine-<VERSION>-macos-aarch64/enso-<VERSION>/bin/enso`
- on Linux -
  `built-distribution/enso-engine-<VERSION>-linux-amd64/enso-<VERSION>/bin/enso`

you can run the following commands (where `enso` refers to the built runner
executable as explained above):

```bash
enso --run test/Base_Tests
enso --run test/Geo_Tests
enso --run test/Table_Tests
```

Or to run just a single test file (e.g., `Duration_Spec.enso`):

```bash
enso --run test/Base_Tests/src/Data/Time/Duration_Spec.enso
```

To run with a debugger first start the debugger listening on 5005, add a
breakpoint in a test then run with

```bash
JAVA_TOOL_OPTIONS='-agentlib:jdwp=transport=dt_socket,server=n,address=5005' enso --run test/Base_Tests/src/Data/Time/Duration_Spec.enso
```

The Database tests will by default only test the SQLite backend, to test other
backends see
[`test/Table_Tests/src/Database/README.md`](../test/Table_Tests/src/Database/README.md)
for information on how to configure them.

The Base tests rely in a few places on the system language. On Linux you can set
the `LANG` environment variable to `C` to make sure that the language is
configured correctly and run the tests as following:

```bash
LANG=C enso --run test/Base_Tests
```

Note that JVM assertions are not enabled by default, one has to pass `-ea` via
`JAVA_TOOL_OPTIONS` environment variable. There are also Enso-specific assertions
(method `Runtime.assert`) that can be enabled when `ENSO_ENABLE_ASSERTIONS`
environment variable is set to "true". If JVM assertions are enable, Enso
assertions are enabled as well.
-->

#### Static Analysis

Additionally, you can check a project/script for basic errors (without running
it) using `--compile` flag. The `--enable-static-analysis` flag enables
experimental static analysis passes. So if you want to try out the type checker
prototype on your project, you can run:

```sbt
sbt:enso> runEngineDistribution --compile Hello --enable-static-analysis
```

#### Native Image

Native image is a capability provided alongside GraalVM that allows the
generation of native executables from JVM language programs (such as the Enso
interpreter itself). Details:

- [infrastructure](./infrastructure/native-image.md)
- [native image debugging](./debugger/native-debugging.md)
- [sbt support](./infrastructure/sbt.md)

Native image support is disabled for development (as it slows down
edit/compile/run cycle significantly). One has to **opt-in** to enable it.

<!--
#### Test Dependencies

To run all the stdlib test suites, set `CI=true` environment variable:

```bash
env CI=true enso --run test/Base_Tests/
```

For more details about the CI setup, you can check the
`.github/workflows/scala.yml` GitHub workflow.
-->

#### Running IDE

Running development version of the IDE is possible via the npm script in the
root of the repository:

```bash
enso$ corepack pnpm install
enso$ corepack pnpm run compile
enso$ corepack pnpm run dev:gui
```

By default the `dev:run` script looks for the dev `enso` executable in the
`built-distribution` directory assuming that it was built with sbt:

```sbt
sbt:enso> buildEngineDistribution
```

and will fallback to downloading the latest nightly if not found. You can
override the location of `enso` executable by providing the absolute path to it
in the `ENSO_ENGINE_PATH` environment variable.

```bash
enso$ ENSO_ENGINE_PATH=/path/to/enso corepack pnpm run dev:gui
```

You can provide extra arguments to the enso executable using the
`ENSO_ENGINE_ARGS` environment variable.

```bash
enso$ ENSO_ENGINE_ARGS='--log-level trace' corepack pnpm run dev:gui
```

To [debug](debugger/README.md) the language server process of a running project,
you can set the
`JAVA_TOOL_OPTIONS=-agentlib:jdwp=transport=dt_socket,address=5005` environment
variable when starting the `pnpm run dev:gui` dev server. Just
[configure your Java IDE](debugger/README.md) to listen on port 5005 before
starting the project.

## Pull Requests

Pull Requests are the primary method for making changes to Enso. GitHub has
[fantastic documentation](https://help.github.com/articles/about-pull-requests/)
on using the pull request feature. Enso uses the 'fork-and-pull' model of
development. It is as described
[here](https://help.github.com/articles/about-collaborative-development-models/)
and involves people pushing changes to their own fork and creating pull requests
to bring those changes into the main Enso repository.

Please make all pull requests against the `develop` branch.

- We run CI on all contributions to Enso, but it's still useful for you to run
  the tests yourself locally first! This can be done by running `test` in the
  `enso` project in sbt.
- Additionally, please ensure that your code conforms to the Enso style guides,
  particularly the [Scala Style Guide](./style-guide/scala.md) and the
  [Java Style Guide](./style-guide/java.md).

Make sure you perform these checks before _every_ pull request. You can even add
[git hooks](https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks) before
every push to make sure that you can't forget.

- Every pull request to the Enso repository is reviewed by a member of the core
  team! You'll get assigned a reviewer based on the areas your PR touches, but
  please feel free to ask for a specific person if you've worked with them in a
  specific area before!
- If you have questions, or would like to begin the review process before your
  PR is 'done', please use the
  [Draft Pull Requests](https://github.blog/2019-02-14-introducing-draft-pull-requests/)
  feature on GitHub. Doing so will allow you to make use of our CI
  infrastructure as part of your development process.

Once the reviewer approves your pull request it will be tested by our continuous
integration provider before being merged. If we request changes to your PR,
please feel free to discuss the suggestions and comments! We can only achieve
the best results through open collaboration.

## Documentation

Documentation improvements are very welcome! For now, the main documentation
available is the _developer_ documentation for the language, which can be found
at the [dev docs site](https://enso.org/docs/developer). The source for this
documentation is found in the [`docs/`](.) folder, and can be altered from
there.

Documentation pull requests will be reviewed in exactly the same way as normal
pull requests.

To find documentation-related issues, sort by the
[Category: Documentation](hhttps://github.com/enso-org/enso/labels/Category%3A%20Documentation)
label.

## Issue Triage

Sometimes issues can be left open long after the bug has been fixed. Other
times, a bug might go stale because something has changed in the meantime.

It can be helpful to go through older bug reports and make sure that they are
still valid. Load up an older issue, double check that it's still true, and
leave a comment letting us know if it is or is not. The
[least recently updated](https://github.com/enso-org/enso/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-asc)
sort is good for finding issues like this.

Contributors with sufficient permissions can help by adding labels to help with
issue triage.

If you're looking for somewhere to start, take a look at the
[Difficulty: Beginner](https://github.com/enso-org/enso/labels/Difficulty%3A%20Beginner)
issue label, as well as the
[Status: Help Wanted](https://github.com/enso-org/enso/labels/Status%3A%20Help%20Wanted)
and
[Status: Good First Issue](https://github.com/enso-org/enso/labels/Status%3A%20Good%20First%20Issue)
labels.

## Out-of-Tree Contributions

As helpful as contributing to Enso directly is, it can also be just as helpful
to contribute in other ways outside this repository:

- Answer questions in the [Discord](https://chat.luna-lang.org) or on
  [StackOverflow](https://stackoverflow.com/questions/tagged/enso).

## Helpful Documentation and Links

For people new to Enso, and just starting to contribute, or even for more
seasoned developers, some useful places to look for information are:

- The [design documentation](./README.md).
- The community! Don't be afraid to ask questions.
