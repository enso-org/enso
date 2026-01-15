---
layout: developer-doc
title: Bazel Build System
category: infrastructure
tags: [infrastructure, build, bazel]
order: 2
---

# Bazel Build System

This document provides an overview of Enso's Bazel build system, explains
fundamental Bazel concepts, covers common pitfalls when working with Bazel, and
describes our roadmap for migrating from the legacy Rust-based build system.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [How Bazel Works](#how-bazel-works)
  - [Build Graph and Artifact-Oriented Design](#build-graph-and-artifact-oriented-design)
  - [Starlark Configuration Language](#starlark-configuration-language)
  - [Toolchains](#toolchains)
  - [Hermeticity and Reproducibility](#hermeticity-and-reproducibility)
  - [Sandboxing Strategies](#sandboxing-strategies)
  - [Remote Caching and Execution](#remote-caching-and-execution)
- [Project Overview](#project-overview)
  - [Current Architecture](#current-architecture)
  - [Why Bazel?](#why-bazel)
- [Configuration Files](#configuration-files)
- [Getting Started](#getting-started)
  - [Prerequisites](#prerequisites)
  - [Platform-Specific Setup on Windows](#platform-specific-setup-on-windows)
- [Common Build Targets](#common-build-targets)
  - [GUI Build](#gui-build)
  - [Engine Distribution](#engine-distribution)
  - [Utility Targets](#utility-targets)
- [Custom Toolchains](#custom-toolchains)
- [Tips and Tricks](#tips-and-tricks)
- [Common Pitfalls](#common-pitfalls)
- [Future Plans](#future-plans)

<!-- /MarkdownTOC -->

## How Bazel Works

Bazel is a build system originally developed by Google that focuses on
correctness, reproducibility, and scalability. Understanding its core concepts
helps explain why we're adopting it.

### Build Graph and Artifact-Oriented Design

Unlike traditional build systems that execute scripts sequentially, Bazel
constructs a **directed acyclic graph (DAG)** of all build targets and their
dependencies before executing anything. This graph-based approach enables:

- **Parallel execution**: Independent targets build simultaneously
- **Incremental builds**: Only targets affected by changes are rebuilt
- **Dependency analysis**: Bazel knows exactly what depends on what

Every build produces **artifacts** - files that are the output of build actions.
Bazel tracks the relationship between inputs (source files, dependencies) and
outputs (compiled binaries, bundles). If inputs haven't changed, Bazel can skip
the action entirely and reuse cached artifacts.

**Example**: When you run `bazel build //app/gui:dist`, Bazel:

1. Analyzes the build graph to find all transitive dependencies
2. Checks which artifacts are already cached
3. Executes only the actions needed to produce missing artifacts
4. Stores new artifacts in the cache for future builds

### Starlark Configuration Language

Bazel uses **Starlark** (formerly Skylark) - a Python-like configuration
language - for defining build rules. Starlark appears in two types of files:

**BUILD.bazel files** define targets within a package:

```python
# Example: Define a TypeScript library
ts_project(
    name = "my_lib",
    srcs = glob(["src/**/*.ts"]),
    deps = ["//other/package:lib"],
)
```

**.bzl files** define reusable rules, macros, and functions:

```python
# Example: Custom rule definition
def my_custom_rule(name, srcs):
    native.genrule(
        name = name,
        srcs = srcs,
        outs = [name + ".out"],
        cmd = "process $(SRCS) > $@",
    )
```

Key Starlark concepts:

- **Targets**: Named build units (libraries, binaries, tests)
- **Labels**: References to targets, e.g., `//app/gui:dist` or `@npm//react`
- **Providers**: Structured data passed between rules (like interfaces)

### Toolchains

**Toolchains** abstract over the tools needed to build for different platforms.
Instead of hardcoding paths to compilers or interpreters, Bazel resolves the
appropriate toolchain at build time based on the target platform.

For example, our custom SBT toolchain (in `toolchains/sbt/`) provides:

- The path to the `sbt` executable
- A `SbtInfo` provider that rules can use to invoke sbt
- Template variables for use in build commands

The toolchain is defined separately from the rules that use it. This means the
`run_sbt` rule doesn't need to know where sbt is installed - it just requests
the SBT toolchain and Bazel provides the appropriate one for the current
platform.

Toolchains enable:

- **Cross-compilation**: Build for different platforms from one machine
- **Hermetic builds**: Tools are managed by Bazel, not system-installed versions
- **Portability**: Same BUILD files work on Linux, macOS, and Windows

### Hermeticity and Reproducibility

Bazel strives for **hermetic builds** - builds that depend only on declared
inputs and are isolated from the host system. This is achieved through:

- **Sandboxing**: Build actions run in isolated environments with only declared
  inputs visible
- **Strict dependency declaration**: Undeclared dependencies cause build
  failures
- **Managed toolchains**: Compilers and tools are downloaded and versioned by
  Bazel

The benefit: given the same source code and Bazel configuration, builds produce
identical outputs regardless of the machine they run on.

### Sandboxing Strategies

Bazel provides several sandboxing strategies with different levels of isolation.
Understanding these helps when debugging build issues or configuring builds for
specific environments.

**`local` strategy:**

No sandboxing at all. The action's command runs with the working directory set
to your workspace's execroot. This is the fastest but provides no isolation -
actions can access undeclared inputs or leave behind unexpected files.

**`processwrapper-sandbox` strategy:**

A portable sandboxing strategy that works on any POSIX system. It:

1. Creates a sandbox directory with symlinks pointing to declared source files
2. Executes the action with the working directory set to this sandbox
3. Moves declared output artifacts back to the execroot
4. Deletes the sandbox

This prevents actions from accidentally using undeclared input files or
littering the execroot with unknown outputs. It's lightweight but doesn't
provide filesystem or process isolation.

**`linux-sandbox` strategy:**

Builds on `processwrapper-sandbox` with additional OS-level isolation using
Linux namespaces (similar to Docker). It:

- Makes the entire filesystem read-only except for the sandbox directory
- Optionally blocks network access
- Uses PID namespaces to hide other processes from the action
- Reliably kills all processes (including daemons) when the action completes

This prevents dangerous accidents like a buggy test running `rm -rf $HOME`.

**`darwin-sandbox` strategy:**

Similar to `linux-sandbox` but for macOS, using Apple's `sandbox-exec` tool to
achieve comparable isolation.

**Nested sandbox limitations:**

Both `linux-sandbox` and `darwin-sandbox` have restrictions in nested
environments:

- `linux-sandbox` cannot run inside Docker containers (unless using
  `docker run --privileged`) because Docker already uses Linux namespaces
- `darwin-sandbox` cannot run inside an already-sandboxed process

In these cases, Bazel automatically falls back to `processwrapper-sandbox`.

**Configuring sandbox strategy:**

To prevent silent fallback and ensure a specific strategy is used:

```bash
bazel build --spawn_strategy=worker,linux-sandbox //target
```

This will fail with an error rather than silently falling back to a less strict
strategy.

In this project, we use `processwrapper-sandbox` by default on Linux and macOS
(configured in `.bazelrc`) to ensure consistent behavior across local builds and
CI environments (which often run in Docker).

### Remote Caching and Execution

Bazel can store build artifacts in a **remote cache** shared across developers
and CI. When you build a target:

1. Bazel computes a hash of all inputs (sources, dependencies, build commands)
2. Checks if an artifact with that hash exists in the remote cache
3. Downloads the cached artifact instead of rebuilding (cache hit)
4. Uploads newly built artifacts to the cache (cache miss)

This dramatically speeds up builds - if CI already built a commit, developers
can download those artifacts instead of rebuilding locally.

**Remote execution** takes this further by running build actions on remote
workers, distributing compilation across a cluster of machines.

## Project Overview

### Current Architecture

Enso uses a hybrid build system where **Bazel acts as the primary
orchestrator**, managing the build of frontend components
(TypeScript/JavaScript), Rust components (parser, WASM FFI), and delegating
JVM/Scala compilation to sbt.

The key insight is that Bazel doesn't directly compile Scala code. Instead, it:

1. Builds all non-JVM dependencies first (Rust parser, TypeScript components,
   Python resources)
2. Invokes sbt as a subprocess, passing the pre-built artifacts via system
   properties
3. sbt recognizes it's running under Bazel (via `BazelSupport` plugin) and skips
   running cargo/pnpm, using Bazel-provided artifacts instead

This architecture is implemented in the `BazelSupport` sbt plugin
([`project/BazelSupport.scala`](../../project/BazelSupport.scala)) which detects
when sbt is invoked by Bazel and configures artifact paths accordingly.

### Why Bazel?

We are migrating from a custom Rust-based build system (located in
[`build_tools/`](../../build_tools/)) to Bazel. The legacy `./run` script
provided CI orchestration and target management, but Bazel offers significant
advantages:

- **Superior caching**: Fine-grained artifact caching with remote cache support
- **Multi-language native support**: First-class rules for TypeScript, Rust,
  Java - no custom integration needed
- **Parallel execution**: Automatic parallelization based on the build graph
- **Reproducibility**: Hermetic builds ensure consistent results across machines
- **Cross-platform support**: Native support for Windows, Linux, and macOS with
  platform-specific toolchain resolution

## Configuration Files

| File             | Purpose                                         |
| ---------------- | ----------------------------------------------- |
| `MODULE.bazel`   | Bzlmod configuration - external dependencies    |
| `.bazelrc`       | Bazel configuration flags and settings          |
| `.bazelversion`  | Pinned Bazel version (currently 8.2.1)          |
| `.bazelignore`   | Directories excluded from Bazel's file watching |
| `BUILD.bazel`    | Build target definitions (40+ files in project) |
| `.bazelrc.local` | Local overrides (not committed, user-specific)  |

## Getting Started

### Prerequisites

Ensure you have the following installed:

- Node.js (version specified in `.node-version`)
- [Bazelisk](https://github.com/bazelbuild/bazelisk) (recommended) or use Bazel
  provided by `pnpm` after `pnpm`: `pnpm bazel ...` will execute the Bazel
  command.

### Platform-Specific Setup on Windows

Windows requires special configuration due to path length limitations and
symlink requirements:

1. Enable **Developer Mode** for filesystem symlinks

2. Create `.bazelrc.local` (or `%USERPROFILE%\.bazelrc`) with short paths:

   ```
   # Use different drive letter if needed, but paths must be SHORT
   startup --output_base=C:/_bzl
   common --disk_cache=C:/_bzl-disk
   common --repository_cache=C:/_bzl-repo
   ```

3. Ensure `bash.exe` is available in PATH (Git Bash, MSYS2, but NOT WSL):
   - Add `C:\Program Files\Git\bin` to PATH, or
   - Set `BAZEL_SH` environment variable to bash.exe path

## Common Build Targets

### GUI Build

Build the web-based GUI:

```bash
bazel build //app/gui:dist
```

This compiles TypeScript, bundles with Vite, and produces a static distribution.

Build the Electron package:

```bash
bazel build //app/electron-client:dist
```

### Engine Distribution

Build the Enso engine (JVM version):

```bash
bazel build //:sbt_build_engine_distribution
```

Build the native image version:

```bash
bazel build //:sbt_build_native_engine_distribution
```

### Utility Targets

**Lint Bazel files:**

```bash
bazel run //:lint_bazel_files
```

**Format Bazel files:**

```bash
bazel run //:format_bazel_files
```

**Write generated files to source tree:**

```bash
bazel run //:write_all
```

This updates generated files like WASM bindings, AST codegen, icon metadata,
etc.

## Custom Toolchains

Enso defines several custom toolchains in the `toolchains/` directory:

### SBT Toolchain (`toolchains/sbt/`)

Manages sbt installation and provides the `run_sbt` rule for invoking sbt builds
from Bazel.

- `extensions.bzl` - Module extension for sbt repository
- `toolchain.bzl` - Toolchain implementation with `SbtInfo` provider
- `run_sbt.bzl` - Rule for running sbt with Bazel-managed dependencies

### FlatBuffers Toolchain (`toolchains/flatc/`)

Provides the `flatc` compiler for FlatBuffers schema compilation. Automatically
downloads platform-specific binaries (v24.3.25) for macOS, Linux, and Windows.

### Tar Toolchain (`toolchains/tar/`)

Windows-specific toolchain for tar operations, using the system's built-in tar.

## Tips and Tricks

### Local Configuration

Create `.bazelrc.local` for personal settings that shouldn't be committed:

```bash
# Example: Enable remote cache for local development
build --remote_cache=grpcs://your-cache-server
build --remote_cache_header="authorization=Bearer TOKEN"
```

### Querying Build Graph

Find all targets in a package:

```bash
bazel query //app/gui:all
```

Show dependencies of a target:

```bash
bazel query "deps(//app/gui:dist)" --output=graph
```

Find reverse dependencies:

```bash
bazel query "rdeps(//..., //lib/rust/parser:parser)"
```

### Debugging Builds

Show verbose output for failing actions:

```bash
bazel build --verbose_failures //target
```

Keep sandbox for debugging:

```bash
bazel build --sandbox_debug //target
```

Show executed commands:

```bash
bazel build --subcommands //target
```

### Working with Generated Files

Bazel builds are hermetic - all outputs go to Bazel's output directories, not
the source tree. However, IDEs and other local tools often need generated files
to be present in the source tree to provide proper code completion, navigation,
and error checking.

The `//:write_all` target solves this by copying Bazel-generated files back into
the source tree:

```bash
bazel run //:write_all
```

This target updates the following generated files:

| Target                                        | Description                          |
| --------------------------------------------- | ------------------------------------ |
| `//app/rust-ffi:write_wasm_dist`              | WASM bindings for the Rust FFI       |
| `//app/ydoc-shared:write_ast_codegen`         | AST type definitions for YDoc        |
| `//app/gui:write_icon_metadata`               | Icon names for the GUI               |
| `//app/table-expression:write_parser_codegen` | Parser codegen for table expressions |

**When to run `write_all`:**

- After cloning the repository (to populate generated files for IDE support)
- After modifying code generators (Rust parser, AST definitions, etc.)
- Before committing changes that affect generated files
- When your IDE shows errors in generated code that Bazel builds successfully

**How it works:**

The `write_source_files` rule from `aspect_bazel_lib` compares Bazel's generated
output with the files in the source tree. When run, it copies any differences
back to the source tree. This pattern keeps generated files version-controlled
while ensuring they stay in sync with their generators.

### Release Builds

Enable stamping for release builds (includes version info):

```bash
bazel build --config=release //target
```

## Common Pitfalls

### 1. Windows Path Length Issues

**Problem:** Builds fail with "path too long" errors on Windows.

**Solution:** Configure short output paths in `.bazelrc.local`:

```
startup --output_base=C:/_bzl
common --disk_cache=C:/_bzl-disk
```

### 2. Stale Build Artifacts

**Problem:** Changes don't seem to take effect, or builds behave unexpectedly.

**Solution:** Clean and rebuild:

```bash
bazel clean --expunge
bazel build //target
```

### 3. Sandbox Permission Errors

**Problem:** Build actions fail with permission denied errors in sandbox.

**Solution:** Some actions need local execution. Check `.bazelrc` for strategy
overrides. On Linux/macOS, we use `processwrapper-sandbox` by default.

## Future Plans

We are actively migrating from the legacy Rust-based build system
([`build_tools/`](../../build_tools/)) to Bazel. The `./run` script and
associated Rust code provided build orchestration, CI generation, and artifact
management, but maintaining a custom build system has significant overhead.

### Migration Roadmap

**Completed:**

- TypeScript/JavaScript builds via `rules_js` and `rules_ts`
- Rust component builds via `rules_rust`
- FlatBuffers installation via custom toolchain
- Basic CI integration with remote caching
- SBT toolchain for Engine builds
- Electron package builds for all three platforms

**In Progress:**

- Proper version information handling for the Electron package
- macOS DMG signing and notarization

**Planned:**

- Engine distribution builds orchestrated by Bazel (currently delegates to sbt)
- Migrate remaining `./run` script functionality to Bazel
- Complete test execution through Bazel
- Remove dependency on legacy `build_tools/` code

### Benefits of Full Migration

- **Unified build system**: Single tool for all languages and platforms
- **Better caching**: Fine-grained caching at the action level vs. target level
- **Reduced maintenance**: No custom build infrastructure to maintain
