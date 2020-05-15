---
layout: developer-doc
title: Runtime Features
category: runtime
tags: [runtime, design]
order: 4
---

# Runtime Features
This document contains a detailed specification of Enso's runtime. It includes
a description of the technologies on which it is built, as well as the features
and functionality that it is required to support. In addition, the document aims
to explain why _this_ design, rather than one of the many alternatives available
to the team.

When we refer to the Enso 'runtime' in this document, we are referring to the
combination of the language communication protocol, typechecker, optimiser, and
interpreter. Though the interpreter itself has its own runtime, it is these
components that make up _Enso's_ runtime.

The runtime is built on top of [GraalVM](https://www.graalvm.org/), a universal
virtual machine on which you can run any language with an appropriate
interpreter. In basing Enso's runtime on GraalVM, we not only have access to a
comprehensive toolkit for building high-performance language interpreters, but
also to the ecosystems of all the other languages (e.g. C++, Python, R) that can
run on top of it. GraalVM also brings some additional important tooling, such as
the JVM ecosystem's performance monitoring, analysis, and debugging toolsets.

The runtime described below is a complex beast, so this document is broken up
into a number of sections. These aim to provide an architectural overview, and
then describe the design of each component in detail.

<!-- MarkdownTOC levels="1,2" autolink="true" -->

- [Architectural Overview](#architectural-overview)
  - [The Broader Enso Ecosystem](#the-broader-enso-ecosystem)
  - [The Runtime's Architecture](#the-runtimes-architecture)
- [Choosing GraalVM](#choosing-graalvm)
- [The Runtime Components](#the-runtime-components)
  - [Language Server](#language-server)
  - [Filesystem Driver](#filesystem-driver)
  - [Typechecker](#typechecker)
  - [Optimiser](#optimiser)
  - [Interpreter and JIT](#interpreter-and-jit)
- [Cross-Cutting Concerns](#cross-cutting-concerns)
  - [Caching](#caching)
  - [Profiling and Debugging](#profiling-and-debugging)
  - [Foreign Language Interoperability](#foreign-language-interoperability)
  - [Lightweight Concurrency](#lightweight-concurrency)
- [The Initial Version of the Runtime](#the-initial-version-of-the-runtime)
- [Development Considerations](#development-considerations)

<!-- /MarkdownTOC -->

# Architectural Overview
The Enso runtime is just one of the many components of the Enso ecosystem. This
section provides an overview of how it fits into the broader ecosystem, with a
particular focus on how it enables workflows for Enso Studio, the Enso CLI, and
Language Server integration. In addition, this section also explores the
architecture of the runtime itself, breaking down the opaque 'runtime' label
into the

## The Broader Enso Ecosystem
While the runtime is arguably the core part of Enso, for the language would not
be able to exist without it, the language's success is just as dependent on the
surrounding ecosystem.

TBC...

It is worth providing a brief explanation of each of the components to aid in
understanding how the runtime fits into the ecosystem.

- **Enso Studio GUI:** This is the interface with which most of Enso's users
  will interact. It handles the drawing of and interaction with the Enso graph
  for users, as well as the searcher and other user-facing functionality. It
  also provides a text editor.
- **Project Manager:** This allows for management of one or more Enso projects,
  and is primarily responsible for file-system-agnostic interaction with the
  project structure, and spawning of the Enso runtime.
- **GUI Backend:** The GUI backend is instantiated for each project, and is
  responsible for all of the user-facing logic that goes into interaction with
  the Enso runtime.
  + **Graph State Manager:** This component handles management of the state
    required to draw the graph in the GUI.
  + **Double Representation Manager:** This component handles the encoding and
    decoding of the Enso program to and from the intermediate representation.
  + **Undo/Redo Manager:** This component handles undo and redo for the graph, a
    somewhat novel operation as it does not not always have a 1:1 correspondence
    with textual editing.
- **CLI:** This provides a command-line (specifically a terminal) interface to
  the Enso runtime. This allows both for the CLI invocation of Enso, as well as
  an interactive REPL. This communicates with the runtime itself via the
  language server protocol.
- **Enso Runtime:** This is what is described in this document, and is
  responsible for the execution of Enso programs. It handles the typechecking,
  optimisation, and interpretation of Enso code, as well as the provision of
  interfaces to foreign languages.

## The Runtime's Architecture
In order to better appreciate how the components specified below interact, it is
important to have an understanding of the high-level architecture of the runtime
itself. The design in this document pertains _only_ to the 'Enso Runtime'
component of the diagram above, and hence makes no mention of the others.

In the diagram below, the direction of arrows is used to represent the 'flow' of
information between the various components.

TBC...

# Choosing GraalVM
Building the runtime on top of GraalVM was of course not the only choice that
could've been made, but it was overwhelmingly the most sensible option out of
those considered.

At the time the runtime was designed, there were three main options that were
being considered.

- **LLVM:** A battle-tested and comprehensive toolchain for the creation of
  language compilers, [LLVM](https://llvm.org/) includes facilities for
  compilation, optimisation, JIT, and linking.
- **GHC:** The [Glasgow Haskell Compiler](https://gitlab.haskell.org/ghc/) is
  a sophisticated compiler and runtime for Haskell that provides a
  language-agnostic set of internal representations that could be leveraged to
  compile and/or interpret other functional languages.
- **JVM:** The [JVM](https://openjdk.java.net/) is a high-performance virtual
  machine that includes sophisticated garbage collection, profiling tools, and
  a JIT compiler.
- **GraalVM:** A universal virtual machine and language development toolkit,
  [GraalVM](https://www.graalvm.org/) provides a framework for building language
  interpreters, as well as a JIT compiler. Most importantly, it provides tools
  for seamless interoperability between languages that can run on Graal, which
  include Python and R.

The decision to build Enso's runtime using GraalVM was primarily motivated by
business concerns, but these concerns did not override the technical as well.
Addressing them one by one provides a comprehensive picture of why the decision
was made.

Overall, it is clear that GraalVM is an optimal choice for Enso at this stage of
the language's development. While the other potential targets do have their
upsides (e.g. the JVM's sophisticated garbage collection machinery), they all
had at least one 'fatal flaw' for Enso's use case.

### Speed of Development
A language runtime is a complex beast, so any solution that could remove some of
the implementation burden would be beneficial to Enso as a product.

Where LLVM provides comprehensive tools for compiling languages, it provides no
actual runtime. This would require significant implementation effort, requiring
the implementation of facilities for concurrency, as well as garbage collection,
neither of which are simple tasks.

GHC, on the other hand, provides a comprehensive runtime system that includes
both a garbage collector and sophisticated concurrency system. However, while it
does provide language-agnostic intermediate representations, these are tied to
Haskell from a development perspective. Unlike LLVM, GraalVM, or even the JVM,
if GHC Haskell requires a change to these representations, that change will be
made.

With many languages already targeting the JVM it also seemed like an attractive
option. The stable bytecode target would be useful, but other languages have
proven the challenges of generating sensible bytecode to provide good language
performance.

GraalVM manages to provide excellent performance with a sensible, high-level
interface, thereby enabling rapid development of a performant runtime without
the need to implement complex components such as a GC and concurrency.

### Language Interoperability Support
With Enso aiming to be the be-all and end-all for the data-science world, the
ability to seamlessly interoperate with other programming languages is key. This
means that a user should be able to paste in some Python or R code and have it
work properly.

From a simple perspective, there were no other options in this category. While
the JVM would allow for interoperability with other JVM languages such as Scala,
Kotlin, and Java itself, the two 'most important' languages for interoperation
had no support. LLVM's story is similar, allowing users to use LLVM IR as a
common interoperation format, but this is far less practical than the JVM. With
GHC, any interoperation would have to be developed from-scratch and by hand,
essentially ruling it out in this category.

With GraalVM supporting not only our primary interoperability targets, but also
the whole JVM ecosystem and any language that targets LLVM, it is an absolute
dream for ensuring that Luna can seamlessly communicate with a whole host of
other programming languages.

### Implementation Performance
Data science often involves the manipulation of very large amounts of data, and
ensuring that an interactive environment like Enso doesn't slow down as it does
so requires a high level of performance.

GraalVM's partially-evaluated-interpreter based approach allows the developers
to write a 'naive interpreter' and automatically have the platform provide
better performance. This is a stark contrast to all of the other listed options,
each of which would require significant complexity around generating the right
intermediate representation structures, as well as significant work on front-end
language optimisations.

In essence, GraalVM provides for the best performance with the smallest amount
of effort, while still providing comprehensive facilities to improve performance
further in the future.

### Maintenance Burden
Just as important as getting a working runtime is the ability for the developers
to improve and evolve it. This encompasses many factors, but Enso is primarily
concerned with being able to evolve without having to account for undue changes
to the runtime.

LLVM provides a relatively stable IR target, so the maintenance burden wouldn't
have been too onerous. Similarly for the JVM, where the bytecode format has been
stable for many years. Though both projects add new instructions, they very
rarely remove them, meaning that Enso's potential code generator would be able
to work as the underlying platform evolves.

As mentioned before, however, the intermediate representations in GHC that Enso
would have used as a target are very much changeable. This is due to their
primary existence being to support GHC's version of Haskell, which means that
they change often. Furthermore, their generation would require copying of many
of the idiosyncrasies of GHC's lowering mechanisms, and in all likelihood place
a significant burden on Enso's developers.

GraalVM, on the other hand, provides a stable interface to writing an
interpreter that is far higher level than any of the other options. This API is
very unlikely to change, but even if it does the high-level nature means that
the maintenance burden of coping with those changes is significantly reduced.
Furthermore, GraalVM comes with the truffle toolkit for building interpreters,
and as a result provides many of the facilities required by Enso for free or at
least for little effort.

# The Runtime Components
Like any sensible large software project, Enso's runtime is modular and broken
down into components. These are described in detail below.

## Language Server
The language server component is responsible for controlling the runtime itself.
It communicates with other portions of the ecosystem (such as the REPL and the
Enso Studio backend) via a protocol. While this protocol is based on the
[Language Server Protocol](https://microsoft.github.io/language-server-protocol/specification),
it has been extended significantly to better support Enso's use-cases.

<!-- TODO
- A description of the protocol format.
- A description of bidirectional protocol operation. This means that Enso's
  runtime is not purely a server, but can also push data to the client.
- A description of how the protocol design admits extensibility.
- An informal description of each of the protocol messages, for example \
  (`expandOptionalArgs`, which expands all defaulted arguments in a call with
  the defaults as the values). Needs to account for on-demand opt, metadata
  handling.
- A description of how a Luna process should manage source files in server mode,
  including a description of changesets (dual payload, text diff or AST diff).
- https://github.com/luna/luna/issues/365
-->

## Filesystem Driver
This component of the runtime deals with access from the runtime to external
devices. This includes the Enso code files on disk, but is also responsible for
watching filesystem resources (such as databases, files, and sockets) that are
used by Enso programs.

<!-- TODO
- A diagram of the interactive file-system watching.
- A description of how this layer words.
- A design for the strategy for reloading based on source-data changes.
-->

## Typechecker
The typechecker is the portion of the runtime that handles the type-inference
and type-checking of Enso code. This is a sophisticated piece of machinery, with
the primary theory under which it operates being described in the specification
of [the type system](../types/README.md).

<!-- TODO
- A description of the typechecker's architecture as graph transformations.
- An analysis of how the typechecking process interacts with the interpreter.
- An analysis of how we can associate type information with nodes in the Enso
  graph. A description of what information can be erased.
- An analysis of how the typechecker will support for runtime metaprogramming
  and the manipulation of types.
-->

## Optimiser
With much of Enso's performance relying on the JIT optimiser built into Graal,
the native language optimiser instead relies on handling more front-end specific
optimisations.

<!-- TODO
- A diagram of the optimisation process.
- A description of its architecture.
- A description of the optimisations that it needs to perform to generate a
  sensible input to GraalVM.
- A description of additional transformations it needs to perform (e.g. for
  the handling of strictness).
- A design for hierarchical description of optimisation passes.
- A design for parallel local optimisation.
-->

## Interpreter and JIT
The interpreter component is responsible for the actual execution of Enso code.
It is built on top of the Truffle framework provided by GraalVM, and is JIT
compiled by GraalVM.

<!-- TODO
- A design for encoding the execution model for Lazy and Strict computations.
- A design for encoding the evaluation of monadic contexts (how `=` works).
- A design for the compilation strategy (what is resolved when).
- A design for how we work with wired-in functionality (e.g. the stdlib).
- An analysis of techniques that can be used to minimise interpreter startup
  time.
- A description of support for library precompilation.
- An analysis of how the interpreter is involved in the typechecking process.
-->

# Cross-Cutting Concerns
The runtime also has to deal with a number of concerns that don't fit directly
into the above components, but are nevertheless important parts of the design.

## Caching
The runtime cache for Enso is a key part of how it delivers exceptional
performance when working on big data sets. The key recognition, as seen in many
data processing tools, is that changing code or data often doesn't require the
interpreter to recompute the entire program. Instead, it can only recompute the
portions that are required of it, while using cached results for the rest.

<!-- TODO
- Describe the architecture of the cache.
- Describe the dependency-tracking, keying, and cache eviction strategies with a
  focus on granularity, performance, and type information (e.g. strictness).
- Describe the LRU mechanism that can be used to constrain cache size to under a
  certain amount of RAM.
- A description of how the cache is made IO aware and operates in relation to
  the filesystem layer (see Skip for more ideas).
- An examination of how cache state can be persisted to disk to enable fast
  reloading of analysis projects.
-->

## Profiling and Debugging
Similarly important to the Enso user experience is the ability to visually
debug and profile programs. This component deals with the retrieval, storage,
and manipulation of profiling data, as well as the ability to debug programs in
Enso using standard and non-standard debugging paradigms.

<!-- TODO
- An analysis of how breakpoints can be set in the Truffle interpreter.
- A design for a framework / API for introspection of the interpreter state.
- An analysis of how the JVM tools can be used to collect Enso-side profiling
  information.
- A design for this profiling data collection and a discussion of how to expose
  it to users.
-->

## Foreign Language Interoperability
This component deals with using the GraalVM language interoperability features
to provide a seamless interface to foreign code from inside Enso.

<!-- TODO
- A design for standard, unsafe, C-level FFI using JNI.
- A design for what types can be exposed across the C-FFI boundary.
- A design for how to expose foreign languages to Luna in a safe fashion.
- An analysis of how Luna can minimise the conversions that take place when
  going between languages.
-->

## Lightweight Concurrency
Though not strictly a component, this section deals with how Enso can provide
its users with lightweight concurrency primitives in the form of green threads.

<!-- TODO
- Examine how the JVM's basic concurrency primitives can be used in Enso.
- A design for how these can be used for automatic parallelism.
- An examination of how Project Loom could be employed to provide users with
  lightweight concurrency in Enso, thereby avoiding async/await 'colouring' of
  functions.
-->

# The Initial Version of the Runtime
In order to have a working version of the new runtime as quickly as possible, it
was decided to design and build an initial, stripped-down version of the final
design. This design focused on development of a minimal working subset of the
runtime that would allow Enso to run.

<!-- TODO
- Describe a design for a dynamic-only runtime
- Describe hardcoded support for IO, State, Exception (!) monads
-->

# Development Considerations
As part of developing the new Enso runtime, the following things need to be
accounted for. This is to ensure that the eventual quality of the software is
high, and that we also provide a product that is actually useful to our users.

- **Benchmarking:** A comprehensive micro and macro benchmark suite that tests
  all the components of the runtime. This should be accompanied by a regression
  suite to catch performance regressions.
- **Execution Tests:** A test suite that checks that executing Enso programs
  results in the correct outputs.
- **Typechecker Tests:** A test suite that ensures that changes made to the
  typechecker do not result in acceptance of ill-typed programs, or rejection of
  well-typed programs.
- **Caching Tests:** A test suite that ensures that data is evicted from the
  cache when it should be, and retained when it should be.
