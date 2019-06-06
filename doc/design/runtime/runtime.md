# Enso Runtime
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
  - [Typechecker](#typechecker)
  - [Optimiser](#optimiser)
  - [Interpreter and JIT](#interpreter-and-jit)
- [Cross-Cutting Concerns](#cross-cutting-concerns)
  - [Profiling and Debugging](#profiling-and-debugging)
  - [Lightweight Concurrency](#lightweight-concurrency)
  - [Foreign Language Interoperability](#foreign-language-interoperability)
- [The Initial Version of the Runtime](#the-initial-version-of-the-runtime)

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

## The Runtime's Architecture
In order to better appreciate how the components specified below interact, it is
important to have an understanding of the high-level architecture of the runtime
itself.

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

## Typechecker

## Optimiser

## Interpreter and JIT
<!-- Including the cache -->

# Cross-Cutting Concerns
The runtime also has to deal with a number of concerns that don't fit directly
into the above components, but are nevertheless important parts of the design.

## Profiling and Debugging

## Lightweight Concurrency

## Foreign Language Interoperability

# The Initial Version of the Runtime
In order to have a working version of the new runtime as quickly as possible, it
was decided to design and build an initial, stripped-down version of the final
design. This design focused on development of a minimal working subset of the
runtime that would allow Enso to run.

TBC...

<!--

Other:
- Initial Version (Dynamic, hardcoded monad support)
- Why (not GHC or LLVM - primarily business decisions)

https://drive.google.com/file/d/1ImuEySnsfHeMGD94pBvM2DEYc2iJfNW7/view

-->

<!-- # Motivation
For Luna to reach its full potential as a general-purpose programming language
and data-processing environment, it needs one major thing: speed. With the goal
for the language to become _the_ platform for end-to-end development and
communication, spanning from engineers all the way to executives, it needs to be
able to get out of the user's way and work in the background. In other words,
and to take a leaf from Apple's book: it should Just Work.

The current Luna runtime was never intended for long-term or production use, and
has been relied upon far longer than it should. During that period of time, the
design goals for Luna and its platform have solidified, making this the perfect
opportunity to deliver a new runtime that accommodates those goals while
delivering increased performance, and future-proof capabilities.

In doing so, Luna's performance will be dramatically increased, but that is only
one of the major benefits. Alongside this, the new runtime will allow Luna to be
decoupled from Luna Studio via the IDE protocol. It will let Luna interact with
C libraries with negligible overhead. It will, in essence, let Luna achieve its
full potential.

This design document sets out both the high-level architecture of the new
runtime, including detailed explorations of its features and concerns, but it
also contains the detailed designs and implementation plans for each portion of
the new Luna platform. It is intended to both serve as a design plan for the
implementation and, once that is all complete, as documentation for Luna's
design as it evolves. -->


<!-- # Architectural Overview
It is perhaps a touch rich to call this design the 'runtime', as it actually
encompasses a broader portion of the compiler than what would traditionally be
considered a runtime. Due to some of the design goals for Luna, this design also
encompasses changes to the type-checker, the IDE protocol, a JIT, and various
other things necessary for making this all work.

The Luna runtime is based on heavy usage of the Haskell-independent
infrastructure provided by the [GHC (Glasgow Haskell Compiler)](https://gitlab.haskell.org/ghc/ghc)
project. This is because we want to take advantage of the incredibly
sophisticated GHC RTS, as it provides facilities for concurrency, parallelism,
FFI, and garbage collection, alongside others. GHC will be used to provide the
GHC Core IR, from which we will be able to generate STG for use with the GHC
bytecode interpreter (used in GHCi), and for native compilation and dynamic
loading as part of the JIT.

The Luna Runtime integrates across most of the current design for the Luna
compiler, so it's easier instead to diagram the whole compiler, as below. In
this diagram, the direction of arrows represents the flow of information.

```

+---------------------------------------------------------+
| Edge Layer                                              |
+---------------------------------------------------------+

+---------------------------------------------------------+
| Protocol Layer                                          |
+---------------------------------------------------------+

+------------------------------+
| Parser                       |
+------------------------------+

+------------------------------+               +----------+
| Desugarer                    |               | Debugger |
+------------------------------+               | Engine   |
                                               |          |
+------------------------------+  +---------+  |          |
| Typechecker                  |  | Tracing |  |          |
+------------------------------+  | Engine  |  |          |
                                  |         |  |          |
+------------------------------+  |         |  |          |
| Compilation Layer            |  |         |  |          |
+------------------------------+  |         |  |          |
                                  |         |  |          |
+------------------------------+  |         |  |          |
| Cache Layer                  |  |         |  |          |
+------------------------------+  |         |  |          |
                                  |         |  |          |
+----------------------+          |         |  |          |
| Bytecode Interpreter |          |         |  |          |
+----------------------+          |         |  |          |
                                  |         |  |          |
                         +-----+  |         |  |          |
                         | JIT |  |         |  |          |
                         +-----+  |         |  |          |
                                  |         |  |          |
+------------------------------+  |         |  |          |
| GHC RTS                      |  |         |  |          |
+------------------------------+  +---------+  +----------+

+------------------------------+
| FFI                          |
+------------------------------+


```

While the diagram above encompasses all of the components of the eventual Luna
compiler architecture, the following components are those that are described
within this design:

- **Edge Layer:**
- **Protocol Layer:**
- **Typechecker:**
- **Compilation Layer:**
- **Cache Layer:**
- **Bytecode Interpreter:**
- **JIT Tier 1:**
- **FFI:**
- **Tracing Engine:**
- **Debugging Engine:**
- **GHC RTS:**

-->

<!-- ## Runtime Layers
The Luna runtime consists of a number of discrete layers from a design
standpoint, each of which handles a separate part of the runtime's function.
While the responsibilities of these layers are usually well-defined, they will
need to be fairly tightly integrated, primarily for performance.

Please note that these layers aren't intended to relate directly to
architectural components at the code level. There will likely be the need for
additional components, and some of these layers may actually be different uses
of the same architectural component (e.g. the JIT layers).

-->

<!--
- A set of brief descriptions of each of the layers.
- A diagram that shows how they interact, and the approximate communication flow
  between them.
-->

<!-- ### 1 - The Edge Layer -->
<!--
- A diagram of the interactive file-system watching.
- A diagram of the protocol interactions.
- This layer handles:
  + Managing code files.
  + Managing and watching data on disk.
  + Communication between the runtime and the protocol client.
- A description of the strategy to determine when to reload data based on disk
  changes.
-->

<!-- ### 2 - The Protocol Layer -->
<!--
- An analysis of what is required to efficiently parse and respond to protocol
  messages.
- A description of the unified protocol that handles explicit control over the
  runtime's operation, as well as the features required by an IDE-protocol.
- The IDE-protocol portion should reflect discussions with David (held on email
  and recorded here: https://github.com/luna/luna/issues/365)
- A list of the protocol messages with informal descriptions (spec to come
  later), for example (`expandOptionalArgs`, which expands all defaulted
  arguments in a call with the defaults as the values).
- A description of protocol support for performance data collection (potentially
  integrated with the IDE protocol).
- A description of protocol support for debugging (integrated with the IDE
  protocol).
- A description of how to design the protocol to admit extensions.
- An examination of how this supports building a rich REPL interface, and how it
  supports Luna Studio.
- A description of how files are controlled based on the protocol impl, and how
  they should be hosted by the Luna process.
- A description of the interaction between the protocol and the typechecker.
- A description of how changesets for open files should be handled.
- A mechanism for handling the notion of active and passive 'layers', as well as
  on-demand optimisation.
- An analysis of how the graph layout and metadata is handled. This should not
  longer be associated with explicit metadata in the source.
- An analysis of how to handle the necessary callbacks to this layer
- A design for handling metadata internally while keeping it at the end of the
  file so as not to interfere with code.
-->

<!-- ### 3 - The Compilation Layer and Type-Checker -->
<!--
- A diagram of the compilation process.
- A description of the interaction between this and Luna-native passes.
- A list of requirements placed on the Luna Core optimiser to allow for
  generation of proper GHC core (e.g. generating core for TCO).
- The code-generator will have to handle explicit strictness annotations.
- A list of things to encode in GHC Core and things that get erased. Particular
  focus on our type-system and whether we should (or how we can) encode rows.
- A description of the compilation strategy: eager + on-demand loading to ensure
  that as little time as possible is spent waiting.
- An analysis of how on-demand evaluation for type-checking should work. A
  restriction on what can be encoded (can only evaluate known-typed exprs).
- A design for exposing a hierarchical structure for optimisation passes (e.g.
  `+Pass.Optimisation.TCO`).
- An analysis of Luna-side optimisations required for the new runtime.
- An analysis of what type-erasure (if any) we can get away with at the Luna
  level (in the end GHC will type-erase our core). Any retained type info should
  be contained in-line in the Luna IR.
- An analysis of how to avoid explicitly encoding any types in the assumptions
  of the rest of the runtime (allowing for later addition of dependent and
  linear types).
- While Luna is statically typed, the runtime manipulation of types provides
  less opportunities for usage-analysis based erasure than languages like Idris
  or Agda. However, it is likely still possible that we can apply a
  usage-analysis pass to the Luna Core graph. Think about the `Dynamic` type.
- The analysis of _relevance_ of type information is interesting, and
  potentially we can learn some lessons from the progress of Dependent Haskell.
- A list of things that we need to avoid in the generated core (e.g. an
  excessive number of coercions).
- An analysis of the potential implementation burden from additions to GHC core.
- An analysis of techniques that this layer can employ to minimise the runtime
  start-up time:
  + **Dynamic Layering:** Precompilation of portions of code not in the active
    layer could be performed. This would provide increased performance, but
    there must be significant care taken to ensure that appropriate code is
    deoptimised when necessary (de-specialisation).
  - **Optimisation without Tracing:** Code that is compiled in the background
    can have general optimisations done to it that can then be improved upon
    using the input from the tracing process later on.
  - **Static Tracing:** The decisions on the order for background optimisation
    can be made via static analysis on the Luna IR graph. The code that is used
    'soonest' from the `main` function should be compiled and optimised first.
  - **Library Precompilation:** There is some potential to ship our compiler
    with (platform-specific) precompiled-to-bytecode libraries.
  - **Parallelism:** The listed startup tasks should happen in parallel as much
    as possible. The biggest opportunity for this is likely the generation of
    GHC Core from the Luna IR, but parsing can also potentially be parallelised
    (but requires discovery to be done properly).
  - **Tailored Passes:** Based on the kinds of execution that functions are
    seeing it is possible to select sets of Luna IR and GHC Core optimisation
    passes to best improve that function's performance.
- An examination of how we deal with 'wired-in' functionality (e.g. relying on
  Haskell libraries for the stdlib for now).
- An examination of how we can use levity polymorphism internally to improve
  performance.
-->

<!-- ### 4 - The Cache Layer -->
<!--
- The actual architecture of the runtime cache:
  + A description of the keying strategy.
  + A description of the dependency-tracking strategy.
  + A description of the eviction strategies in use, especially concerning type
    alterations and specialisation. It needs to account for changes in (inputs,
    outputs, type (incl. Monad, Exception), code, and code that it depends on or
    depends on it, strictness).
  + The mechanisms by which it allows for hot-reloading (keeping data around
    where possible)
  + The LRU mechanism that maintains some N sets of in/out for each code block.
  + The unit of program functionality that the cache works with: what the level
    of granularity and whether it should be tunable
- A diagram of the cache architecture and sharding approach
- A discussion of how we compensate for the magic of the cache in predictable
  performance: some portions of the caching should be _optional_ to aid in this.
- An accounting for how strictness and laziness interact with the cache.
- A description of how the cache interacts with layer 1, making it IO-aware.
- An analysis of _what_ to cache, and how it can be tuned for memory (and disk)
  usage (e.g. caching of infinite structures).
- An examination of how cache state can potentially be serialised to disk to
  load projects more quickly (needs to handle external changes to the code and
  invalidate the loaded cache based on this).
- This should be informed by Skip, a programming language that caches results
  where possible.
-->

<!-- ### 5 - The Byte-Code Interpreter -->
<!--
- A description of how the GHC bytecode interpreter will be used to evaluate
  Luna programs.
- A description of how the JIT'ed code is going to be linked back into the
  interpreter process and the JIT hot-swap mechanism (e.g. the 'plugins')
  mechanism.
- An analysis of the interpreter's role in type-checking, allowing for
  evaluation of programs to compute types, and then graph reduction by the Luna
  TC and optimiser. Graph reduction as an optimisation strategy.
- An analysis of how best to combine strict evaluation with optional laziness.
-->

<!-- ### 6 - JIT -->
<!--
- An examination of the kind of optimisations would be performed by this JIT
  tier (the specifics can come later).
- A description of the trade-off this JIT layer makes.
- An analysis of how we can use the W^X mitigation.
- An examination of the JIT as a solution to non-type-erased code.
- An analysis of the approximate optimisation pipeline (e.g. Luna IR -> GHC Core
  -> Core2Core -> STD -> Native Code -> Load into JIT)
- An examination of the kind of optimisations would be performed by subsequent
  JTI tiers.
- A description of why we want a second JIT stage, and the anticipated
  performance benefits.
- A discussion of the drawbacks of subsequent JIT stages (primarily compilation
  cost).
- An analysis of how the optimisation pipeline would differ across JIT tiers.
- A description of a mechanism that can be used to track the performance of the
  JITed code, and deoptimise it if the binary is slower than the bytecode.
-->

<!-- ### 7 - JIT Tier 2 -->
<!--
- An examination of the kind of optimisations would be performed by this JIT
  tier (the specifics can come later).
- A description of why we want a second JIT stage, and the anticipated
  performance benefits.
- A discussion of the drawbacks of this layer (primarily compilation cost).
- An analysis of how the optimisation pipeline would differ in this tier.
>>>>>>> origin/master
-->

<!-- ## Cross-Cutting Concerns
There are a number of elements of the design for the new runtime that cannot be
easily partitioned into the above layers. These are explored below from the
standpoint of requirements and high-level design, and will be integrated into
multiple (if not all) of the above layers.

-->

<!-- ### 1 - FFI -->
<!--
- A diagram of how FFI calls work, and the support libraries needed.
- A description of how we want FFI to work, and its performance characteristics.
- A list of what we need from user code to provide enough information to the
  backend to properly encode the FFI calls.
- A description of how we will ensure that FFI calls remain as low-overhead as
  possible.
- An analysis of what types can be used across the C-FFI boundary. Support for
  value structs where possible (using compiler layout assumptions).
- An analysis of the potential to support callbacks to Luna from C, and the
  support for running Luna programs from C.
- An analysis of how best to translate Haskell's FFI semantics into Luna.
- The interpreter itself currently doesn't support `foreign export`, so these
  will need to be compiled by the first JIT tier separately and then dynamically
  loaded.
-->

<!-- ### 2 - Tracing Engine -->
<!--
- A description of the mechanisms by which execution is traced.
- A description of _what_ data is tracked and how it is used to make decisions
  about the JIT. Time and memory.
- An examination of which portions of this are required for phase one of the
  implementation.
- A description of how this functionality can be used for inbuilt performance
  tracking.
- An examination of what traces are used for:
  + Forced inlining of traces to ensure optimisation of the whole trace.
  + Performance annotation.
- An exploration of how we trace enough data without slowing down the bytecode
  interpreter stage too much. Tracing calls will be eliminated in the JIT'ed
  code.
- An examination of how performance tracing can be achieved based on the JIT's
  trace.
- An exploration of what mechanisms we can apply to get faster warm-up times
  (e.g. static tracing, on-demand optimisation). Minimisation of the necessary
  initial tasks:
  1. Lexing and Parsing of Luna source code, coupled with generation of the Luna
     IR graph.
  2. Type-checking of Luna IR and any reduction that may take place (see later).
  3. Generation of GHC Core from Luna IR.
  4. Translation of Core to Bytecode for initial interpretation.
- An examination of how the JIT's automatic optimisation should interact with
  the on-demand optimisation available to Luna Studio.
- An analysis of the stages of trace information:
  1. Profiling information is collected during execution. This is traditionally
     for loops (or recursive calls), but can be augmented to compute hot paths
     and other useful information.
  2. Once a code path is considered 'hot', the JIT records an execution trace of
     the exact instructions executed, including functions for inlining. This
     trace is often stored as IR, but Luna can do better by annotating the IR
     graph.
  3. The resultant trace consists of one execution path, which can be optimised
     easily. Guard instructions are inserted as appropriate into the trace to
     ensure that the assumptions made during collection still hold.
  4. The trace is optimised, including CSE, dead-code elimination, escape
     analysis, heavy inlining and constant folding.
  5. The compiled trace is executed until a guard fails, forcing deoptimisation.
= A description of how this tracing functionality can be used to provide useful
  compiler-wide logging.
-->

<!-- ### 3 - Concurrency -->
<!--
- An exploration of how the runtime will need to handle concurrency.
- A description of which GHC primitives and RTS operations we can rely on.
- A description of how the bytecode interpreter helps achieve concurrency in the
  new runtime.
- An analysis of techniques for automatic parallelism that defer to manual
  parallelism where necessary. How do these interact with stateful and IO-based
  computation?
- An analysis of how GHC's concurrency primitives can be used to retain as much
  concurrency performance as possible.
- An exploration of techniques to avoid async/await 'colour'.
-->

<!-- ### 4 - Debugging Engine -->
<!--
- An examination of how performance tracing can be achieved based on the JIT's
  trace.
- A description of what features we want out of the debugger.
- An examination of what kind of debugging support we can get for free from the
  bytecode interpreter, and what we would need to build on top.
-->

<!-- !!!! DETAILED DESIGN SECTIONS BELOW !!!! -->

<!-- # The Edge Layer -->

<!-- # The Protocol Layer -->

<!-- # The Compilation Layer and Type-Checker -->

<!-- # The Cache Layer -->

<!-- # The Byte-Code Interpreter -->

<!-- # JIT -->

<!-- # FFI Support -->

<!-- # Tracing Engine -->

<!-- # Concurrency -->

<!-- # Debugging Engine -->

<!-- # Language Embedding
It is an eventual goal for Luna, and hence this runtime design, to be able to
embed other languages (e.g. Python and R) for seamless interoperability.

-->

<!--
- An analysis of whether this is possible with the GHC-based runtime without
  significant overhead.
- An analysis of how this might be accomplished.
- ESA Plugins as Optimiser Plugins
- No-overhead with multiple language nodes connected together.
-->

<!-- # Benchmarking the Runtime -->
<!--
- A description of how the runtime will be benchmarked.
- A description of how regressions will be caught.
- An analysis of any external infrastructure to allow for automated regression
  discovery.
-->

<!-- # AOT Compilation
While Luna's runtime is not intended for the production of AOT-compiled binaries
for Luna programs, it just so happens that much of the work on the runtime is
also applicable to this scenario.

-->

<!--
- An analysis of what portions of the runtime work can be used to allow AOT
  compilation.
- A brief elucidation of the _additional_ functionality needed to enable the
  AOT compilation workflow for Luna.
-->

<!-- # Acceptance Criteria
This new runtime for Luna is a gargantuan effort, but that means that we need to
be all the more rigorous when it comes to defining what 'success' means for this
addition to the project.

-->

<!--
- The scope of the whole project.
- What is the scope of the first deliverable?
- Go into detail about the acceptance criteria for the new runtime, particularly
  around functionality, start-up time, performance, and future-proofing.
-->

<!-- # Unresolved Questions
This section should address any unresolved questions you have with the RFC at
the current time. Some examples include:

- Is there potential to upstream portions of the JIT into GHC itself? This could
  bring a whole new execution paradigm to the Haskell ecosystem if so.
- What kind of maintenance burden can we expect when changing to new GHC
  versions? The GHC API tends to change fairly often, so we have to account for
  that in the design.
- Is it worth creating our own wrapper around the necessary parts of the GHC API
  to allow the change surface on version bumps to be minimised? Some use of
  type-level programming could likely help with correctness around strictness
  and laziness, as well as boxed and unboxed types.
- What are the security implications for the language while building a JIT
  compiler?
- What is the exact boundary of stage 1. It looks like the JIT tier may be
  needed after all (for FFI). It may, in the end, actually be simpler to add
  this to GHC and use a fork until it hits stable.

-->

<!-- # Glossary
This section is designed to define terms that may be unfamiliar to some users:

- **ABI** - Application Binary Interface: A well-specified and defined interface
  between multiple binary program components (as opposed to an API, which
  operates at the level of program code).
- **AOT** - Ahead of Time: The opposite of JIT compilation, where code is
  compiled to binaries ahead of being executed.
- **FFI** - Foreign Function Interface: A mechanism by which functions written
  in another language can be called, usually operating via the C ABI.
- **IR** - Intermediate Representation:
- **JIT** - Just in Time: Where compilation to binary or bytecode takes place as
  needed for the execution of the program.
- **RTS** - Runtime System: A program that provides the underlying primitives
  and functionality for a programming language to execute. When used in this
  document, it exclusively refers to the GHC Runtime System.

-->

<!-- END OF WIP PROPOSAL -->
