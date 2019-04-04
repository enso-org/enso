___
- **Feature Name:** Runtime 2.0
- **Start Date:** 2019-03-26
- **Change Type:** Breaking
- **RFC Dependencies:** Syntax Overhaul, Modules
- **RFC PR:** Leave Empty
- **Luna Issue:** Leave Empty
- **Implemented:** Leave blank, this will be filled with the first version of
  the relevant tool where it is implemented.

# Summary
A one-paragraph, high-level summary of the proposal.

# Motivation
Why should we make this change? What use-cases does it support? What benefits
does it bring to the ecosystem? Explain why the status quo is insufficient or
not ideal.

# Architectural Overview
<!--
- A diagram of the overall architecture for the runtime.
- An illustration of how the runtime fits into the broader Luna system.
- A brief bullet-pointed list of the layers and their key features.
-->

## Runtime Layers
The Luna runtime consists of a number of discrete layers from a design
standpoint, each of which handles a separate part of the runtime's function.
While the responsibilities of these layers are usually well-defined, they will
need to be fairly tightly integrated, primarily for performance.

Please note that these layers aren't intended to relate directly to
architectural components at the code level. There will likely be the need for
additional components, and some of these layers may actually be different uses
of the same architectural component (e.g. the JIT layers).

<!--
- A set of brief descriptions of each of the layers.
- A diagram that shows how they interact, and the approximate communication flow
  between them.
-->

### 1 - The Edge Layer
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

### 2 - The Runtime Protocol
<!--
- An analysis of what is required to efficiently parse and respond to protocol
  messages.
- A description of the unified protocol that handles explicit control over the
  runtime's operation, as well as the features required by an IDE-protocol.
- The IDE-protocol portion should reflect discussions with David (held on email
  and recorded here: https://github.com/luna/luna/issues/365)
- A list of the protocol messages with informal descriptions (spec to come
  later).
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
-->

### 3 - The Compilation Layer and Type-Checker
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
-->

### 4 - The Cache Layer
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

### 5 - The Byte-Code Interpreter
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

### 6 - JIT Tier 1
<!--
- An examination of the kind of optimisations would be performed by this JIT
  tier (the specifics can come later).
- A description of the trade-off this JIT layer makes.
- An analysis of how we can use the W^X mitigation.
- An examination of the JIT as a solution to non-type-erased code.
- An analysis of the approximate optimisation pipeline (e.g. Luna IR -> GHC Core
  -> Core2Core -> STD -> Native Code -> Load into JIT)
-->

### 7 - JIT Tier 2
<!--
- An examination of the kind of optimisations would be performed by this JIT
  tier (the specifics can come later).
- A description of why we want a second JIT stage, and the anticipated
  performance benefits.
- A discussion of the drawbacks of this layer (primarily compilation cost).
- An analysis of how the optimisation pipeline would differ in this tier.
-->

## Cross-Cutting Concerns
There are a number of elements of the design for the new runtime that cannot be
easily partitioned into the above layers. These are explored below from the
standpoint of requirements and high-level design, and will be integrated into
multiple (if not all) of the above layers.

### 1 - FFI
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
-->

### 2 - JIT Tracing
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
-->

### 3 - Concurrency
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

### 4 - Debugging and Performance Tracing
<!--
- An examination of how performance tracing can be achieved based on the JIT's
  trace.
- A description of what features we want out of the debugger.
- An examination of what kind of debugging support we can get for free from the
  bytecode interpreter, and what we would need to build on top.
-->

<!-- !!!! DETAILED DESIGN SECTIONS BELOW !!!! -->

# The Edge Layer

# The Runtime Protocol

# The Compilation Layer and Type-Checker

# The Cache Layer

# The Byte-Code Interpreter

# JIT Tier 1

# JIT Tier 2

# FFI

# JIT Tracing

# Concurrency

# Debugging and Performance Tracing

# Language Embedding
It is an eventual goal for Luna, and hence this runtime design, to be able to
embed other languages (e.g. Python and R) for seamless interoperability.

<!--
- An analysis of whether this is possible with the GHC-based runtime without
  significant overhead.
- An analysis of how this might be accomplished.
- ESA Plugins as Optimiser Plugins
- No-overhead with multiple language nodes connected together. 
-->

# Benchmarking the Runtime
<!--
- A description of how the runtime will be benchmarked.
- A description of how regressions will be caught.
- An analysis of any external infrastructure to allow for automated regression
  discovery.
-->

# AOT Compilation
While Luna's runtime is not intended for the production of AOT-compiled binaries
for Luna programs, it just so happens that much of the work on the runtime is
also applicable to this scenario.

<!--
- An analysis of what portions of the runtime work can be used to allow AOT
  compilation.
- A brief elucidation of the _additional_ functionality needed to enable the
  AOT compilation workflow for Luna.
-->

# Acceptance Criteria
This new runtime for Luna is a gargantuan effort, but that means that we need to
be all the more rigorous when it comes to defining what 'success' means for this
addition to the project.

<!--
- The scope of the whole project. 
- What is the scope of the first deliverable?
- Go into detail about the acceptance criteria for the new runtime, particularly
  around functionality, start-up time, performance, and future-proofing.
-->

# Unresolved Questions
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

# Glossary
This section is designed to define terms that may be unfamiliar to some users:

- AOT - Ahead of Time: The opposite of JIT compilation, where code is compiled 
  to binaries ahead of being executed. 
- JIT - Just in Time: Where compilation to binary or bytecode takes place as 
  needed for the execution of the program. 

<!-- END OF WIP PROPOSAL -->

<!--

#### Resources
For more information about GHC and how it could be used as part of a JIT
compiler pipeline, please see the links below:

- [GHC Manual](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/)
- [GHC API](https://hackage.haskell.org/package/ghc)
- [GHC Design Commentary](https://ghc.haskell.org/trac/ghc/wiki/Commentary)
- [GHC Core-to-Core Passes](https://www.microsoft.com/en-us/research/wp-content/uploads/1998/09/comp-by-trans-scp.pdf)
- [GHC Reading List](https://ghc.haskell.org/trac/ghc/wiki/ReadingList)
- [Core Spec](https://git.haskell.org/ghc.git/blob/HEAD:/docs/core-spec/core-spec.pdf)
- [GHC Architecture Dive](http://www.stephendiehl.com/posts/ghc_01.html)
- [GHC IR Forms](http://www.stephendiehl.com/posts/ghc_02.html)
- [Targeting Core](http://www.stephendiehl.com/posts/ghc_03.html)
- [Grin](https://github.com/grin-tech/grin)
- [Example Pipeline](https://github.com/chrisdone/prana/blob/0cbb7b4b96bbfdb4f0d6a60e08f4b1f53abdfb15/prana-ghc/src/Prana/Ghc.hs#L106-L154)

-->
