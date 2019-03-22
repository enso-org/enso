___
- **Feature Name:** Futureproof Runtime
- **Start Date:** 2018-08-24
- **Change Type:** Breaking
- **RFC Dependencies:**
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

# Guide-Level Explanation
Explain the proposal as if teaching the feature(s) to a newcomer to Luna. This
should usually include:

- Introduction of any new named concepts.
- Explaining the feature using motivating examples.
- Explaining how Luna programmers should _think_ about the feature, and how it
  should impact the way they use the language. This should aim to make the
  impact of the feature as concrete as possible.
- If applicable, provide sample error messages, deprecation warnings, migration
  guidance, etc.
- If applicable, describe the differences in teaching this to new Luna
  programmers and experienced Luna programmers.

For implementation-oriented RFCs (e.g. compiler internals or luna-studio
internals), this section should focus on how contributors to the project should
think about the change, and give examples of its concrete impact. For
policy-level RFCs, this section should provide an example-driven introduction to
the policy, and explain its impact in concrete terms.

# Reference-Level Explanation
This is the technical portion of the RFC and should be written as a
specification of the new feature(s). It should provide a syntax-agnostic
description of the planned feature, and include sufficient detail to address the
following points:

- Impact on existing feature(s) or functions.
- Interactions with other features.
- A clear description of how it should be implemented, though this need not
  contain precise references to internals where relevant.
- An explanation of all corner cases for the feature, with examples.

This section should be written in comprehensive and concise language, and may
include where relevant:

- The grammar and semantics of any new constructs.
- The types and semantics of any new library interfaces.

## Textual Syntax Explanation
This should explain how the new feature interacts with the textual syntax
representation of Luna. It should touch on the same categories as above.

## Graph Syntax Explanation
This should explain how the new feature interacts with the graphical syntax for
Luna. It should also touch on the same categories as above.

# Drawbacks
A description of why we _should not_ do this. Write this section as if you are
picking apart your proposal.

# Rationale and Alternatives
A few paragraphs addressing the rationale for why this design is the best
possible design for this feature in its design space. It should address how the
proposed design resolves the motivating factors.

A few paragraphs addressing alternative designs for the feature, and the reasons
for not choosing them.

A paragraph or two addressing the impact of not including this feature.

# Unresolved Questions
This section should address any unresolved questions you have with the RFC at
the current time. Some examples include:

- What parts of the design will require further elaboration before the RFC is
  complete?
- Which portions of the design will need resolution during the implementation
  process before the feature is made stable.
- Are there any issues related to this RFC but outside its scope that could be
  addressed in future independently of this RFC? If so, what are they?

# Notes

## TCO
The current interpreter/runtime performs no tail-call optimisation (TCO),
allocating a new stack frame for each recursive call where the recursive call is
in tail position. The new runtime should definitely support TCO, though the
exact details of this is unknown.

- Should TCO be an optimisation pass enabled with `-O` flags (or equivalent,
  e.g. `+Pass.Optimisation.TCO`)? This is more likely to be useful when we have
  a native target.
- It should likely be enabled by default for the interpreter, especially while
  we aren't doing many other optimisations.
- Needs to allow for _guarded recursion_ (tail recursion modulo constructors)
  to maintain lazy semantics.
- Any such optimisation should not interfere with the semantics of thunk
  evaluation.

## Runtime Design
The following designs should account for both interactive use in Luna Studio, as
well as interactive use in a future Luna REPL.

#### Runtime Design

- Luna's future as a compiled and interpreted language, with the need to support
  multiple backends.
- An effective division of the user experience into two groups of 'layers'. The
  first layer is the portion of the graph being actively manipulated by the
  user, and this should be interpreted. The second layer is the 'rest' of the
  graph, which can be compiled so as to offer the best performance.
- This needs to be more sophisticated than a traditional JIT-style method cache,
  as the nature of Luna's type-system means that we can dynamically compute
  types (dependent typing).
- This native compilation can take time, so need the ability to hot-swap code.
- Need to provide the ability for callers to control opt and deoptimisation
  levels as they see fit.
- The difficulty is what cache-invalidation strategy to use, as types can change
  dynamically in a way not possible with traditional languages (e.g. a change to
  a number can alter types).
- This interacts poorly with specialisation-based optimisation strategies.
- Need to think about how this plugs into the NCG, and think of the interpreter
  as just _one_ level of the compilation stack.
- One of the current issues with the runtime is doing (slow) lookups during
  execution. We want to avoid this as much as possible.
- The machinery around `GHC.Exts.Any` may not be the best way to implement the
  runtime data. There is some overhead associated with it, as well as
  complexity. It may be sensible to use a C-level data model, as this would
  potentially provide better interop with other languages and compilation
  stages.
- The runtime should support both PGO and manual optimisation. It should also be
  possible to mark a function as `NeverOptimise` such that PGO does not kick in
  if the function is in a live layer.

#### Caching

- Lazy eviction of layers from cache, but connected to the interactive layer.
- Needs to cache the results of computations on the graph. The only things that
  should be recomputed are those _downstream_ from the change.
- However, this cache must also be type-aware.
- The type must also be effect-aware (e.g. IO - watching files on disk, or
  MVars), but default to active evaluation.
- Freeze caching: prevents recomputation of nodes even when not up to date.
- Frozen values should save with the project.
- Frozen values have the ability to reload.
- Multi-layer caches (memory, disk, cache eviction) to control memory usage.
  Size control set in project `config.yaml`.
- Eviction strategies by age, computation time, usage, etc.
- Caching of lazy / infinite structures.
- Dirty flag propagation and invalidation, but with saving of parameters.
- It is possible that the method cache and the value cache can be one and the
  same.
- The ideas presented by [skip](http://skiplang.com/) may have some very useful
  implications for the runtime caching mechanism.

#### Graph Reduction

- The interpreter is the major part of the typechecker, allowing for the
  evaluation of type level expressions.
- Structural pattern matching for optimisation (e.g. a pure function with
  literal arguments).
- Such graph reductions are optimisation passes.
- How these reductions are propagated through the graph (as they may introduce
  new patterns) is an important point of this design.
- Reduction may elide useful things (e.g. sliders), so could be disabled or
  (hot-pathed).

#### Separate Compilation

- It should be possible to compile separate parts of the IR to separate
  languages as part of the JIT architecture.
- This would allow hybrid compilation to support web use-cases. An example of
  this is that the currently editable view could compile to JavaScript to be
  executed live on the client, with compiled code executing on the server.
- This has some complex interactions around data transfer and an object model,
  however.

#### Measurement

- Luna, as a language, should be trivially easy to profile, both when it comes
  to memory and runtime (clock and CPU time).
- Easy inbuilt profiling will both allow visual profiling in Luna Studio, and
  the command-line generation of reports (or live files).
- The new runtime must incorporate hooks for gathering this profiling info, so
  it can easily be displayed to users.
- This profiling support should _not_ instroduce a negative performance impact.
- It is likely, if based on a JIT, that the PGO infrastructure should be able
  to be utilised for this profiling information.

#### Static Analysis and Graph Compilation

- To increase the 'warm up' speed of a JIT, it is possible that static tracing
  through the Luna graph can be used to augment dynamic JIT tracing.
- Such static tracing would give _some_ idea of what code paths to optimise in
  absence of dynamic tracing information, potentially reducing JIT warm-up.

## Parallelism
As part of its usage in Luna Studio, Luna wants to offer the ability for
automatic parallelism of certain function calls. Furthermore, its use as a
programming language will provide explicit programmer control over parallelism.
The runtime needs to support both of these use-cases.

#### Runtime Parallelism

- The runtime needs to be thread-safe and support both OS-threads and
  lightweight green threading that multiplexes onto OS threads. For now this can
  be provided by the GHC RTS, but the need for this should inform any future
  designs.
- Some functions should be able to be automatically parallelised based on the
  size of their inputs (or other measures).
- This automatic parallelism should be able to be controlled by the user,
  whether through a configuration option or configuration parameter passed to
  the runtime (e.g. `+Runtime.Parallelism.Automatic`).
- Automated parallelism, however, should defer to manually-controlled
  parallelism where relevant (e.g. any situation where the two may conflict).
- Manual parallelism should provide access to OS-threads (e.g. POSIX threads),
  as well as a lightweight green-threading solution.
- Automatic splitting at branches in pure cases or cases with provably separate
  evaluations. This should be a graph-level annotation set by a pass.
- This parallelism should automatically extend to execution on non SMP systems.
  That means both NUMA systems and GPU-based compute. This is not a suggestion
  that Luna itself can execute on GPUs, but there should be native support in
  the RTS for efficiency in Data Science analysis use-cases.
- The runtime should be based on a green-threads model for handling execution of
  asynchronous code. This should work regardless of whether it is executing on
  a SMP system or not.

#### Design Considerations for Dealing with Parallelism

- Any stateful and side-effecting operations in the runtime need to be carefully
  managed so as not to introduce race conditions.
- Synchronisation for MVars so we can allow for some cross-thread communication.
- Even if they aren't exposed to the users, standard synchronisation primitives
  must _work_ in the new runtime (barrier, mutex, spinlock), as they allow for
  fine-grained concurrency control. Such control would be important for big-data
  applications where parallel data structures can be implemented using them
  (e.g. persistent tries).
- The concurrency should be implemented at as low a level as possible,
  potentially using the GHC RTS directly. The more layers we pile up, the more
  performance we lose.
- The concurrency needs to work with _both_ interpreted code and compiled code
  so as to provide a seamless experience. This means the runtime has to have
  direct access to ABI hooks in compiled code where necessary.
- Whatever is done, it mustn't cripple performance like the Python GIL does.
- Needs to have support for asynchronous computations, but avoid the problems
  that naturally arrise from an `async/await`-based [approach](http://journal.stuffwithstuff.com/2015/02/01/what-color-is-your-function/).

## RTTI
While Luna is currently simply typed (no pi-types), we fully intend for it to
evolve into a truly dependently-typed language over time as we upgrade the type
system. While we already have plans for this, the runtime needs to support it
too.

#### Types at Runtime

- The issue with dependent-types is that you can't erase _all_ type information
  at runtime. Unless this is designed sensibly, it is very possible that keeping
  extraneous type information around could bloat the runtime.
- If we can afford the memory cost, it may be better to retain _all_ type
  information at runtime, even in the compiled code. This would speed up the
  flow for optimisation and deoptimisation (and potentially yield improvements
  for the typechecker).
- If we do this, type information should be retained inline in the expression
  context as much as possible. The more indirections done to look up type info,
  the slower it is to work with type-level computations (bearing in mind that
  type and value-level computations are executed by the same interpreter).
- Any runtime type support shouldn't make any assumptions regarding the type
  universe of the types on which it is operating. Doing so would preclude future
  introductions of additional type universes such as
  [Affine Types](http://asaj.org/talks/lola16/?full#cover),
  [Linear Types](https://ghc.haskell.org/trac/ghc/wiki/LinearTypes) or
  [Uniqueness Types](http://docs.idris-lang.org/en/latest/reference/uniqueness-types.html)
  ([Substructural Types](https://en.wikipedia.org/wiki/Substructural_type_system)
  in general).
- While Luna is statically typed, the runtime manipulation of types provides
  less opportunities for usage-analysis based erasure than languages like Idris
  or Agda. However, it is likely still possible that we can apply a
  usage-analysis pass to the Luna Core graph.
- The analysis of _relevance_ of type information is interesting, and
  potentially we can learn some lessons from the progress of
  [Dependent Haskell](https://ghc.haskell.org/trac/ghc/wiki/DependentHaskell)
  in GHC.

#### Type Erasure

- While execution of type-erased code at runtime is often seen as necessary for
  the execution performance of code, this often brings limitations in situations
  where RTTI would be useful.
- Luna, with such a rich type language, has the capacity to benefit in a
  significant way from the availability of RTTI, so we don't want to erase Luna
  fully.
- A JIT, however, would provide a solution to this: as it optimises functions it
  can specialise and type-erase them for performance. The un- or less-optimised
  versions of these functions are still available in cases where the optimised
  code needs to be invalidated.

## ESA
Future plans for Luna also include embedding other languages within Luna code to
allow for seamless interoperability with said languages. Designing this
properly, and making it easy to write new bindings, will require some level of
support in the runtime. This system is known as the Embedded Syntax Architecture
(ESA).

#### Runtime Design for ESA

- Ideally, we want zero-cost data access between the languages participating in
  ESA linkages.
- This means that the runtime's data model needs to be low-level enough that any
  language can reasonably interact with it.
- It also needs to interact with ESA in a thread-safe manner.

## Evaluation Strategies
While Luna is currently a lazy language, we have plans to make it strict (with
optional laziness) in the future.

#### Laziness and Strictness

- Should be able to handle a mixture of lazy and strict data, with support for
  'viral' laziness annotations.
- Should be able to handle functions with a mixture of lazy and strict
  arguments.
- It should have well-defined evaluation strategies for both lazy and strict
  code (e.g. call-by-need for lazy code, call-by-name for strict code).
- The interpreter's design needs to be agnostic to the evaluation strategy.
- Strict by default with rich support for laziness.
- Data should be able to be 'switched' into a lazy mode, with support for both
  deep and shallow laziness.
- Shallow is lazy fields.
- Deep is lazy type, lazy fields / methods
- Types can be lazy by default.
- Every piece of data needs strictness annotations in its metadata (in addition
  to Monad, Error, Type, etc).
- Having flexible support for laziness and strictness means it is triviail to
  implement short-circuiting operations _inside_ Luna.
- For optimisation's sake, we may actually be able to generate call-by-need
  based strict evaluation, eagerly replacing thunks at evaluation time.

#### A Skeleton Design for Runtime Laziness Handling

- Every runtime value will be tagged (as part of its metadata blob, along with
  type, monad, error, etc).
- This acts as a key to tell the evaluator how to execute the code in question,
  and whether code generation needs to generate code for thunks.

## FFI
Luna's FFI currently only supports calling C ABI functions via the use of
libffi. The runtime can likely support dynamic function calls in a slightly
better fashion, and potentially extend this to different languages.

#### FFI in the Luna Runtime

- The FFI should be tied to the current back-end. As we want to support multiple
  backends, the JS backend should allow for JavaScript FFI, while the ASM
  backend should allow for C FFI.
- This means that there should be a `Native` FFI tag for multi-target projects.
- This flexibility should extend to the interpreted runtime.
- FFI calls should be as close to zero-overhead in the dynamic runtime as
  possible, while in compiled code they should be zero-cost.

## A JIT-Based Luna Runtime
The following section aims to explain why integration of a JIT Compiler into the
Luna runtime helps to accommodate many of the features described in the notes
above. It also aims to explain why using a JIT is not _just_ a benefit for the
Runtime and its featureset, but also for the Luna ecosystem as a whole.

### Why a JIT?
Luna fills a truly unique niche when it comes to programming languages, and in
order to ensure it provides the best user experience there are a significant
number of performance requirements placed on the Luna Runtime.

Meeting these requirements is no small task, especially once we begin to account
for the highly-dynamic nature of Luna's computational graphs, and the way that
types can change with expressions. As a result, the dynamism offered by a JIT,
combined with the high performance that one can bring, makes it the perfect
choice for the implementation of Luna's runtime.

### How a JIT Solves Specific Issues
As part of the Luna Runtime, use of a JIT solves many specific issues that we
are faced with for the new runtime. These include:

- **Caching for Performance:** In order to improve Luna's performance, we want
  to add support for dynamic caching of values that don't change. However, as
  types and values are both first-class in Luna, the deoptimisation strategies
  designed for use by the JIT compiler are identical to the cache eviction
  strategies for the value cache. Furthermore, building them into the same
  system means that far more sophisticated caching can be implemented, including
  caching of file-system values.
- **Dynamic Compilation:** Luna, for the most part, only needs to be interactive
  at the layer that the user is editing. As a result, we can dynamically compile
  the rest of the codebase with optimisations in order to gain performance. This
  would allow the runtime to swap in optimised functions during execution to
  provide significant performance increases. This is not true in _all_ cases,
  however, as changes to the active layer may invalidate some optimisations made
  by the JIT compiler.
- **Profiling Information:** As part of the implementation of a standard tracing
  JIT, the compiler and runtime are required to measure usage statistics
  (including memory and CPU) for pieces of code in order to determine what to
  optimise. Along the same lines, this tracing must have a very low performance
  impact. As a result, it becomes trivial to implement high-performance
  profiling of Luna code on top of the JIT tracing information.
- **Runtime Parallelism:** A properly-designed JIT is able to not only optimise
  functions dynamically, but trace execution paths such that opportunities for
  automated parallelism can be exploited.

This makes it clear that a JIT is likely to be an instrumental part of Luna's
new runtime, and that a JIT tailored especially for Luna is likely to bring
huge gains for the language.

### Additional Benefits of a JIT
While building the Luna Runtime on top of a JIT helps to implement many of the
requirements, it also brings with it some additional benefits.

- By its very nature, a JIT has to generate assembly in memory, whether that be
  optimised or not. This means that there is at least a partial implementation
  of many of the features needed for the creation of full AOT compilation. As we
  definitely want to be able to AOT compile Luna in the future, this is very
  helpful.
- A JIT is very appropriate for handling the interactivity of Luna. Functions
  that are not directly editable can be dynamically compiled for higher
  performance, while code can easily be deoptimised when it changes. This makes
  it easier to support the type-changes as well, relying just on a correctly
  implemented function-cache eviction strategy.

## An Analysis of JIT Architecture Options
There are multiple options that are worth exploring when it comes to the
integration of a JIT compiler into Luna's runtime. This section aims to record
the detailed research performed into each of these options such that a concrete
recommendation for moving forward can be made.

### GHC
GHC is not only the most sophisticated Haskell compiler in existence today, but
it is also a test-bed for ideas that explore the evolution of functional
languages. It would be brilliant to leverage the years of accrued knowledge in
the optimisation of functional language that GHC embodies.

The majority of GHC's optimisations are contained within the Core-to-core
pipeline, that uses self-contained transformations that transform expressions
within the core language. Luna would be able to utilise this pipeline by
treating GHC Core as a compilation target, and running the resultant code
through the rest of the GHC code-generation pipeline.

#### Feasibility
As a platform, GHC has seen over twenty years of continuous improvement and is
at the forefront of functional language research in both performance and
type-systems. As a potential compilation target for the Luna Runtime JIT, it is
able to provide a rich featureset that would ease the implementation burden
significantly.

The key element of this featureset is GHC's Core language. Core is a rich but
highly explicit language that has support for explicit coercions and type/kind
equalities, a must for a dependently-typed language. The ability to express much
of Luna's type-level machinery in GHC core is a huge boon, removing the need to
explicitly implement significant portions of Luna's types for compilation.

However, all is not sunshine and roses, with a choice for GHC meaning that Luna
is forever tied to it. This includes not just the positives, but also the
negative aspects of GHC such as its poor garbage collection.

Nevertheless, GHC is a compelling option for incorporation into Luna's runtime
JIT, providing an excellent compilation target. In addition, the benefits of GHC
being an open system means that Luna can improve the platform that it compiles
to, and hence bring benefits to the entire Haskell ecosystem.

#### GHC as a JIT
While GHC's core language has always been intended as a feasible compilation
target for other functional programming languages, there is currently no support
for use of GHC in a JIT-based architecture.

However, this does not mean that it is not feasible, as the compiler provides a
featured API for controlling its behaviour (see [The GHC API](#the-ghc-api)).
This means that Luna would be able to hook into the functionality of GHC that it
requires, up to and including explicit control over all the details of
compilation.

Initial research into using GHC in this way indicates that the JIT phases would
likely operate approximately as follows:

- **Tier 1:** Direct interpretation of Luna IR
- **Tier 2:** Luna IR -> GHC Core -> STG -> Native Code -> Load into JIT
- **Tier 3:** Luna IR -> IR Opt (Fast) -> GHC Core -> Core Opt (Fast) -> STG
  Native Code -> Load into JIT
- **Additional Tiers:** These would operate as for Tier 3, but would apply
  successively more expensive optimisations as they become worthwhile.

It should be noted however that, unlike both LLVM (with ORC) and GraalVM, GHC
has no native support for operation as a JIT. This means that much of the work
for this approach would revolve around building a JIT around GHC. This would
require (but isn't limited to):

- Tools for tracing analysis to know what to optimise.
- Tools for manual optimisation and deoptimisation of code.
- Tools for static tracing of Luna's IR graphcs.
- Tools for dynamic hot-swapping of compiled and interpreted code.

Furthermore, such an approach would require the availability of GHC as part of
the Luna binary distribution. This would lead to a not-insignificant increase in
the size of the Luna binary.

##### Compiler Performance
GHC is known for suboptimal compilation times for Haskell programs that utilise
sophisticated pieces of the typelevel machinery. Howver, most of this slowdown
is attributed to the renamer and typechecker, rather than the core pipeline.

Core's type language is significantly simpler, with far more left explicit. This
means that as long as Luna generates sensible core (e.g. not generating an
obscene number of [coercions](https://ghc.haskell.org/trac/ghc/ticket/8095)), it
is likely to be feasible to use GHC as part of a JIT pipeline.

It is somewhat complicated to measure, and more time needs to be spent on it,
but initial tests show that much of the time is spent in both typechecking and
the optimisation of highly suboptimal core expressions.

##### The Optimisation Pipeline (Core2Core)
The majority of GHC's optimisation work takes place in the Core2Core pipeline.
This is a set of fully independent passes that transform core expressions to
other core expressions, very similarly to how Luna's graph transformation passes
operate.

The main benefits of using GHC are thus twofold:

1. We can trivially build new optimisations for core that work as part of the
   existing optimisation pipeline.
2. Standard optimisations for imperative programs (such as those employed by
   LLVM or GraalVM) are often significantly less effective when applied to
   functional code, as discussed in the [GRIN paper](https://github.com/sdiehl/papers/blob/master/Grin.pdf).
   Functional languages rely more on whole-program optimisation for their
   performance, and so compilers built with imperative languages in mind may not
   be a good fit. GHC gives us suitable optimisations for free, where using
   LLVM or GraalVM would require us to desugar Luna to an imperative form (such
   as Cmm that GHC uses) ourselves.

This does not mean that we get _everything_ for free from GHC, however. Certain
optimisations are still made on the source language, which we would be free to
experiment with in the Luna frontend IR -> IR transformation pipeline before
generation of core.

##### GHC Core as a Compilation Target
Core is an explicitly-typed but simple language that serves as a real-world
implementation of [System-FC](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/FC)
(more info [here](|https://www.seas.upenn.edu/~sweirich/papers/fckinds.pdf)). It
comes with a rich specification of both its syntax and informal semantics, and
this is kept up to date in the [Core Spec](https://git.haskell.org/ghc.git/blob/HEAD:/docs/core-spec/core-spec.pdf).

This language, as currently implemented in GHC, supports many useful features
such as:

- Explicit type and kind equalities (necessary for efficient implementation of
  dependent types).
- A simple expression format that can be machine-generated using the GHC API (as
  a [`CoreSynType`](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType)
  representation of the Luna IR).
- Rich support for modelling types and coercions, meaning that 95% of Luna's
  metatheory can be supported in it. That which cannot be can likely be
  implemented as direct logic, much as Idris does.
- Rich support for both strictness and laziness, as well as boxed and unboxed
  types. Care needs to be taken to generate the correct core from Luna IR in
  order to ensure semantic correctness around this.

While Core is a rich target language, however, the documentation on using GHC in
such a way is lacking compared to both GraalVM and LLVM. This may increase the
rate of mistakes in implementation, and hence the work required, but this
research makes evident that using GHC would be worthwhile if this route is the
one taken.

Core is translated to STG (Spineless Tagless G-Machine), which is an efficient
model of functional language execution. This means that Luna can build on the
work done on this, utilising an eval/apply style of evaluation model, as
described in [Making a Fast Curry](https://github.com/sdiehl/papers/blob/master/Making_A_Fast_Curry.pdf).

As for the ability to target core, Stephen Diehl has a fantastic post that is
about [targeting core](http://www.stephendiehl.com/posts/ghc_03.html), and this
is likely to be a useful resource.

Furthermore, using GHC Core as a compilation target means that we have the
option of going via Cmm, GHC's enhanced version of the C-- portable assembly
language. As mentioned [here](https://lhc-compiler.blogspot.com/2009/01/why-llvm-probably-wont-replace-c.html),
and expanded upon in [this SO question's answers](https://stackoverflow.com/questions/815998/llvm-vs-c-how-can-llvm-fundamentally-not-be-better-for-haskell-than-c),
C-- has robust support for zero-cost garbage collection, a must for Luna.
Furthermore, as Edward Z. Yang states in his answer to the above question, Cmm's
model is both higher- and lower-level than LLVM IR, with support for more
natural optimisations than LLVM's SSA style (requirements for `phi` expressions
everywhere).

##### The GHC Runtime System
In addition to being a fantastic compilation target for functional languages,
GHC is accompanied by a rich runtime system (RTS). This RTS has support for both
native concurrency and multithreaded IO, making it a brilliant potential target
for Luna.

Using GHC as a compilation target would allow us to make use of the GHC RTS for
Luna's execution. As a result, a significant featureset is acquired for 'free',
instead of requiring implementation by us. Furthermore, building on top of GHC
would give us great usability features such as DWARF information, and the whole
suite of GHC's profiling tools.

Furthermore, Luna is built in Haskell, meaning that loading GHC-generated code
for use by the JIT is able to be trivially handled in this case via
[`System.Plugins.Load`](https://hackage.haskell.org/package/plugins-1.5.5.0/docs/System-Plugins-Load.html)
in conjunction with `loadFunction`.
As long as we are content that Luna will always be built in Haskell, then the
ability to trivially share code between JIT'ed and interpreted components all in
the same runtime is a huge boon.

While the GHC RTS is inherently garbage collected, it has facilities for direct
allocation and deallocation. This means that in the future it is likely possible
to implement optimisations around evaluation and memory usage predicated on the
availability of unique, linear or affine types in Luna.

#### The GHC API
GHC provides a rich programmatic interface to GHC itself, allowing Luna to have
detailed control over the compilation process. The [GHC API](https://hackage.haskell.org/package/ghc)
provides the facilities for comprehensive control over GHC, as well as analysis
based on what GHC is capable of.

#### Interaction with the FFI
In interpreted code, Luna is forced to dynamically load libraries and symbols
for executing FFI calls using the system dynamic linker (and libffi). Using a
JIT, however, means that this can be greatly improved.

GHC's Runtime System has rich support for making FFI calls, so Luna's call speed
could be dramatically improved in JIT-compiled code by generating static native
calls where possible.

#### Negatives to Using GHC
While the above constitutes a very good set of reasons to take this approach to
building a JIT for Luna's runtime, it is not without downsides. The ones that I
see as most important when making this decision are as follows:

- It would perenially tie the implementation of Luna to Haskell and, morover, to
  GHC itself. This would preclude making Luna a self-hosting language in future,
  or at least preclude doing it without significant rewrites.
- GHC's native targets are limited when compared to something like LLVM.
- GHC's support for platform-specific extensions is limited compared to the rich
  featureset that LLVM offers in this area.
- GHC's garbage collector is not really at the forefront of garbage collectors.
  There are other, more sophisticated, approaches to garbage collection that may
  have benefits for the kinds of workloads that Luna aims to tackle going
  forward.
- While Core is intended as as platform for functional languages, certain of the
  in-development initiatives are explicitly targeted at Haskell. An example of
  this is the [Lightweight Concurrency](https://ghc.haskell.org/trac/ghc/wiki/LightweightConcurrency)
  project, which would remove features from the RTS and move them into the
  source language (Haskell) instead. Such initiatives would add work for us as
  Luna's implementors.
- There is no simple way that we can efficiently pass debugging symbols through
  the compilation/JIT pipeline.

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

### GraalVM
GraalVM is a new initiative developed by Oracle Labs that aims to provide a
high-performance polyglot language runtime. It consists of a universal virtual
machine and toolkit for seamless interoperability between programming languages.

Basing Luna's runtime and execution on GraalVM would reduce implementation
burden through use of the Truffle toolkit. This allows a single implementation
of language semantics and the language runtime, from which a high-performance
JIT and interpreter can be derived using partial-evaluation optimisation
strategies.

#### Feasibility
GraalVM is an innovative piece of software, allowing for easy language
development and interoperability between languages. However, for our use-case,
it is stymied by a lack of control over the JIT itself, and lack of true support
for functional paradigms that make it not an amazing fit for the development of
Luna's runtime.

#### Building a Runtime JIT using GraalVM
Rather than building GraalVM _into_ a JIT as part of Luna's runtime, use of the
Graal platform would instead constitute the entire Luna RTS.

The Truffle language development framework, included as part of Graal provides
the implementor with tools for creating a language interpreter and matching
runtime. The Graal compiler is then able to use a partial-evaluation approach
to derivie [high-performance code](https://chrisseaton.com/rubytruffle/pldi17-truffle/pldi17-truffle.pdf)
from the simple interpreter. In doing so, the language implementor is able to
provide only a _single_ implementation of the language's semantics and runtime,
rather than a traditional approach where interpreter and compiler are forced to
both implement the semantics themselves. These Truffle interpreters are aware of
the partial-evaluation optimisations that can be made, and hence make this
optimisation problem tractable.

In addition, GraalVM has rich support for both debugging and profiling. Users
are able to build their own introspection tools for the platform, and this
functionality would be a boon when it comes to the profiling requirements placed
on the Luna runtime.

However, Truffle (and hence Graal) are targeted primarily at imperative
languages, providing a less-than-ideal set of tools for the implementation of
functional languages. This includes a lack of support for both functional
paradigms and sophisticated type-system features, and so it may be difficult to
shoehorn Luna's semantics into it.

In addition, Luna's JIT has some specific requirements placed on it, especially
around both manual control over optimisation and value caching. These are,
unfortunately, not supported by Graal:

- Caching of values and traces is not something built into the Graal JIT. It is
  very likely that attempting to retrofit this would take significant work, and
  require the team to maintain a fork of Graal itself.
- While there is functionality for manually triggering deoptimisation of Graal
  code (`CompilerDirectives.transferToInterpreterAndInvalidate()`), there is no
  functionality for the converse. This means that if Graal is used there will be
  no way to manually trigger optimisation of a specific piece of Luna code.

#### GraalVM and Language Interop
Probably the _key_ advantage to using Graal would be the Polyglot features that
it provides. The [Polyglot SDK](http://www.graalvm.org/sdk/javadoc/org/graalvm/polyglot/package-summary.html)
provides functionality for zero-copy sharing of data between languages executing
on the same GraalVM instance. These languages include JavaScript, R and LLVM
bitcode, as well as an experimental implementation of Python 3.7. This Polyglot
API allows for native evaluation of foreign language code via the
`Polyglot.eval` command.

Polyglot Graal uses a Java-style datamodel and standardised interoperability
protocol that describes the functionality that each language needs to provide to
operate on common data. More details can be found in this [paper](https://chrisseaton.com/truffleruby/dls15-interop/dls15-interop.pdf).

#### GraalVM and Performance
Unlike standard tracing-based JITs or language compilers (see [PyPy](https://pypy.org/)),
GraalVM uses a partial-evaluation based approach to performance. This means that
it relies less on traditional imperative-language optimisations that would not
apply to Luna as a functional language. Some notes:

- Graal's optimisations are applicable to both dynamic and static languages.
  However, the latter will suffer from longer warmup times as there is less
  'semantic cruft' to prune through partial evaluation.
- Graal's startup time is guaranteed to be slower than a hand-tooled runtime for
  a language, but can offer comparable speed once warmed up.
- Graal has a well-specified machine and memory model, with support for
  automatic memory management, concurrency and synchronisation primitives.
- It is able to perform the following optimisations on high-level languages:
  - Complex cross-module inlining.
  - Aggressive speculative optimisation.
  - Escape analysis for elimintation of spurious object allocations.

The optimisation process for languages implemented using Truffle and running on
the Graal platform is as follows:

1. The interpreter collects information on the subset of the implemented
   language semantics that are _actually_ being used at runtime. This includes
   both profiling and type information, and is used to perform specialisation.
2. Partial evaluation is applied to this specialised interpreter to derive
   compiled code that is faster.
3. If speculation is too specific, it fails back to the interpreter and this
   information is used to alter the specialisation (usually by generalising it).
4. This process repeats until a threshold of sufficient generality is reached.

#### Negatives to using GraalVM
Much like GHC, GraalVM offers a compelling solution for implementation of Luna's
RTS. However, it is not without its negatives:

- GraalVM is currently only in preview release for Windows platforms, one of the
  core platforms for Luna. There is no timeframe for Windows stabilistation.
- While Graal has had its 1.0 release, it is still under heavy development and
  is resultantly quite buggy. Along the same lines, the Polyglot interop
  protocol is somewhat limited, and may not support all of the functionality
  required to operate seamlessly with Luna at a high level.
- GraalVM operates under two licenses. The community edition is free for use in
  production, but misses out on some of the performance of the enterprise
  edition.
- Working with Graal requires writing Java and the team has limited experience
  with the language.
- Unlike both LLVM and GHC-based approaches, using GraalVM offers no pathway to
  a native compilation target for Luna. While the platform includes SubstrateVM,
  a sophisticated static-tracing compiler that does code reachability analysis,
  this only works on _Java_ code. While we could thus provide a statically
  compiled binary of the Luna Runtime, there is no path to static Luna binaries
  themselves.

#### Resources
For more information about GraalVM and how it could be used for implementing the
RTS for Luna, see the following links:

- [GraalVM Website](https://www.graalvm.org/)
- [GraalVM Documentation](https://www.graalvm.org/docs/)
- [GraalVM API Documentation](http://www.graalvm.org/sdk/javadoc/)
- [Partial Evaluation and Truffle](https://chrisseaton.com/rubytruffle/pldi17-truffle/pldi17-truffle.pdf)
- [Language Interoperability](https://chrisseaton.com/truffleruby/dls15-interop/dls15-interop.pdf)

### LLVM
LLVM is a well-known and oft-used open-source compiler toolkit. It includes a
huge set of analysis and optimisation passes, as well as flexible tools for
building compilers. It supports JIT Compilation, accurate Garbage Collection,
and additional features for building a runtime such as coroutines.

While not specifically intended for functional language development, LLVM
provides a paradigm-agnostic intermediate representation (LLVM IR), which can
act as a target for any kind of language. There is significant tooling for
working with this representation, and in addition there are Haskell bindings to
the LLVM API for easy integration with existing Luna infrastructure.

#### Feasibility
The top-level summary of this analysis of LLVM is that, while it would involve
_more_ work to create Luna's JIT and Runtime based on LLVM, the end result will
allow us far more control and robustness than a solution built on top of GHC.

That doesn't, however, mean that it is the best solution. In comparison to LLVM,
using GHC would mean that there is a significant amount less that requires
implementation. This includes the runtime itself, with all the features that
entails, but also a whole host of functional optimisations based on types.

LLVM is provided a rich interface to Haskell via the [`llvm-hs`](https://github.com/llvm-hs/llvm-hs/)
bindings. These are a faithful representation of the LLVM API, with some Haskell
flavour for it to feel seamless. This is likely to result in a
highly-maintainable runtime that is easy to imrpove with time (e.g. the
[sixten backend sources](https://github.com/ollef/sixten/tree/master/src/Backend)
are a good example of this). This Haskell API is also designed to eliminate as
much of the LLVM boilerplate as possible.

Furthermore, LLVM is a widely-used and highly supported toolchain. This means
that there is significant assistance available should it be required, but also
that bugs are likely to be fixed rapidly. It also means that LLVM is likely to
implement rapid mitigations for security vulnerabilities such as [Spectre v1](https://llvm.org/docs/SpeculativeLoadHardening.html).
Even more useful, however, is the suite of sophisticated tooling that comes with
LLVM, much of which has no GHC equivalent.

#### An LLVM-Based JIT
LLVM has provided multiple APIs for just-in-time compilation during the course
of its development. While MCJIT was, until recently, the API du-jour, the new
ORC JIT API brings many improvements to how it can be used, including support
for true interactive JIT applications.

The big benefit of this is that we would only require a _single_ implementation
of Luna's semantics, not the dual implementations that using GHC would require.
For linking LLVM into the Luna binary, we get a seamless JIT experience as
follows:

1. Generate LLVM IR in memory.
2. Pass this LLVM IR to the ORC API.
3. Recieve back a function pointer which can be directly called.

This supports true on-request compilation, tracing dependencies and automatic
compilation of functions as needed.

The ORC API offers a flexible set of controls for JIT implementation. The user
is able to pass both functions and entire modules, and has fine-grained control
over the optimisation pass layers executed before the dynamic compilation step.
This means that Luna would be able to use both LLVM-standard optimisations and
custom-written passes.

Use of the LLVM API would provide support for many features required of the Luna
runtime, including support for [zero-cost exceptions](https://llvm.org/docs/ExceptionHandling.html),
[coroutines](https://llvm.org/docs/Coroutines.html), [DWARF debugging symbols](https://llvm.org/docs/SourceLevelDebugging.html),
and [garbage collector integration](https://llvm.org/docs/GarbageCollection.html).
While these aren't included with a runtime, they would likely ease the
implementation burden of Luna's runtime significantly should the LLVM route be
chosen.

#### LLVM and Optimisation
As an industrial-strength compiler toolchain, LLVM has significant support for
controlling performance. The toolchain provides extensive control over memory
layout, copying and stack usage; all things which the higher-level GHC core does
not. This means that LLVM IR is likely to provide a better route for giving our
users controlled performance.

This performance, however, would likely initially be lower than that provided by
GHC. This is by virtue of GHC core supporting a significant number of functional
language optimisations that LLVM does not. These optimisations would instead
need to be written as LLVM passes or performed directly as passes on the Luna
IR. Nevertheless, LLVM provides robust support for optimising IR:

- The LLVM toolchain builds optimisations as [IR Passes](https://llvm.org/docs/WritingAnLLVMPass.html),
  which are IR to IR transformations. This means that user-written passes are
  just as powerful as included passes; there is significant scope for improving
  code performance using these passes.
- LLVM has support for a huge number of existing [optimisation passes](https://llvm.org/docs/Passes.html).

These passes, however, can only do so much in cases where the generated IR is
pathologically bad. This means that Luna would have to take care to generate
[sensible IR](https://llvm.org/docs/Frontend/PerformanceTips.html) for the JIT,
especially as the initial JIT passes would be run without any except the most
basic optimisations (e.g. constant folding).

#### Foreign Languages and FFI
LLVM is a platform used by many modern programming languages, including the
`clang` C and C++ compiler, `rustc` the Rust compiler, and `julia` the Julia
compiler and interpreter. All of these languages compile to LLVM IR, which opens
up an opportunity for langauge interoperability.

The IR could be used for language interoperability, with the ability to mix
calling conventions seamlessly for IR-level FFI. The downside of this, however,
is that exposing a high-level interface between the languages, much like GraalVM
is able to, becomes a much more challenging task.

While it operates at a lower level, FFI calls to these languages would be
_extremely_ performant.

#### Native Compilation
Much like GHC, using LLVM as part of the Luna runtime would provide a simple
path to native compilation of Luna binaries. LLVM started development as a
framework for the creation of native compilers, providing LLVM IR as a
target-agnostic representation of language semantics.

As use ofthe LLVM ORC JIT involves the generation of LLVM IR from Luna code, the
same IR can be compiled AOT to native binaries instead of on-demand by the JIT.
Both paths are trivially supported by the LLVM API, and this could be a boon for
Luna.

#### Negatives to using LLVM
Much like all options in this comparison, using LLVM is not without its
downsides.

- LLVM does not come with its own native runtime, though it does have support
  for many features that the runtime would require. This results in a
  significantly increased implementation burden for Luna's maintainers as there
  is less existing functionality to piggyback on.
- Writing custom passes for LLVM must be done in C++ as it is not exposed by any
  bindings to the toolchain. They must be directly compiled into the binary, so
  if Luna chooses to implement custom passes we would be requried to maintain
  our own fork of LLVM.
- While LLVM IR is, like GHC Core, a statically- and explicitly-typed language
  representation, they operate at separate levels of sopistication. Core is much
  higher-level than LLVM IR, and thus provides a far more sophisticated type
  language. LLVM IR, however, is higher level than Cmm as it abstracts away
  details around hardware (e.g. register allocations).
- The limited type-language of LLVM IR is likely to limit many optimisations to
  Luna IR itself, while GHC core would be able to cope with these.
- LLVM's optimisations are targeted at imperative languages. While it would be
  possible to duplicate many of GHC's optimisations as LLVM passes, this would
  be a significant amount of work.
- Using LLVM would require re-implementation of the runtime support for Luna's
  standard library in C++ rather than Haskell.

#### Resources
For more information about LLVM and the ways it supports the design of Luna's
runtime, please see the following links:

- [LLVM Reference Manual](https://llvm.org/docs/index.html).
- [LLVM IR Reference](https://llvm.org/docs/LangRef.html)
- [Kaleidoscope Language Tutorial](https://llvm.org/docs/tutorial/LangImpl01.html)
- [Kaleidoscope JIT](https://llvm.org/docs/tutorial/BuildingAJIT1.html)
- [Sixten Backend](https://github.com/ollef/sixten/tree/master/src/Backend)
- [Haskell LLVM Library](https://github.com/llvm-hs/llvm-hs/)
- [Kaleidoscope Language in Haskell](http://www.stephendiehl.com/llvm/)
- [Packaging LLVM](https://llvm.org/docs/Packaging.html)

## Thinking about JITs
To get the kind of performance we need, and to support some of the more advanced
features of the Luna Runtime, the new design will be based around a JIT compiler
at its core. This section aims to elucidate and clarify some thoughts on the
design and development of an appropriate JIT compiler.

#### An Overview of JIT Architecture
The design of a JIT compiler is fundamentally based on the ability to hot-swap
compiled code for interpreted code in a seamless fashion. This can be performed
in many ways, but for Luna it will require compilation of IR to binary code in
memory, then prepared for direct execution _in process_.

The benefit of doing so is that the resultant execution is much faster than any
direct-interpretation based approach, but suffers the pentalty of a small lag
at startup.

A JIT takes arbitrary portions of code (e.g. functions, traces, modules), and
translates them to machine code to aid in performance. These artifacts are then
cached for later reuse. The choice of exactly _what_ gets optimised is one of
the key design decisions for a JIT. In Luna's case, it is likely that a hybrid
approach will be required, combining tracing, speculative compilation, method
JITing and sophisticated dependency analysis.

All JITs rely on deep knowledge of their target language's execution model and
the underlying language design. It is only through exploiting this that the best
performance can be obtained.

#### JIT Startup Performance
The main downside of a JIT compiler-based runtime is that you have to make a
careful tradeoff between startup time and initial performance. The more time
taken to optimise and precompute portions of the program, the longer it will
take for the runtime to start up.

A similar problem is the issue of warm-up time. This refers to how long the JIT
takes to achieve optimal performance for a given piece of code. In essence, the
more precomputation done, the shorter the warm-up time, but the longer the start
up time.

There is a minimal set of tasks that would need to take place before Luna's JIT
would be able to start:

1. Lexing and Parsing of Luna source code, coupled with generation of the Luna
   IR graph.
2. Typechecking of Luna IR and any reduction that may take place (see later).
3. Generation of GHC Core from Luna IR.
4. Translation of Core to GHCi Bytecode for initial interpretation.

While none of these steps are _inherently_ slow, as demonstrated by the JVM,
which can start up relatively quickly, these are still enough to create some
kind of a delay. The problem therefore becomes one of ensuring the above
processes take as little time as possible in order to ensure that Luna's runtime
is responsive.

##### Techniques for Minimising Startup Time
As mentioned above, there is a set of tasks that _must_ take place for the JIT
to startup. However, there are a number of techniques that can be utilised to
reduce this as much as possible:

- **Precompilation:** It is possible to precompile commonly-used functionality
  to an appropriate stage. The standard library, for example, could be
  distributed as bytecode so that it doesn't require compilation each time.
- **Parallelism:** The listed startup tasks should happen in parallel as much
  as possible. The biggest opportunity for this is likely the generation of GHC
  Core from the Luna IR, but parsing can also potentially be parallelised (but
  requires discovery to be done properly).

##### Techniques for Minimising Warm-Up Time
As a tradeoff with startup time, warm-up time can be decreased by performing
more work ahead of time. Luna has a unique advantage over a standard tracing
JIT in that the interactivity can provide hints as to what needs to be compiled
and optimised.

- **Dynamic Layering:** Precompilation of portions of code not in the active
  layer could be performed. This would provide increased performance, but there
  must be significant care taken to ensure that appropriate code is deoptimised
  when necessary (de-specialisation).
- **Optimisation without Tracing:** Code that is compiled in the background can
  have general optimisations done to it that can then be improved upon using the
  input from the tracing process later on.
- **Static Tracing:** The decisions on the order for background optimisation can
  be made via static analysis on the Luna IR graph. The code that is used
  'soonest' from the `main` function should be compiled and optimised first.

One of the primary issues with long warm-up times in JIT compilers is that they
become inappropriate for short-term execution, providing less-than-ideal
performance under such circumstances. There are, however, a few techniques that
can be employed to reduce the problem:

- **Project Caching:** As much of the work as possible done by the JIT should be
  cached in a `Package/.cache/IR` directory that should be default excluded from
  version control. This is separate from any precompiled IR that _should_ be
  distributed. Keeping these artifacts (e.g. Core, bytecode) ensures that, where
  things have not changed, the work does not need to be done again.
- **Precompilation:** Commonly used functionality (e.g. the stdlib) should be
  precompiled on the first execution, and even distributed as core or bytecode.

It should be noted that tracing becomes less useful in such a setting, but as
many Luna sessions take place in an interactive environment (e.g. Luna Studio or
the forthcoming REPL), the ability to generate and optimise traces will be very
important.

#### Optimisation Opportunities in JITs
The key idea behind JIT optimisations is that runtime profiling information can
be taken advantage of to provide performance beyond that of a standard AOT
compilation strategy. The optimisations can rely on information about the exact
architecture on which the code is executing, and can exploit this for better
performance. This permits optimisations such as:

- **Full-System Optimisations:** The JIT is capable of performing system-level
  optimisations (e.g. aggressive inlining) without needing to do so
  speculatively. This allows elision of the runtime checks that an AOT compiler
  would have to insert, thereby increasing performance.
- JITs are also able to rearrange code for better cache behaviour. This should
  be possible even in Luna's case, using tracing to optimise the code to be
  executed in memory.

All JITs are capable of performing a fairly standard set of optimisations, but
these (inlining, constant folding, CSE, escape analysis) can be improved upon by
exploiting language-specific information.

##### Tracing for Luna
The functional nature of Luna, enhanced by the visual syntax, results in a
language that is highly amenable to a tracing-based optimisation approach. The
way that tracing would likely operate for Luna is as follows:

1. Profiling information is collected during execution. This is traditionally
   for loops (or recursive calls), but can be augmented to compute hot paths and
   other useful information.
2. Once a code path is considered 'hot', the JIT records an execution trace of
   the exact instructions executed, incvluding functions for inlining. This
   trace is often stored as IR, but Luna can do better by annotating the IR
   graph.
3. The resultant trace consists of one execution path, which can be optimised
   easily. Guard instructions are inserted as appropriate into the trace to
   ensure that the assumptions made during collection still hold.
4. The trace is optimised, including CSE, dead-code elimination, escape
   analysis, heavy inlining and constant folding.
5. The compiled trace is executed until a guard fails, forcing deoptimisation.

This process can be improved upon by exploitation of language specifics. Luna's
advanced structural type system is capable of encoding many guarantees in the
types. As a result, a significant number of guards may be able to be elided.
Furthermore, alterations to type information are already intended to trigger
both deoptimisation and cache eviction, furtehr eliminating the need for many
guard instructions.

As a functional language, Luna's JIT would be able to start execution traces at
function calls, recursive loops and FFI boundaries, exploiting the Luna IR graph
to collect and store hyperblock identification information. The region selection
algorithm for this is always very language specific, and while hints can be
obtained from codebases such as LuaJIT, it will inherently be highly coupled to
Luna's execution semantics.

Effective tracing inherently requires the collection of profiling information in
order to decide when to collect a trace. Furthermore, Luna's forthcoming ability
to provide visual performance analysis of code requires the same. This means
that the profiling information collection should introduce as little overhead
as possible.

It is likely that augmenting the Luna IR graph with trace information will be
the most effective and performant way to do this. Nevertheless, the collection
of statistics needs to be minimally intrusive, so it is suggested that using
CPU performance counters can be sensible.

##### Adaptive Optimisations in Luna
The dynamic nature of Luna means that tracing is not the _only_ technique that
should be employed in the Luna JIT. Adaptive optimisation is the process of
using runtime information to dynamically recompile code with appropriate
optimisations. This is not the same technique as tracing, as it does not collect
execution traces, instead relying on call counts (and similar) for functions.

Luna could potentially exploit this using the following techniques:

- **Optimisation Layers:** Where code is executed frequently, it could be run
  through increasingly aggressive optimisation layers to provide better
  performance.
- **Tailored Passes:** Based on the kinds of execution that functions are seeing
  it is possible to select sets of Luna IR and GHC Core optimisation passes to
  best improve that function's performance.

##### Manual Optimisation Control in JITs
There is very little literature available that deals with exposing manual
controls for function optimisation and deoptimisation in JITs. Most JIT
architectures are based on automated processes, so the interaction between the
manual and automatic is not very well covered.

Luna's usage model means that there is information not encoded by the types or
runtime about which functions can be optimised heavily. It is those not being
directly edited by the user that become candidates for increased optimisation,
and hence the compiler needs to expose calls to hint (on the IR graph) about
such usage to the JIT.

#### Building the Luna JIT
The _exact_ architecture for Luna's JIT compiler is still somewhat up in the
air, even though it seems fairly certain that it will be based on GHC. The main
question that is still open is whether building a JIT will require just one
(core generation) or two (core generation and direct interpretation)
implementations of the language semantics.

The main sticking point in discussions so far is that typechecking and
optimising the Luna IR requires compile-time code execution. The simplest way
to do this woul dbe to create an interpreter that directly executes Luna IR, but
this is inherently going to be the slowest possible design. While it is likely
that gains over the existing interpreter can be realised, direcct operation on
a rich format like the graph will never be as quick as operating on bytecode.

The second, and more attractive option is to perform code generation and
compilation to bytecode during typechecking and optimisation. This would require
just _one_ implementation of the language semantics, increasing maintainability.
The typechecker and optimiser would be given access to the JIT, allowing them to
execute Luna code where necessary. The result could then be used to update the
IR graph directly.

#### Security with JIT Compilers
While JIT compilers can provide excellent performance without the overhead of
precompilation, they are not a panacea and come with their own issues.

The predominant issue is a _security_ one, as JIT compilers are those that
fundamentally rely on executable data. Any vulnerability in the JIT can hence
have the potential for a heap-pollution attack. Furthermore, dynamic reflection
capabilities that are often used to support runtime inspection and debugging,
such functionality can further increase the attack surface if not designed in a
sensible fashion.

In order to help reduce this attack surface, care must be taken to implement the
W/X mitigation where code is only executable using the following process:

1. Code is written to memory.
2. It is marked as read-only.
3. It is marked executable.

These must be atomic and discrete steps in order to ensure that the attack
surface is reduced.

#### Resources
The following are useful resources when thinking about building JIT compilers.

- [LuaJIT Source Code](https://github.com/LuaDist/luajit)
- [Are JITs Winning?](http://lambda-the-ultimate.org/node/3851)

