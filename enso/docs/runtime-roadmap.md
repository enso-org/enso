---
layout: developer-doc
title: High-Level Runtime Roadmap
category: summary
tags: [contributing]
order: 6
---

# High-Level Runtime Roadmap

This roadmap consists of longer, open-ended tasks that are required to make Enso
better in the long term. The tasks here are not in any order that indicates
priority, but the dependencies between tasks are described.

## Technology Choices

With the advent of Java 17 and its ergonomic improvements (read:
pattern-matching), it makes little sense to retain the usage of Scala throughout
the compiler. The language was originally introduced due to the capabilities of
its type system in comparison to Java's, but very little of this functionality
has been used in the end.

We recommend moving everything to Java as part of this work, as you will end up
with better tooling support. Scala has been a problem child.

Enso originally started working with Java 8, and was transitioned (painfully,
due to the JPMS) to Java 11. Java 8 was EOL'd by the graal team after a couple
of years. It seems likely that Java 11 will suffer a similar fate, though the
transition from 11 to 17 will be far less painful as it doesn't introduce any
breaking language-level changes.

## Static Analysis

Enso is a fairly dynamic language, but this doesn't mean that it doesn't admit
static analysis. There are a number of areas that can be made better (read: more
intuitive, more performant, and so on). These, again, are not in order of
priority, but where there are dependencies these are indicated.

### Purpose-Built IR

The current compiler IR is a bit of a mess. Due to time constraints, we ended up
moving on with it though it was firmly unsuited to the direction we wanted to
evolve the compiler. While many of the features listed below are _possible_ in
the current IR, they are difficult and inelegant compared to doing them on an IR
suited to the task.

Currently, the IR is:

- Very verbose and difficult to add a new node to. Adding a new node requires
  adding ~100 lines of code that could likely be automated away. Lots of
  boilerplate.
- Of unknown performance.
- Partially mutable, making it confusing as to which things are shared.

A new IR for Enso would have to:

- Be able to be serialized to disk (the current one can).
- Remove the verbosity and repetition when adding new nodes.
- Be built with performance and easy traversal in mind.

While it is a daunting task to wholesale move the entire compiler to a new IR,
it can instead be done in an incremental fashion. First, it makes sense to
design and build the new IR, and then write a translation from the current IR to
the new IR. With that done, the boundary between the two in the compiler can be
gradually shuffled, starting with codegen (`IrToTruffle`), until no usages of
the old IR remain.

If it were up to us, we'd _consider_ basing the new IR on a mutable graph as
this easily admits many common compiler operations, and also is likely to reduce
memory usage of the compiler overall. Care should be taken with introducing
mutability, however. While the current IR is mutable in limited ways (primarily
the metadata on nodes), a fully mutable IR will have to have comprehensive
utilities for deep copying and dealing with cycles. That said, Marcin thinks
that it _may_ be worthwhile to stick to an immutable structure.

These two approaches offer a trade-off in terms of what they make easy. While
it's very easy to reason about tree-like structures (within a module), it makes
certain operations (e.g. Alias Analysis) more painful than they would otherwise
be (we had to make a graph on top of the tree to get this working).

_Unreliably_, we can guestimate at:

- A tree with less verbosity and fixing some niggles would be approximately a
  month to implement the new IR and migrate the compiler and passes.
- A more novel graph-based IR would be more complex to implement (a couple of
  months perhaps), but also to migrate the passes due to the change in
  underlying principles. While it would make certain passes (e.g. dataflow
  analysis, alias analysis) easier to maintain and understand, the underlying
  principle still changes.

### Improving Static Analysis Capabilities

Though we're not suggesting moving to a fully-type-checked language any time
soon, the current system doesn't make use of most of the information contained
in the type signatures. This should involve:

- Processing and resolving the existing type signatures for use in the compiler.
  We want to use them to provide accurate and helpful suggestions in the IDE.
  The type signatures are currently ignored by the compiler. They are only kept
  in their original almost-AST form. They are currently used primarily for
  documentation, though can also be used to indicate lazy arguments, and perform
  some role in automated parallelism analysis.
- Performing forward-only type propagation. This is a big win for comparatively
  low effort: if we have the standard library liberally type-hinted,
  forward-only propagation for workflows in the IDE means that you can have type
  information for a majority of the program without having to implement
  backwards inference rules for Enso (which are very complex). This win is for
  user programs, and _requires_ type hinting of libraries to work well.
- Using that information to perform certain optimisations (see
  [below](#static-optimization-passes)).

While you do not need to [update the IR](#purpose-built-ir) to do this analysis
and subsequent optimisation, it would certainly make many of them easier. If you
are writing more passes on top of the old IR, it's just piling on technical
debt. Please be aware of this.

### Static Optimization Passes

With improved
[static analysis capabilities](#improving-static-analysis-capabilities), we gain
the ability to do lots more optimisations statically.

#### Scope Flattening

There are multiple points in the language where we create new scopes where this
isn't strictly necessary. Eliminating these extra scopes eliminates the need for
allocations and dynamic calls.

- Many types of pattern matches can, instead of treating each branch as a
  lambda, flatten these into an almost JS-style `if-then-else`. Rather than
  inserting a function call for each branch, we can hoist (with renaming)
  variables into the same scope. This means we don't need to perform a function
  call or allocate a new scope.
- With type inference, there are many cases where a lazy argument doesn't need
  to be made lazy (currently they are evaluated in a separate scope). This would
  improve performance significantly. In our opinion, this is the biggest
  performance pitfall of the language implementation.

For simple programs, GraalVM can usually optimise these additional scopes away.
However, doing this flattening process removes the need to optimise these things
and may actually admit more optimisations (claim unverified). This means that we
think Graal will spend more time optimising the parts of the programs that
matter.

#### Pattern Match Optimisation

Currently we don't perform any optimisation when desugaring nested pattern
matches. This means that the IR (and resultant generated truffle code) is far
larger than it needs to be.

- Deduplicating and flattening case expressions will bring a large win in terms
  of memory usage.
- This will likely also improve performance as less `if` branches need to occur
  to resolve the actual target function of the pattern match.
- It may be useful to look at dotty's implementation of pattern match desugaring
  and optimisation as Ara finds it very readable.

#### Liveness Analysis

Currently Enso keeps every variable alive for as long as it's in scope. This
means that we have two major pitfalls:

1. We retain large data for far longer than is necessary (until the end of the
   enclosing scope, rather than until the last usage), ballooning the language's
   memory usage.
2. We accidentally capture these bloated scopes when creating closures, further
   retaining unnecessary data for the lifetime of the closure.

While we originally proposed to perform scope pruning when capturing variables
in closures, a far more sensible approach is to perform liveness analysis:

- Use the information to free variables as soon as they are no longer used. Look
  into the Truffle APIs
  ([`Frame#clear`](https://www.graalvm.org/truffle/javadoc/com/oracle/truffle/api/frame/Frame.html#clear-com.oracle.truffle.api.frame.FrameSlot-))
  for informing GraalVM about this for increased performance in compiled code.
- This will allow them to be garbage collected when not needed.
- Furthermore, this will also mean that extraneous values are not captured in
  closures and further kept alive.
- This process needs to account for the fact that `Debug.breakpoint`,
  `Debug.eval` may be used in this code. Under such circumstances, all in-scope
  variables should be retained for the duration of the call.

Note that scope pruning could still be a win in rarer circumstances, but is not
needed for the majority of improvement here.

#### Devirtualisation

There are multiple features in Enso that generate dynamic calls that do not
always need to (e.g. when the concrete type of an atom is known at compile time,
its accessors can be inlined, or when the types of `a` is known in `a + b` are
known, we can devirtualise the `+` implementation that specializes based on the
type of `b`. If we know the type of `b` we can do even better and compile the
specific add implementation). In conjunction with the
[better static analysis](#improving-static-analysis-capabilities) it should
become possible to devirtualise multiple types of calls statically, and allow
you to inline the generated code instead.

- The cheapest way to do this is to retain the call, but make the call static
  with pre-sorted arguments. This behaves nicely in the IDE.
- The more expensive way to do this is with deep analysis in the compiler and
  direct inlining of method bodies wherever they match a heuristic. This would
  have to only occur _outside_ introspected scopes, as it does not behave well
  with the IDE (without specific handling, at least).

We recommend a combination of the two, using the latter for non-introspected
scopes, and the former for scopes being observed by the IDE. That said, if the
first brings enough of a win, there may be little point to the second.

## Language Semantics

While Enso is fairly semantically complete, there are still a number of things
that have proven awkward to work with.

### Shadow Definitions

Enso has a concept of _extension methods_. These are methods that are _not_
defined "alongside" the type (in the same compilation unit). Currently, we have
no way to define methods that are _not_ extensions on builtin types without
defining them in Java. This is awkward, and leads to a poor experience for both
developers of Enso, and the users (where there is a special case rule rule for
certain types, and also a hacky form of documentation for these same types).

For types defined in Java, their methods defined in Enso are extensions and are
hence not available without importing `Base`. Currently if I have a `Text` and
don't have `Base` imported, I can't call `split` on it as it's an extension.

This is particularly important for polyglot, as polyglot calls are not handed
extension methods. Polyglot calls only have access to the methods defined on the
type.

To rectify this situation, we recommend implementing a system we have termed
"shadow definitions":

- Instead of creating builtins into their own module, provide an
  annotation-based system for linking definitions in Enso source code to
  built-in implementations and types in the compiler.
- For builtin types, the compiler should be informed that the type for a builtin
  is actually _defined_ in a source file, despite being implemented elsewhere.
- You can see the existing design for this annotation-based system in
  [`Builtins.enso`](https://github.com/enso-org/enso/tree/main/engine/runtime/src/main/resources/Builtins.enso).
- Implementing this has a knock-on effect on what can be done later. For
  example, `Vector` and `Time` are currently defined in `Base`, and are
  therefore not (Truffle) interop friendly. With this system, we could implement
  these types in such a way that they can be handled properly in interop, making
  it much more seamless to use them with other truffle languages.
- Doing this will also improve the situation around the Builtins IR. Currently
  it is not really a library as it exists purely for documentation purposes.
  This means that it doesn't have a library location into which we can
  precompile the builtins before distribution (so right now it gets compiled on
  user machines in the first run).

With this done, it may still be necessary to create a Java DSL for implementing
built-in methods and types, but that is unclear at this point.

### Static Methods on Types

Currently, Enso allows calling methods on _modules_, _constructors_, and
_instances_. This does not conform to the language specification because it
allows constructors and instances to be treated the same at runtime. This leads
to odd results (see the ticket below).

The end result should be compliant with the design described
[here](https://github.com/enso-org/enso/issues/1851), and needs to be taken into
account when defining builtins.

### Better Safepointing

Enso currently uses a hand-rolled safepointing system for interrupting threads
and handling resource finalisation. With 21.1, Truffle landed its own system for
doing this. Enso should be updated to use
[the new system](https://github.com/oracle/graal/blob/master/truffle/docs/Safepoints.md),
instead, as it will provide better performance and more robust operation.

## Runtime Performance

While Enso is performant when it gets JITted by GraalVM, the performance when
running in purely interpreted mode is poor. That said, there are still
performance improvements that can be made that will benefit compiled code as
well.

### Interpreter Performance

This can be greatly improved.

- Start by measuring the performance with compilation disabled (no graal, only
  Java code running).
- Analyse the performance for bottlenecks in the interpreted code and fix the
  problems. See the brief guide to Graal document.
- Keeping methods in `HashMap` and similar implementation decisions can easily
  be improved.
- Many of the above-listed static optimisations will greatly help here.

### Unboxed Atoms

Currently every atom in Enso is stored boxed. In limited circumstances it may be
possible to unbox these and hence remove the indirection cost when accessing
their data.

- Read the details of Truffle's
  [`DynamicObject`](https://www.graalvm.org/truffle/javadoc/com/oracle/truffle/api/object/DynamicObject.html),
  and the sources.
- Use this system to inform the design for a system that reduces the overhead of
  dynamic field names and arities when accessing data on Atoms.

### Unboxed Vectors

Enso currently doesn't have support for unboxed arrays (and hence vectors). This
means that it incurs a significant performance cost when working with pure
numerical arrays. This can be improved.

- Read the truffle documentation on
  [truffle libraries](https://github.com/oracle/graal/blob/master/truffle/docs/TruffleLibraries.md).
- Based on this, define a system that seamlessly specializes and deoptimises
  between boxed and unboxed arrays as necessary.

## IDE

As Enso's primary mode of use is in the IDE, there are a number of important
improvements to the runtime and compiler that will greatly improve the user
experience there.

### Caching and User-Defined Types

Currently it is virtually impossible to define types for users in the IDE. This
is due to a semantic issue with the IDE's value cache. When defining a type and
creating an instance of it, the value of that instance is cached. When later
defining a method on it, the cached value is retained with the _old_ scope.

- Improve the heuristics for cache eviction in the IDE.
- Where no other strategy is possible, fall back to evicting the entire cache.

See [#1662](https://github.com/enso-org/enso/issues/1662) for more details and
options.

### Dynamic Caches

Currently, the IDE cache is fairly _dumb_, maintaining soft references to as
many in-scope values as possible. When memory runs out, the _entire_ cache gets
evicted, which is costly.

- Implement more sophisticated profiling information that can track allocations,
  LRU, and so on.
- Improve the cache eviction behaviour based on this.
- Ensure that, no matter what, the runtime should not go out of memory due to
  the cache.
- These eviction strategies should account for changes such as those described
  [above](#caching-and-user-defined-types)

### Lazy Visualization Support

Currently, IDE visualisations are evaluated eagerly on their candidate data.
This is a nightmare when working with huge amounts of data (e.g. tables with
millions of rows), and can easily lock up both the runtime and IDE. The current
solution artificially limits the amount of data sent to the IDE.

In the future, we want to support the ability to cache inside visualisation code
such that the preprocessor doesn't have to be recomputed every time the IDE
changes the parameters. This will enable the ability to view the full data in
the IDE without having to send it all at once, or recompute potentially costly
preprocessors.

- Implement caching support for the visualisation expression processing.
- This cache should, much like the IDE's introspection cache, track and save the
  values of all top-level bindings in the visualisation preprocessor.

## Parser

Parser
