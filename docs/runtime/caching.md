---
layout: developer-doc
title: Caching
category: runtime
tags: [runtime, caching, execution]
order: 1
---

# Caching

It is not uncommon for users in data-analysis jobs to work with data on the
order of _gigabytes_ or even _terabytes_. As fast as computers have become, and
as efficient as programming languages can be, you still don't want to compute on
such large amounts of data unless you absolutely have to.

This wouldn't usually be an issue, with such data-analysis tasks being able to
run in a 'batch mode', where the user starts their job in a fire-and-forget
fashion. Enso, however, is a highly _interactive_ environment for working with
data, where waiting _seconds_, let alone _hours_, would severely hamper the user
experience.

Given that Enso is a highly interactive language and platform, we want to take
every measure to ensure that we provide a highly responsive experience to our
users. To that end, one of the key tenets of the new runtime's featureset for
aiding in this is the inclusion of a _caching_ mechanism.

Caching, in this case, refers to the runtime's ability to 'remember' the values
computed in the currently observed scopes. In combination with the data
dependency analysis performed by the compiler, this allows the runtime to
recompute the _minimal_ set of expressions when the user makes a change, rather
than having to recompute the entire program.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Cache Candidates](#cache-candidates)
  - [Initial Cache Candidates](#initial-cache-candidates)
  - [Further Development of Cache Candidates](#further-development-of-cache-candidates)
- [Partial-Evaluation and Side-Effects](#partial-evaluation-and-side-effects)
  - [Side Effects in the Initial Version](#side-effects-in-the-initial-version)
  - [In The Future](#in-the-future)
- [Cache Eviction Strategy](#cache-eviction-strategy)
  - [Initial Eviction Strategy](#initial-eviction-strategy)
  - [Future Eviction Strategies](#future-eviction-strategies)
- [Dataflow Analysis](#dataflow-analysis)
  - [Identifying Expressions](#identifying-expressions)
  - [Specifying Dataflow](#specifying-dataflow)
- [Cache Backend](#cache-backend)
  - [Initial Implementation of Cache Backend](#initial-implementation-of-cache-backend)
  - [Further Development of Cache Backend](#further-development-of-cache-backend)
- [Memory Bounded Caches](#memory-bounded-caches)
  - [Soft References](#soft-references)
  - [Serialization](#serialization)
  - [Instrumentation](#instrumentation)
  - [Manual Memory Management](#manual-memory-management)
  - [Comparison of Memory Management Approaches](#comparison-of-memory-management-approaches)

<!-- /MarkdownTOC -->

## Cache Candidates

The key use of the Enso value cache is to support the interactive editing of
user code. This means that it caches all bindings within the scope of a given
function, including the function arguments. This means that, as users edit their
code, we can ensure that the minimal amount of their program is recomputed.

Consider the following example:

```ruby
foo a b =
    c = a.frob b
    d = c.wibble b
    a.quux d
```

The cache is active for the _currently visible scope_ in Enso Studio, so when a
user enters the function `foo`, the cache stores the intermediate results in
this function (in this case `c` and `d`), as well as the inputs to the function
(in this case `a`, and `b`).

All intermediate results and inputs are considered as candidates, though as the
cache design evolves, the _selected_ candidates may be refined. Ultimately we
want to cache and reuse as much as possible to minimize the computation costs.
At the same time, we want to limit the memory consumed by the cache.

### Initial Cache Candidates

The initial version of the cache only stores the right-hand-sides of binding
expressions. This is for two main reasons:

- Firstly, this allows us to ensure that the cache does not cause the JVM to go
  out of memory between executions, allowing us to avoid the implementation of a
  [memory-bounded cache](#memory-management) for now.
- It also simplifies the initial implementation of weighting program components.

### Further Development of Cache Candidates

The next step for the cache is to expand the portions of the introspected scope
that we cache. In general, this means the caching of intermediate expressions.

However, once we do this, we can no longer guarantee that we do not push the JVM
out of memory between two program executions. This is best demonstrated by
example.

```
a = (computeHugeObject b).size
```

Here we compute a value that takes up a significant amount of memory, but from
it we only compute a small derived value (its size). Hence, if we want to cache
the intermediate result of the discarded `computeHugeObject b` expression, we
need some way of tracking the sizes of individual cache entries.

## Partial-Evaluation and Side-Effects

The more theoretically-minded people among those reading this document may
instantly realise that there is a _problem_ with this approach. In the presence
of caching, it becomes _entirely_ unpredictable as to when side effects are
executed. This is problematic in that side-effecting computations are rarely
idempotent, and problems might be caused by executing them over and over again.

Furthermore, the nature of the interpreter's support for entering functions
inherently requires that it recompute portions of that function in a different
context, thereby potentially re-evaluating side-effecting computations as well.

In general, it is clear that many kinds of side effect have _problems_ in the
presence of caching and partial-evaluation.

### Side Effects in the Initial Version

Many of the mechanisms required to deal with this kind of issue properly are
complex and require deep type-level support in the compiler. To that end, the
initial version of the interpreter is going to pretend that the problem doesn't
really exist.

- All intermediate values will be cached.
- Cached values will be recomputed as necessary as described in the section on
  [initial eviction strategies](#initial-eviction-strategies).

This can and _will_ recompute side-effecting computations indiscriminately, but
we cannot initially do much better.

#### A Stopgap

While the compiler won't have the machinery in place to properly track
information about side-effects, we can implement a stop-gap solution that at
least allows the GUI to allow users to make the decision about whether or not to
recompute a side-effecting value. This is very similar to the initial approach
used for functions with arguments marked as `Suspended`, and works as follows:

- We provide explicit signatures (containing `IO`) for functions that perform
  side-effects.
- Whenever the runtime wants to recompute a value that performs side-effects, it
  can use this information to ask for user input.
- We can also display a box on these types `always_reevaluate` that lets users
  opt in to automatic re-evaluation of these values.

### In The Future

As the compiler evolves, however, we can do better than this. In particular, we
can employ type-system information to determine which functions are
side-effecting (in absence of annotations), and to class some kinds of said
functions as safe for either caching, re-evaluation, or both. What follows is a
brief sketch of how this might work:

- Rather than having a single type capturing side effects (like `IO` in Haskell)
  we divide the type up into fine-grained descriptions of side-effects that let
  us better describe the particular behaviours of given functions (e.g.
  `IO.Read`, `IO.Write`), all of which are more-specific versions of the base
  `IO` type.
- We provide a set of interfaces that determine whether a given kind of side
  effect can be safely cached or re-evaluated (e.g. `No_Cache` or
  `No_Reevaluate`).
- We can use this information to ask the user about recomputation in far less
  situations.

> The actionables for this section are:
>
> - Evolve the strategy for handling side effects as the compiler provides more
>   capabilities that will be useful in doing so.

## Cache Eviction Strategy

The cache eviction strategy refers to the method by which we determine which
entries in the cache are invalidated (if any) after a given change to the code.

### Initial Eviction Strategy

In the initial version of the caching mechanism, the eviction strategies are
intended to be fairly simplistic and conservative to ensure correctness.

- The compiler performs data-dependency analysis between expressions.
- If an expression is changed, all cached values for expressions that depend on
  it are evicted from the cache.
- Expressions that have been evicted from the cache subsequently have to be
  recomputed by the runtime.

The following rules are applied when an expression identified by some key `k` is
changed:

1. All expressions that depend on the result of `k` are evicted from the cache.
2. If `k` is a dynamic symbol, all expressions that depend on _any instance_ of
   the dynamic symbol are evicted from the cache.

### Future Eviction Strategies

In the future, however, the increasing sophistication of the front-end compiler
for Enso will allow us to do better than this by accounting for more granular
information in the eviction decisions.

Cache eviction takes into account the following aspects:

- **Visualization:** In the first place, we should care about nodes that have
  visualization attached in the IDE.
- **Priority:** The runtime can assign a score to a node, meaning how valuable
  this node is. Less valuable nodes should be evicted first.
- **Computation time:** The runtime can calculate the time that node took to
  compute. Less computationally intensive nodes should be evicted first. Memory
  limit. The cache should not exceed the specified memory limit.

> The actionables for this section are:
>
> - Evolve the cache eviction strategy by employing more granular information as
>   the compiler evolves to provide it.

## Dataflow Analysis

Dataflow analysis is the process by which the compiler discovers the
relationships between program expressions. The output of the process is a data
dependency graph that can be queried for an expression, and returns the set of
all expressions that depended on that expression.

Internally we represent this as a directed graph:

- An edge from `a` to `b` indicates that the expression `a` is depended on by
  the expression `b`.
- These dependencies are _direct_ dependencies on `a`.
- We reconstruct transitive dependencies from this graph.

An expression `a` can be any Enso expression, including definitions of dynamic
symbols. Given that dynamic symbols need not be in scope, care has to be taken
with registering them properly.

Each expression in the compiler IR is annotated with both the set of expressions
that depend on it, and the set of expressions that it depends on.

### Identifying Expressions

Expressions are identified, for the purposes of dataflow analysis, by unique
identifiers on every IR node. The dataflow analysis process creates a dependency
graph between these identifiers.

However, at runtime, the IDE uses a different and separate concept of
identifiers. Translating between these external identifiers and the internal
identifiers is left to the runtime and is not the responsibility of the dataflow
analysis pass.

### Specifying Dataflow

Dataflow analysis takes place on the core set of language constructs, defined as
those that extend `IRKind.Primitive`. Their dataflow is specified as follows,
with arrows representing edges in the graph.

#### Atom

An atom is dependent on the definitions of its arguments, particularly with
regard to any defaults.

```
atom <- arguments
```

#### Method

A method is dependent on the definition of its body. Methods at the point that
dataflow analysis runs are 'bare' methods, meaning that they are defined as
functions.

```
method <- body
```

#### Block

The value of a block is dependent purely on the value of its return expression.
The return expression may depend on other values.

```
block <- returnValue
```

#### Binding

The value of a binding is dependent both on the name of the binding and the
expression being assigned in the binding.

```
binding <- name
binding <- expression
```

#### Lambda

The value of a lambda is dependent on the return value from its body, as well as
the definitions of any defaults for its arguments.

```
lambda <- body
lambda <- argumentDefaults
```

#### Definition Argument

The value of a function definition argument is dependent purely on the value of
its default, if that default is present.

```
defArgument <- defaultExpression
```

#### Prefix Application

The value of a prefix application is dependent on the values of both the
function expression being called, and the arguments.

```
prefix <- function
prefix <- arguments
```

#### Call Argument

The value of a call argument is dependent both on the value that it's wrapping,
as well as the name it has, if it exists.

```
callArgument <- argumentValue
callArgument <- argumentName
```

#### Forced Term

A forced term is purely dependent on the value of the term being forced (the
`target`).

```
force <- target
```

#### Typeset Members

A typeset member is dependent on the definition of its label, as well as the
possibly present definitions of its type and value.

```
typesetMember <- label
typesetMember <- memberType
typesetMember <- memberValue
```

#### Typing Operators

All typing operators in Enso (`Type`) are dependent on their constituent parts:

```
typingExpr <- expressionChildren
```

#### Name

An occurrence of a name is dependent on the definition site of that name. This
means that it is broken down into two options:

1.  **Static Dependency:** The definition site for a given usage can be
    statically resolved.

    ```
    name <- staticUseSite
    ```

2.  **Dynamic Dependency:** The definition site for a given usage can only be
    determined to be a symbol resolved dynamically.

    ```
    name <- dynamicSymbol
    ```

    Under these circumstances, if any definition for `dynamicSymbol` changes,
    then _all_ usages of that symbol must be invalidated, whether or not they
    used the changed definition in particular.

#### Case Expressions

The value of a case expression depends on the value of its scrutinee, as well as
the definitions of its branches.

```
case <- scrutinee
case <- branches
case <- fallback
```

#### Case Branches

The value of a case branch depends on both the pattern expression and the result
expression.

```
caseBranch <- casePattern
caseBranch <- expression
```

#### Comments

The value of a comment is purely dependent on the value of the commented entity.

```
comment <- commented
```

## Cache Backend

The cache is implemented as key-value storage with an eviction function.

The cache stores the right-hand side expressions of the bindings in the
key-value storage. The storage can be as simple as a Hash Map with values
wrapped into the Soft References as a fallback strategy of clearing the cache.
The eviction function purges invalidated expressions from previous program
execution.

### Further development

Cache intermediate results of expressions to reduce the cost of new computations
and extend the eviction strategy to clear the cache based on memory consumption.

Extend the eviction strategy by adding an asynchronous scoring task that
computes some properties of stored objects (e.g., the size of the object). Those
properties can be used in the eviction strategy as optional clues, improving the
hit ratio.

> The actionables for this section are:
>
> - Evolve the cache by storing the results of intermediate expressions
>
> - Evolve the cache eviction strategy implementation by employing more
>   information of the stored values

## Memory Bounded Caches

Memory management refers to a way of controlling the size of the cache, avoiding
the Out Of Memory errors.

The methods below can be divided into two approaches.

1. Limiting the overall JVM memory and relying on garbage collection.
2. Calculating the object's size and using it in the eviction strategy.

In general, to control the memory size on JVM, anything besides limiting the
total amount involves some tradeoffs.

### Soft References

Soft References is a way to mark the cache entries available for garbage
collection whenever JVM runs a GC. In practice, it can cause long GC times and
reduced overall performance. This strategy is generally considered as a last
resort measure.

The effect of the GC can be mitigated by using the _isolates_ (JSR121
implemented in GraalVM). One can think of an _isolate_ as a lightweight JVM,
running in a thread with their own heap, memory limit, and garbage collection.

The problem is that _isolates_ can't share objects. And even if we move the
cache to the separate _isolate_, that would require creating a mechanism of
sharing objects based on pointers, which requires implementing serialization. On
the other hand, serialization itself can provide the size of the object, which
is enough to implement the eviction policy, even without running the _isolates_.

### Serialization

One approach to get the size of the value stored in the cache is
_serialization_. The downside is the computational overhead of transforming the
object into a byte sequence.

### Instrumentation

Another way of getting the object's size is to use JVM instrumentation. This
approach requires running JVM with _javaagent_ attached, which can complicate
the deployment and have indirect performance penalties.

### Manual Memory Management

This method implies tracking the size of the values by hand. It can be done for
the values of Enso language, knowing the size of its primitive types. For
interacting with other languages, Enso can provide an interface that developers
should implement to have a better experience with the cache.

### Comparison of Memory Management Approaches

Below are some key takeaways after experimenting with the _instrumentation_ and
_serialization_ approaches.

#### Enso Runtime Benchmark

The existing runtime benchmark was executed with the java agent attached to
measure the impact of instrumentation.

#### Serialization Benchmark

[FST](https://github.com/RuedigerMoeller/fast-serialization) library was used
for the serialization benchmark. It doesn't require an explicit scheme and
relies on Java `Serializable` interface.

#### Instrumentation Benchmark

Java
[`Instrumentation#getObjectSize()](https://docs.oracle.com/javase/8/docs/api/java/lang/instrument/Instrumentation.html)
can only provide a _shallow_ memory of the object. It does not follow the
references and only takes into account public fields containing primitive types.

Benchmark used the `MemoryUtil#deepMemoryUsageOf` function of
[Classmexer](https://www.javamex.com/classmexer/) library. It utilizes Java
reflection to follow the references and access the private fields of the object.

#### Benchmark Results

Benchmarks measured Java array, `java.util.LinkedList`, and a custom
implementation of lined list `ConsList`, an object that maintains references for
its head and tail.

```java
  public static class ConsList<A> {

    private A head;
    private ConsList<A> tail;
...
}
```

Function execution time measured in milliseconds per operation (lower is
better).

```text
Benchmark                               (size)  Mode  Cnt     Score     Error  Units
FstBenchmark.serializeArray               1000  avgt    5    21.862 ±   0.503  us/op
FstBenchmark.serializeConsList            1000  avgt    5   151.791 ±  45.200  us/op
FstBenchmark.serializeLinkedList          1000  avgt    5    38.139 ±  12.932  us/op
InstrumentBenchmark.memoryOfArray         1000  avgt    5    17.700 ±   0.068  us/op
InstrumentBenchmark.memoryOfConsList      1000  avgt    5  1706.224 ±  61.631  us/op
InstrumentBenchmark.memoryOfLinkedList    1000  avgt    5  1866.783 ± 557.296  us/op
```

- There are no slowdowns in running the Enso runtime benchmark with the
  instrumentation `javaagent` attached.
- Serialization works in microsecond time range and operates on all Java objects
  implementing _Serializable_ interface.
- Java `Instrumentation#getObjectSize() performs in nanosecond time range. The
  _deep_ inspection approach based on the reflection was significantly slower
  than the serialization.

The resulting approach can be a combination of one of the approaches with the
introspection of the value. For example, it can be a case statement, analyzing
the value and applying the appropriate method.

```java
public long getSize(Object value) {
  if (value instanceof Primitive) {
    return getPrimitiveSize(value);
  } else if (value instanceof EnsoNode) {
    return introspectNode(value);
  } else {
    return null;
  }
}
```
