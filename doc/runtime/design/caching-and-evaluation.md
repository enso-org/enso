# Caching and Evaluation
It is not uncommon for users in data-analysis jobs to work with data on the
order of _gigabytes_ or even _terabytes_. As fast as computers have become, and
as efficient as programming languages can be, you still don't want to compute on
such large amounts of data unless you absolutely have to.

This wouldn't usually be an issue, with such data-analysis tasks being able to
run in a 'batch mode', where the user starts their job in a fire-and-forget
fashion. Enso, however, is a highly _interactive_ environment for working with
data, where waiting _seconds_, let alone _hours_, would severely hamper the user
experience.

To that end, Enso's runtime includes an in-built value caching mechanism. This
mechanism is used to remember values of computations such that they can be
re-used as down-stream code is changed. This means that only the _necessary_
parts of the computation are re-evaluated when code is changed, providing users
with a far more interactive and responsive experience.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Cache Candidates](#cache-candidates)
- [Partial-Evaluation and Side-Effects](#partial-evaluation-and-side-effects)
    - [Side Effects in the Initial Version](#side-effects-in-the-initial-version)
    - [In The Future](#in-the-future)
- [Cache Eviction Strategies](#cache-eviction-strategies)
    - [Initial Eviction Strategies](#initial-eviction-strategies)
    - [Future Eviction Strategies](#future-eviction-strategies)

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
cache design evolves, the _selected_ candidates may be refined. Once the values
for each candidate are stored, changes to the code will re-use these cached
values as much as possible.

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

## Cache Eviction Strategies
The cache eviction strategy refers to the method by which we determine which
entries in the cache are invalidated (if any) after a given change to the code.

### Initial Eviction Strategies
In the initial version of the caching mechanism, the eviction strategies are
intended to be fairly simplistic and conservative to ensure correctness.

- The compiler performs data-dependency analysis between expressions.
- If an expression is changed, all cached values for expressions that depend on
  it are evicted from the cache.
- These evicted values must be computed.

### Future Eviction Strategies
In the future, however, the increasing sophistication of the front-end compiler
for Enso will allow us to do better than this by accounting for more granular
information in the eviction decisions.

> The actionables for this section are:
>
> - Evolve the cache eviction strategy by employing more granular information
>   as the compiler evolves to provide it.
