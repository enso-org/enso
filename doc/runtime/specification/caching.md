# Caching
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

- [Dataflow Analysis](#dataflow-analysis)
    - [Identifying Expressions](#identifying-expressions)
    - [Specifying Dataflow](#specifying-dataflow)
- [Cache Eviction Strategy](#cache-eviction-strategy)

<!-- /MarkdownTOC -->

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
Dataflow analysis takes place on the core set of language constructs, defined
as those that extend `IRKind.Primitive`. Their dataflow is specified as follows,
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
All typing operators in Enso (`IR.Type`) are dependent on their constituent
parts:

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
The value of a case expression depends on the value of its scrutinee, as well
as the definitions of its branches.

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

## Cache Eviction Strategy
The cache eviction strategy refers to the process by which, for a given change,
we decide which elements should be evicted from the cache. In the current form,
the following rules are applied when an expression identified by some key `k` is
changed:

1. All expressions that depend on the result of `k` are evicted from the cache.
2. If `k` is a dynamic symbol, all expressions that depend on _any instance_ of
   the dynamic symbol are evicted from the cache.

Expressions that have been evicted from the cache subsequently have to be
recomputed by the runtime.
