---
layout: developer-doc
title: Dynamic Dispatch
category: types
tags: [types, dispatch]
order: 6
---

# Dynamic Dispatch

Enso is a language that supports pervasive dynamic dispatch. This is a big boon
for usability, as users can write very flexible code that still plays nicely
with the GUI.

The current implementation of Enso supports single dispatch (dispatch purely on
the type of `self`) when calling function. When calling (binary) operators Enso
may perform more complicated dispatch when searching for the right operator
implementation to invoke.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Specificity](#specificity)
- [Multiple Dispatch](#multiple-dispatch)
- [Resolving Clashes on `Any`](#resolving-clashes-on-any)

<!-- /MarkdownTOC -->

Another page related to [dispatch](../semantics/dispatch.md) exists.

## Specificity

In order to determine which of the potential dispatch candidates is the correct
one to select, the compiler needs to have a notion of _specificity_, which is
effectively an algorithm for determining which candidate is more specific than
another.

> [!WARNING] Static compiler selects nothing. The right method to invoke is
> _selected in the runtime_.
>
> - Always prefer a member function for both `x.f y` and `f y x` notations.
> - Only member functions, current module's functions, and imported functions
>   are considered to be in scope. Local variable `f` could not be used in the
>   `x.f y` syntax.
> - Selecting the matching function:
>   1. Look up the member function. If it exists, select it.
>   2. If not, find all functions with the matching name in the current module
>      and all directly imported modules. These functions are the _candidates_.
>   3. Eliminate any candidate `X` for which there is another candidate `Y`
>      whose `this` argument type is strictly more specific. That is, `Y` this
>      type is a substitution of `X` this type but not vice versa.
>   4. If not all of the remaining candidates have the same this type, the
>      search fails.
>   5. Eliminate any candidate `X` for which there is another candidate `Y`
>      which type signature is strictly more specific. That is, `Y` type
>      signature is a substitution of `X` type signature.
>   6. If exactly one candidate remains, select it. Otherwise, the search fails.

The runtime system of Enso identifies the type of a value in `obj.method_name`
invocation. It checks the _table of virtual methods_ for given type and finds
proper implementation of `method_name` to invoke. Should there be no method of
given name in the value's type (or its supertypes like `Any`) to invoke, a
`No_Such_Method` panic is raised.

There is a special dispatch for
[broken values & warnings](../semantics/errors.md).

## Multiple Dispatch

Multiple dispatch is currently used for
[binary operators](../syntax/functions.md#type-ascriptions-and-operator-resolution).

Multiple dispatch is also used on `from` conversions, because in expression
`T.from x` the function to use is based on both `T` and `x`.

> [!WARNING] Supporting general _multiple dispatch is unlikely_
>
> Supporting it for general functions remains an open question as to whether we
> want to support proper multiple dispatch in Enso. Multiple dispatch refers to
> the dynamic dispatch target being determined based not only on the type of the
> `this` argument, but the types of the other arguments to the function.
>
> To do multiple dispatch properly, it is very important to get a rigorous
> specification of the specificity algorithm. It must account for:
>
> - The typeset subsumption relationship.
> - The ordering of arguments.
> - How to handle defaulted and lazy arguments.
> - Constraints in types. This means that for two candidates `f` and `g`, being
>   dispatched on a type `t` with constraint `c`, the more specific candidate is
>   the one that explicitly matches the constraints. An example follows:
>
> ```ruby
>   type HasName
>     name : String
>
>   greet : t -> Nothing in IO
>   greet _ = print "I have no name!"
>
>   greet : (t : HasName) -> Nothing in IO
>   greet t = print 'Hi, my name is `t.name`!'
>
>   type Person
>     Pers (name : String)
>
>   main =
>     p1 = Person.Pers "Joe"
>     greet p1 # Hi, my name is Joe!
>     greet 7  # I have no name
> ```
>
> Here, because `Person` conforms to the `HasName` interface, the second `greet`
> implementation is chosen because the constraints make it more specific.

TODO: Remove this section?

## Resolving Clashes on `Any`

Special attention must be paid to `Any` and its methods and extension methods.
`Any` is a super type of all objects in Enso. As such the methods available on
`Any` are also available on every object - including special objects like those
representing `type` and holding its _static methods_ (discussed at
[types page](types.md) as well).

There is a `to_text` _instance method_ defined on `Any` - what does it mean when
one calls `Integer.to_text`? Should it by treated as:

```ruby
Any.to_text Integer # yields Integer text
```

or should be a static reference to `Integer.to_text` method without providing
the argument? In case of _regular types_ like `Integer` the following code:

```ruby
main = Integer.to_text
```

is considered as invocation of _instance method_ `to_text` on object `Integer`
and yields `Integer` text.

The situation is different when a _module static_ method together with `Any`
_extension method_ is defined:

```ruby
# following function makes sure `simplify` can be called on any object
Any.simplify self = "Not a Test module, but"+self.to_text
# following "overrides" the method on Test module
simplify = "Test module"
```

With such a setup following code invokes `Any.simplify` extension method

```ruby
main = "Hi".simplify
```

and yields `Not a Test module, but Hi` text. On contrary the following code
yields `Test module` value:

```ruby
main = Test.simplify
```

When invoking a method on _module object_ its _module static methods_ take
precedence over _instance methods_ defined on `Any`. Thus a module serves
primarily as a _container for module (static) methods_.

## Method Invocation

**Terminology**:

- **parameter** is an identifier in a method definition.
  - Every parameter has a position and a name.
  - Parameters can have default values.
- **argument** is an expression in a method invocation.
  - Arguments can be positional or named.
  - Named arguments are written as `name=expression`.
- **static method invocation** is a method invocation with `self` named argument
  provided.
  - Note that `self=expression` does not have to be specified as the first
    argument, but is is a good convention to do so.
  - Note that with this requirement, we essentially define a special syntax for
    _static method invocation_.
- **instance method invocation** is every method invocation that is not
  _static_.
  - That is, it is a method call without `self` named argument provided.
- **eigen type** of a type `My_Type` is a type of type, usually written as
  `My_Type.type`.
  - Every type has an eigen type.
  - Eigen type of an eigen type is itself.
  - There is always exactly one instance of each eigen type, that is, eigen type
    is singleton itself.
  - The name is motivated by an
    [eigen value of a matrix](https://en.wikipedia.org/wiki/Eigenvalues_and_eigenvectors).
- **singleton type** is a type that has no constructors.
  - There can be no atoms of a singleton type.
  - The only instance of a _singleton type_ is the type itself.
  - `Meta.type_of Singleton_Type` is `Singleton_Type`.
  - This is different to `Meta.type_of Normal_Type`, which is
    `Normal_Type.type`.
- **associated type** of a module `My_Module` is a type for the module
  - It is basically an eigen type for a module.
- **builtin type** is a type annotated with `@Builtin_Type`.
  - Builtin type cannot be defined outside standard libraries.
  - Builtin types are usually implemented in the engine, and not with pure Enso
    code.
- **parent type** of a type `My_Type` is a type that `My_Type` "extends", i.e.
  `My_Type` inherits all the methods defined on its parent type.
  - Every type has exactly one parent type, except for `Any` type.
  - `Any` has no parent type.
  - All types have implicit parent type `Any`.
  - There are some exceptions for some _builtin types_
    - For example `Float` and `Integer` builtin types have `Number` parent.
    - `Number` has `Any` parent.
- **symbol table**.
  - Every type has an associated symbol table.
  - Symbol table maps symbols to their definitions.
  - All definitions are methods.
    - Atom constructors are methods.
    - Atom fields are methods. More specifically, every atom field has an
      associated getter method.

This section describes the _method invocation_ process, which resolves a
concrete method definition for a concrete call site and evaluates it. For a
method call expression `Receiver.symbol`, this section focuses only on a single
dispatch based on the `Receiver` argument. For multiple dispatch, see the
[Multiple Dispatch](#multiple-dispatch) section.

This section is further divided into
[Instance method invocation](#instance-method-invocation) and
[Static method invocation](#static-method-invocation). Note the differences
between these types of invocations:

- Static method invocation has `self` named argument provided.
- Instance method invocation provides `self` argument implicitly.

### Instance method invocation

Instance method invocation is any method invocation without `self` named
argument specified (see terminology). Before a method is invoked, it needs to be
_resolved_. Method resolution algorithm for the `Receiver.symbol` expression
first determines the _type_ of the `Receiver`, and then finds the method
definition in its _symbol table_:

1. **Determine the type of `Receiver`:**

- 1.1. If `Receiver` is type, the result will be _eigen type_.
- 1.2. If `Receiver` is _singleton type_, the result will be the _type itself_.
- 1.3. If `Receiver` is a value (instance / atom), the result will be the _type
  of the value_.
- 1.4. If `Receiver` is a module, the result will be the _associated type_ for
  the module.
- 1.5. If `Receiver` is a polyglot object, method resolution and invocation will
  be handled according to the [polyglot interoperability](../polyglot/README.md)
  rules.
  - Polyglot object can be, for example:
    - A Java class, imported by `polyglot java import ...` statement.
    - Java object instance, created by `Java_Class.new ...` expression.
    - Javascript, Python, or any other allowed foreign language object returned
      by a foreign method call.
    - Refer to [polyglot readme](../polyglot/README.md) for more details.
- 1.6. If there is no `Receiver`, we are just looking for a variable in the
  current lexical scope or any parent scopes. See
  [Lexical scope lookup](#lexical-scope-lookup).

2. **Look up symbol in the symbol table of the determined type:**

- 2.1. Lookup the `symbol` in the Receiver's type and all its parent types.
- 2.2. If it is found, continue to 3.
- 2.3. If it is not found, raise `No_Such_Method` panic and stop.

3. **Invoke the method with defaulted self argument**:

- 3.1. `symbol` is a method in Receiver's type (or its parent type) symbol
  table.
- 3.2. Such method is treated as if it's first argument is named `self` and has
  the preapplied value of `Receiver`. In other words, the method invocation is
  equivalent to the `method self=Receiver` expression.

### Static method invocation

Static method invocation is any method invocation with `self` named argument
provided (see terminology). Let's consider the following static method
invocation expression: `Receiver.symbol self=receiver`.

4. **Resolve method on Receiver**:

- 4.1. Lookup the `symbol` in `Receiver` (not its type!) and all its parent
  types.
  - Note that if `Receiver` is not a type itself, it has no symbol table, so no
    method is found.
- 4.2. If it is found, continue to 5.
- 4.3. If it is not found, raise `No_Such_Method` panic and stop.

5. **Invoke the method with the provided self argument**:

- 5.1. Method was found on `Receiver` or its parent type.
- 5.2. Treat the method as if it's first parameter is named `self` and has no
  default value.
- 5.3. Bind the `receiver` expression to `self` parameter.

### Lexical scope lookup

- If `symbol` is defined in the current lexical scope, select it and stop.
- Iterate parent scopes: from the current lexical scope, up until this module
  scope. If `symbol` is defined in the scope, select it and stop.
- Look for the `symbol` in all transitively imported modules (in DFS?). If
  `symbol` is defined in any of the modules, select it and stop.
- Raise `Name_Not_Found` panic and stop.

## Examples

```
@Builtin_Type
type Any
    to_text self = "???"

type My_Type
    Cons
    method self x = x + 1

obj = My_Type.Cons
```

### Example (a)

Evaluation of `obj.method 41`:

- Receiver type is determined as `My_Type` (1.3)
- `method` is looked up in `My_Type` symbol table, and found (2.2)
- `method` is executed as `My_Type.method self=obj x=41` (3.2)
- expression is evaluated to 42.

### Example (b)

Evaluation of `My_Type.method obj 41`:

- Receiver type is determined as `My_Type.type` (1.1)
- There is no `method` in `My_Type.type` symbol table (2.3)
- Raise `No_Such_Method` panic.

### Example (c)

Evaluation of `My_Type.method self=obj 41`:

- Lookup `method` on `My_Type` (4.1).
- `My_Type.method` is found (4.2).
- Execute method as `My_Type.method self=obj x=41` (5.2).
- expression is evaluated to 42.

### Example (d)

Evaluation of `Any.to_text obj`:

- Receiver type is determined as `Any.type` (1.1)
- There is no `Any.type.to_text` method (2.3)
- Raise `No_Such_Method` panic.

### Example (e)

Evaluation of `Any.to_text self=obj`:

- Lookup `to_text` on `Any` (4.1).
- `Any.to_text` is found (4.2).
- Execute method as `Any.to_text self=obj` (5.2).
- expression is evaluated to `"???"`.

### Example (f)

Evaluation of `My_Type.to_text`:

- Receiver type is determined as `My_Type.type` (1.1)
- `to_text` is found in `Any` symbol table (2.2)
  - `Any` is parent of `My_Type.type`.
- method is executed as `Any.to_text self=My_Type` (3.2)
- expression is evaluated to `"???"`.
