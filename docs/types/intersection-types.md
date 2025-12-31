---
layout: developer-doc
title: The Enso Type Hierarchy
category: types
tags: [types, hierarchy, typeset]
order: 2
---

# Intersection Types

Intersection types play an important role in Enso
[type hierarchy](./hierarchy.md) and its visual representation. Having a value
that can play _multiple roles_ at once is essential for smooth _live
programming_ manipulation of such a value.

Intersections types are created with the use of
[`&` operator](./hierarchy.md#typeset-operators). In an attempt to represent
`Complex` numbers (with real and imaginary component) one may decide to create a
type that is both `Complex` and `Float` when the imaginary part is `0`:

```ruby
type Complex
    Num re:Float im:Float

    plain_or_both self =
        if self.im != 0 then self else
            both = self.re : Complex&Float
            both # value with both types: Complex and Float
```

Having a value with such _intersection type_ allows the IDE to offer operations
available on all individual types.

## Creating

Creating a value of _intersection types_ is as simple as performing a type
check:

```ruby
self : Complex&Float
```

However such a _type check_ is closely associated with
[conversions](../syntax/conversions.md). If the value doesn't represent all the
desired types yet, then the system looks for
[conversion methods](../syntax/conversions.md) being available in the scope
like:

```
Complex.from (that:Float) = Complex.Num that 0
```

and uses them to create all the values the _intersection type_ represents.

> [!NOTE] Note that if a `Float.from (that:Complex)` conversion were available
> in the scope, any `Complex` instance would be convertible to `Float`
> regardless of how it was constructed. To ensure that such mix-ins are only
> available on values that opt-in to being an intersection type (like in the
> `Complex` example above where we include the `Float` mix-in only if
> `self.im == 0`), we need to ensure that the conversion used to create the
> intersection type is not available in the default conversion resolution scope.
> Thus it cannot be defined in the same module as `Complex` or `Float` types,
> but instead it should be defined in a separate module that is only imported in
> the place that will be constructing the multi-values.

<!--
Just as demonstrated at
https://github.com/enso-org/enso/commit/3d8a0e1b90b20cfdfe5da8d2d3950f644a4b45b8#diff-c6ef852899778b52ce6a11ebf9564d102c273021b212a4848b7678e120776287R23
-->

### Narrowing Type Check

When an _intersection type_ value is being downcast to _some of the types it
already represents_, these types become its _visible_ types. Any additional
types it represents become _hidden_ (unless [open check](#open-type-check) is
requested).

The following operations consider only the _visible_ part of the type:

- [dynamic dispatch](../types/dynamic-dispatch.md)
- [Positional (structural) pattern matching](./pattern-matching.md#positional-matching)
- cases when value is passed as an argument

However the value still keeps internal knowledge of all the types it represents.

Thus, after casting a value `cf:Complex&Float` to just `Complex`, e.g.
`c = cf:Complex`:

- method calls on `c` will only consider methods defined on `Complex`
- the value `c` can only be passed as argument to methods expecting `Complex`
  type
- a type error is raised when a `Float` parameter is expected

As such a [static analysis](./inference-and-checking.md) knows the type a value
_has been cast to_ (the _visible_ part) and can deduce the set of operations
that can be performed with it. Moreover, any method calls will also only accept
the value if it satisfies the type it _has been cast to_. Any additional
remaining _hidden_ types can only be brought back through an _explicit_ cast. To
perform an explicit cast that can uncover the 'hidden' part of a type write
`f = c:Float` or inspect the types in a `case` expression, e.g.

```ruby
case c of
    f : Float -> f.sqrt
    _ -> "Not a float"
```

Remember to use `f.sqrt` and not `c.sqrt`. `f` in the case branch _has been cast
to_ `Float` while `c` in the case branch only _can be cast to_.

> [!WARNING] Keep in mind that while both argument type check in method
> definitions and a 'type asserting' expression look similarly, they have
> slightly different behaviour.
>
> ```
> f a:Float = a
> g a = a:Float
> ```
>
> These two functions, while very similar, will have different behaviour when
> passed a value like the value `c` above. The function `f` will fail with a
> type error, because the visible type of `c` is just `Complex` (assuming the
> conversion to `Float` is not available in the current scope). However, the
> function `g` will accept the same value and return it as a `Float` value,
> based on the 'hidden' part of its type.

> [!NOTE] In the **object oriented terminology** we can think of a type
> `Complex&Float` as being a subclass of `Complex` and subclass of `Float`
> types. As such a value of type `Complex&Float` may be used wherever `Complex`
> or `Float` types are used. Let there, for example, be a function:
>
> ```ruby
> use_complex c:Complex callback:(Any -> Any) = callback c
> ```
>
> that accepts `Complex` value and passes it back to a provided callback
> function. It is possible to pass a value of `Complex&Float` type to such a
> function. Only operations available on type `Complex` can be performed on
> value in variable `c`.
>
> However the `callback` function may still explicitly cast the value to
> `Float`. E.g. the following is valid:
>
> ```ruby
> both = v : Complex&Float
> use_complex both (v-> v:Float . sqrt)
> ```
>
> This behavior is often described as being **open to subclasses**. E.g. the
> `c:Complex` check allows values with _intersection types_ that include
> `Complex` to pass thru with all their runtime information available, but one
> has to perform an explicit cast to extract the other types associated with
> such a value.

This behavior allows creation of values with types like `Table&Column` to
represent a table with a single column - something the users of visual _live
programming_ environment of Enso find very useful.

```ruby
Table.join self right:Table -> Table = ...
```

Such a `Table&Column` value can be returned from the above `Table.join` function
and while having only `Table` behavior by default, still being able to be
explicitly cast by the visual environment to `Column`.

### Open Type Check

There is a special form of _intersection type_ check that can be used to perform
a type check, but avoid _downcasting a value_ (as the
[narrowing check](#narrowing-type-check) does). Use:

```ruby
cf : Complex&Any
```

to **verify** the `cf` value is a `Complex` number, but allow any other _visible
types_ it has to remain _visible_.

Every value in Enso is of `Any` type. Static
[type checking](./inference-and-checking.md#any-is-special) treats `Any` type in
a special way - anything can be done with it. Any method invocation is
(potentially) valid. Thus combining `Complex&Any` check does two things:

- ensures that values that pass such a check offer all functionality of
  `Complex`
- but they also may offer any other functionality (via its `Any` type)

Such a combination is useful for static development tools (like editor code
completion) that can offer methods of `Complex` type for such a value in hand.
Dynamic development tools benefit as well as all the _visible types_ after such
a type check still remain visible and can directly be used.

### Converting Type Check

When an _intersection type_ is being checked against a type it doesn't
represent, any of its component types can be used for
[conversion](../syntax/conversions.md). Should there be a `Float` to `Text`
conversion:

```ruby
Text.from (that:Float) = Float.to_text
```

then `Complex&Float` value `cf` can be typed as `cf:Text`. The value can also be
converted to another _intersection type_ like `ct = cf:Complex&Text`. In such
case it looses its `Float` type and `ct:Float` would fail.

In short: when a [conversion](../syntax/conversions.md) is needed to satisfy a
type check a new value is created to satisfy just the types requested in the
check.

## Signature vs. Cast

There are two slightly different places where _type checking_ occurs:

- function signature definitions - e.g.
  `inc value:Integer by:Integer=1 -> Integer = value+by`
- explicit runtime type checks - e.g. `new_value = value:Integer`

These _type checks_ are almost the same, except small difference with respect to
_intersection types_. While the _function signature checks_ consider **only
visible part** of a type, the _explicit type checks_ "try hard" and **also
include the hidden part** of a type. As a result there two checks maybe yield a
different result:

```ruby
id x -> Text = x

t1 = v:Text
t2 = id v
```

The values `t1` and `t2` are guaranteed to be of type `Text` if the type check
succeeds (and doesn't `Panic`). However it is possible that `t1` becomes `Text`
and `t2` definition panics. Imagine a value defined as

```ruby
both = 42 : Integer&Text # requires a conversion to Text to be around
v = both:Integer # hides the Text type
```

This behavior is important for
[static type checking](./inference-and-checking.md) which operates _only on
visible types_ of a value. Otherwise it wouldn't be possible to report
[meaningful type mismatch errors](./inference-and-checking.md#relation-to-intersection-types).

### Return Type Checks

The small difference in treating _hidden types_ implies that there is a
difference in following two definitions...

```ruby
id x:Text -> Text = x
id x = x:Text
```

...with respect to _intersection types_. Both type check the returned value to
`Text`, but the first one only considers _visible types_, while the second one
considers also _hidden types_.

### Any type Check

Another implication of treating _hidden types_ differently implies that
[open type check](#open-type-check) behaves differently in

```ruby
id x:Text&Any -> Text&Any = x
id x = x:Text&Any
```

again. The first one only considers _visible types_, while the second one
considers also _hidden types_. As a project of this behavior, there is also a
difference in a simple _Any type check_:

```ruby
id x:Any -> Any = x
id x = x:Any
```

the first one is a no-op as every value in Enso is of type `Any`. The second
explicit cast of `x:Any` however **considers the hidden types** and makes them
all visible. Explicit cast to `Any` instructs the runtime to _unhide all types_
of a value and make them _visible_.

## Equality & Hash Code

A value of an intersection type is equal with other value, if all values _it has
been cast to_ are equal to the other value. E.g. a value of `Complex&Float` is
equal to some other value only if its `Complex` part and `Float` part are equal
to the other value. The _hidden_ types of a value (e.g. those that it _can be
cast to_, if any) aren't considered in the equality check.

The order of types isn't important for equality. E.g. `Complex&Float` value can
be equal to `Float&Complex` if the individual components (values _it has been
cast to_) match. As implied by (custom)
[equality rules](../syntax/functions.md#custom-equality) the `hash` of a value
of _intersection type_ must thus be a sum of `hash` values of all the values it
_has been cast to_. As a special case any value wrapped into an _intersection
type_, but _cast down_ to the original type is `==` and has the same `hash` as
the original value. E.g. `4.2 : Complex&Float : Float` is `==` and has the same
`hash` as `4.2` (in spite it _can be cast to_ `Complex`).
