---
layout: developer-doc
title: Projections and Field Access
category: syntax
tags: [syntax, projections, field-access, pattern-matching]
order: 12
---

# Projections and Field Access

Enso provides multiple ways for users to access data from their types. It has
the old functional stalwart of pattern matching, but it also has an inbuilt
notion of accessors based on lenses (field projections).

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Pattern Matching](#pattern-matching)
  - [The Underscore in Pattern Matching](#the-underscore-in-pattern-matching)
- [Projections](#projections)

<!-- /MarkdownTOC -->

## Pattern Matching

Pattern matching in Enso works similarly to as you would expect in various other
functional languages. Typing information is _always_ refined in the branches of
a case expression, which interacts well with dependent typing and type-term
unification. There are a few main ways you can pattern match:

1.  **Positional Matching:** Matching on the scrutinee by structure. This works
    both for atoms and typesets (for typesets it is a subsumption judgement).

    ```ruby
    type Vector a
      V2 x:a y:a
      V3 x:a y:a z:a

    v = Vector.V3 x y z

    case v of
      Vector.V3 x y z -> print x
    ```

2.  **Type Matching:** Matching purely by the types involved, and not matching
    on structure.

    ```ruby
    case v of
      Vector.V3 -> print v.x
    ```

3.  **Name Matching on Labels:** Matching on the labels defined within a type
    for both atoms and typesets, with renaming.

    ```ruby
    case v of
      Vector.V3 {x y} -> print x
      {x}             -> print x
    ```

4.  **Naming Scrutinees in Branches:** Ascribing a name of a scrutinee is done
    using the standard typing judgement. This works due to the type-term
    unification present in Enso.

    ```ruby
    case _ of
      v : Vector.V3 -> print v,x
    ```

> The actionables for this section :
>
> - Refine the syntax for the name-based case.
> - Provide code examples for why the renaming use-case is important (e.g. cases
>   where there are clashing field names).
> - Function-resolution matching.

### The Underscore in Pattern Matching

An underscore `_` passed as an argument to a syntactic pattern does not behave
like the function argument shorthand. Instead it acts as a positional match that
is given no name.

## Projections

Unlike the simple accessors defined by most programming language, Enso's
accessors are far more powerful. This is because they are based on lenses.

- Field accessors are standard lenses. As such, they can be used for both the
  getting and setting of properties.
- As a lens (e.g. `.field`) is a first-class function, they can be curried and
  passed around like any other function.

```ruby
type Engine
    type Combustion
        power:          Int
        cylinder_count: Int

    type Electric
        power:   Int
        is_blue: Bool

type Vehicle
    type Car
        color:     String
        max_speed: Int
        engine:    Engine

    type Bike
        color: String

type Person
    type Cons
        name:    String
        vehicle: Vehicle

main =
  p1 = Person.Cons "Joe" (Vehicle.Car 'pink' 300 (Engine.Combustion 500 8))
  print $ p1.name                   # -> Joe
  print $ p1.vehicle.color          # -> pink
  print $ p1.vehicle.max_speed      # -> Some 300
  print $ p1.vehicle.engine.power   # -> Some 500
  print $ p1.vehicle.engine.is_blue # -> None
  p1.vehicle.color     = 'red'      # OK
  p1.vehicle.max_speed = 310        # FAIL: security reasons. Allowing this
                                    #       in Haskell was the worst decision
                                    #       ever. After refactoring it
                                    #       silently does nothing there.

  p2 = p1.vehicle.max_speed    ?= 310 # OK
  p3 = p1.vehicle.engine.power  = 510 # FAIL
  p4 = p1.vehicle.engine.power ?= 510 # OK

  lens_name      = .name
  lens_color     = .vehicle.color
  lens_max_speed = .vehicle.max_speed
  lens_power     = .vehincle.engine.power

  ## Function like usage:
  print $ lens_name      p1
  print $ lens_color     p1
  print $ lens_max_speed p1
  print $ lens_power     p1

  p1 . at lens_name = ... # OK
```

> The actionables for this section are:
>
> - Fix the example above. It isn't correct.
