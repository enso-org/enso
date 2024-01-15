---
layout: developer-doc
title: Wrapped Errors
category: semantics
tags: [semantics, errors, runtime]
order: 12
---

# Wrapped Errors

A wrapped error is an error wrapped in an additional 'error wrapper' value
providing additional information about the error.

For example, an error attached to a value within a `Vector` can be wrapped in a
`Map_Error` wrapper which indicates the position of the value within the
`Vector`.

(Such errors are only wrapped when they are obtained through
`Warning.get_all wrap_errors=True`; see
[Obtaining Wrapped Errors](#obtaining-wrapped-errors).)

For example:

```ruby
err = My_Error.Error "my error"
vec = [10, Warning.attach err 20, 30]
IO.println (Warning.get_all vec)
IO.println (Warning.get_all wrap_errors=True vec)
```

Output:

```ruby
[My_Error.Error my error]
[Map_Error.Error 1 (My_Error.Error my error)] # The error is at index 1
```

## Catching Wrapped Errors

Wrapped errors are "transparent" to `Error.catch`. That is, if you attempt to
catch a certain error, but the error that is actually thrown is wrapped, the
catch will still succeed. In the example below, a `My_Error` error is thrown in
a call to `map`, and so the resulting error is wrapped. You can catch the error
as a `My_Error`, or as a `Map_Error`.

```ruby
fun x = if x == 20 then Error.throw (My_Error.Error "my error") else x
[10, 20, 30].map fun . catch My_Error e->
    IO.println e
[10, 20, 30].map fun . catch Map_Error e->
    IO.println e
```

Output:

```ruby
(My_Error.Error 'my error')
(Map_Error.Error 1 (My_Error.Error 'my error'))
```

Note that if you catch the error as the inner error (`My_Error`), the
`Map_Error` wrapper is stripped off.

## Implementing Error Wrappers

An error wrapper is a regular Enso value that has a conversion to
`Wrapped_Error`. For example:

```ruby
type Map_Error
    Error (index:Integer) inner_error

Wrapped_Error.from (that : Map_Error) = Wrapped_Error.Value that that.inner_error
```

The `from` implementation allows `Error.catch` to detect that it is an error
wrapper, and possibly perform automatic unwrapping on it.

## Obtaining Wrapped Errors

Wrapped errors are obtained in two ways:

- An error thrown during a call to `Vector.map`
- An error attached to a value within a `Vector`

In the case of an error thrown during `Vector.map`, the error is caught, wrapped
in `Map_Error`, and re-thrown.

In the case of an error attached to a value within a `Vector`, the wrapper is
added by `Warning.get_all wrap_errors=True` when it is called on the `Vector`.
In this case, the wrapping is not attached to the value itself, and is therefore
not propagated to downstream values.

## Map_Error

`Map_Error` is the motivating example for wrapped errors, and is currently the
only implemented error wrapper. It exists so that an error thrown during a `map`
call over a large `Vector` will contain the index at which the error occurred,
so it can be displayed to the user in the IDE.

Note that the error does not have to occur during a call to `map`. A `Map_Error`
wrapping is added by `Warning.get_all wrap_errors=True` to any error attached to
a value within a `Vector` (or any array-like container). If the value is
extracted from the `Vector` (for example, using `.at`), its attached `Warning`
is not wrapped.

If a value is nested within multiple `Vector`s, its attached errors are wrapped
with `Map_Error` multiple times. The outermost `Map_Error` index indicates the
index into the outermost `Vector`, the second `Map_Error` index the index into
the sub-`Vector` within the outermost `Vector`, and so on.

For example:

```ruby
fun a = if a == 30 then Error.throw (My_Error.Error a) else a+1
nested_vector = [[10, 20, 30, 40], [30, 10, 20, 30]]
result = nested_vector.map (_.map fun on_problems=Problem_Behavior.Report_Warning) on_problems=Problem_Behavior.Report_Warning
warnings = Warning.get_all wrap_errors=True result . map .value
warnings.map w->
    IO.println w
```

Output:

```ruby
(Map_Error.Error 1 (Map_Error.Error 3 (My_Error.Error 30))) # [1, 3]
(Map_Error.Error 1 (Map_Error.Error 0 (My_Error.Error 30))) # [1, 0]
(Map_Error.Error 0 (Map_Error.Error 2 (My_Error.Error 30))) # [0, 2]
```

## Testing Wrapped Errors

The following test utilities take an `unwrap_errors` parameter:

- `Error.should_fail_with`
- `Problem.test_problem_handling`
- `Problem.expect_warning`
- `Problem.expect_only_warning`

By default, these methods will automatically unwrap errors. Passing
`unwrap_errors=False` will disable this behavior.
