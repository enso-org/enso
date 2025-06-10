---
layout: developer-doc
title: Errors & Panics
category: types
tags: [types, errors]
order: 12
---

# Errors & Panics

Enso supports two notions of errors. One is the standard exceptions model (built
around `Panic.throw` and related methods), while the other is a theory of
_broken values_ that propagate through computations (represented by `Error` and
created by `Error.throw` method).

> [!WARNING] The actionables for this section are:
>
> - Greatly expand on the reasoning and theory behind the two exception models.
> - Explain why broken values serve the GUI well.
> - Explain how this can all be typed.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Exceptions/Panics](#errors--panics)
- [Broken Values](#broken-values)

<!-- /MarkdownTOC -->

## Exceptions/Panics

> [!WARNING] The actionables for this section are:
>
> - Formalise the model of `Panic.throw` as implemented.

## Broken Values

In Enso we have the notion of a _broken value_: one which is in an invalid
state. Such values are very useful for displaying errors in the GUI.

Broken values are fast to allocate and pass around the program. They record line
of their own creation - e.g. where `Error.throw` has happened. Shall that not be
enough, one can run with `-ea` flag, like:

```bash
enso$ JAVA_OPTS=-ea ./built-distribution/enso-engine-*/enso-*/bin/enso --run x.enso
```

to get full stack where the _broken value_ has been created. Collecting such
full stack trace however prevents the execution to run at _full speed_.

### Promotion of Broken Values

Broken values that aren't handled explicitly are automatically promoted through
the parent scope. Let's assume an `open` function that can yield a `Text` or
_broken value_ representing a `File_Error`:

```ruby
open file_name:Text -> Text ! File_Error = ...
```

Then imagine following `test` function trying to open a non-existing file
`gates.txt`

```ruby
test =
  IO.println 'Opening the gates!'
  open 'gates.txt'
  IO.println 'Gates were opened!'
```

Execution of such function will:

- print `Opening the gates!` text
- finish with `File_Error` _broken value_
- **not print** `Gates were opened!`

E.g. the execution of a function body ends after first _uhandled broken value_.

### Propagation of Broken Values

Let's modify the previous example a bit. Let's assign the read text (or _broken
value_) to a variable and return it from the `test` function:

```ruby
test =
  IO.println 'Opening the gates!'
  content = open 'gates.txt'
  IO.println 'Gates were opened!'
  content
```

If the `gates.txt` file exists, its content is returned from the `test`
function. If a `File_Error` _broken value_ is returned from the `open` function,
then the variable `content` will contain such a _broken value_ and as `content`
is the return value from the `test` function, the `File_Error` will be returned
from the `test` function and propagated further as a _broken value_.

In both situations (if the file exists or not) both `IO.println` statements are
executed and the execution of `test` function thus prints both
`Opening the gates!` as well as `Gates were opened!`.

### Detection of Unused Broken Values

Should the last statement (e.g. `content`) of the `test` function defined in
previous section be missing, then the _broken value_ assigned to `content`
variable might _"disappear"_ unnoticed. However in such a situation the Enso
compiler emits a _compile time warning_:

```bash
test.enso:3:3: warning: Unused variable content.
    3 |   content = open 'gates.txt'
      |   ^~~~~~~
```

The combination of _detection_, _propagation_ and _promotion_ of _broken values_
ensures `File_Error` and other _broken values_ are **never lost**
(unintentionally). Should _loosing a broken value_ be a goal, one can change the
line in question to:

```ruby
  _ = open 'gates.txt'
```

e.g. assign it to anonymous variable. That signals to the system one doesn't
care about the result of the `open` function call. No _compiler warning_ is thus
reported and the _broken value_ gets lost during execution.

To handle _broken values_ properly and recover from such an errorneous state,
use methods offered by `Standard.Base.Error` type like `catch`.
