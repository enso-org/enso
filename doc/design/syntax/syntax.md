- **Feature Name:** Enso 2.0 Design
- **Start Date:** 2018-06-26
- **Change Type:** Breaking
- **RFC Dependencies:**
- **RFC PR:**
- **Enso Issue:**
- **Implemented:**

# Summary

The importance of a language's syntax cannot be understated. Badly designed
syntax introduces confusion, leads to unreadable code, and stands in the way of
the language's evolution. Good syntax is fast to write, easy to understand by a
whole team of developers and promotes best practices. In addition to being an
obvious truth to any programmer, it can be seen in real world examples: just
look at Haskell, whose base syntax is over 20 years old. These days, with
Haskell entering the world of dependent types, it has become abundantly clear
that its syntax is not able to cope, leading to confusion even amongst seasoned
language users.

A language's syntax directly affects how code is structured, and as a result
affects what is considered to be idiomatic style. Any sensible programming
language must provide users with the ability to modularise their code and split
it into well-defined parts. The ML family of languages, in particular 1ML and
OCaml, take the concept of a module even further. Enso, with its unique system
for categorical typing, is in a position to provide the most flexible
implementation of modules yet, unifying the concepts of modules, classes and
interfaces.

This proposal introduces a major breaking change for Enso, wholesale replacing
portions of the Language's syntax and semantics with an entirely new model. As a
result, this RFC aims to describe the whole new design in a form of a
documentation with frequent annotations providing rationale for the changes.

# Design Principles

It is impossible to design even a small part of a language without considering
almost every other design decision. Over the past years we have learned that the
only way which will brings us a step closer to a design that fits well into all
requirements is a design that bases on a small set of well defined invariants.
Invariants derive from a careful analysis of the needs. Their definition should
always be the first step when searching for a solution to a complex problem.
They should be used as a very efficient filter to test new ideas and discovering
bad decisions.

Below we present fundamental assumptions regarding how the Enso language should
look and feel like:

1. **The textual syntax must play well with the visual representation.**  
   Both visual and textual representations are equivalently important. Any rule
   which does not fit both worlds at the same time will be rejected.

2. **Easiness in understanding is more important than design minimalism.**  
   Enso is meant to be production, not a research language. It targets a broad
   range of developers and domain experts. Thus it should be fast to write,
   comfortable to read and easy to reason about. In particular, it should
   provide easy to understand compile time errors, which is why for example
   monads in Enso are a special entity handled by the compiler.

3. **There should be one (and preferably only one) way to achieve a goal.**  
   One of the greatest power of a good syntax is that it is easy to read by
   different people from different organizations. The more coding styles or
   design pattern rules users have to learn, the more codebases with different,
   often incompatible approaches will appear. In the ideal world, a language
   would provide one and only one way to write and format code, which would also
   be fast to write and easy to understand by people. Enso design should be
   aligned with this vision.

4. **Type level syntax = value level syntax.**  
   Enso type system is designed to be as expressive and as natural to use as
   rest of the code. We believe that the only true solution for next generation
   programming languages is a well designed dependent type system which will
   blend type level and value level computations into a single scope. Creating a
   future proof dependent type system is still an open research field and we can
   observe many different approaches among modern programming languages and
   learn from their mistakes. One of such biggest mistake is using different
   syntax forms or namespaces for type and value level expressions. It leads to
   having special mechanisms to promote values between the namespaces, like
   prefixing value level data with apostrophe to bring it to type level and
   prevent name clash (see `-XDataKinds` in Haskell).

5. **Small number of rules is better than large.**  
   Any special case or syntactic rule has to be remembered by the user and
   consumes important cognitive power. On the other hand, the syntax can easily
   be oversimplified, which usually leads to complex, hard to understand errors.
   Usually it is preferred to choose a solution which does not introduce any new
   special cases.

6. **Predictable performance and behavior.**  
   Predictable performance and behavior is one of the most important principles
   which separates well designed languages from the bad designed ones. A
   language provides a predicable behavior when its user can write code which
   will not break because of some external conditions, like not-dependent code
   change. A good examples of breaking this rule are standard extension methods
   mechanism (monkey patching in Ruby, Python, JavaScript) or orphan overlapping
   instances in Haskell. Moreover, simple refactoring of the code should never
   affect the performance. Again, consider Haskell here. Changing
   `func2 a = func1 a` to `func2 = func1`
   [can affect performance](https://gitlab.haskell.org/ghc/ghc/issues/8099)
   which it makes Haskell programs very hard to reason about.

# Source Code

## Encoding

Enso accepts UTF8 encoded source code. Tabs are disallowed and every tab is
always automatically converted to four spaces. There is no configuration option
provided on purpose. All variables and operators identifiers are restricted to
ASCII characters. Enso libraries should be widely accessible and users cannot
struggle with typing the function names. However, we understand that there are
situations when using Unicode characters is desirable, for example to design a
high level visual library targeting a narrow domain in a particular country.
That's why Enso allows users to specify optional localized names as part of
function documentation and provides a special support for searching them in Enso
Studio.

We do not plan to support the usage of Unicode characters for operators.
Paraphrasing the
[Idris wiki](https://github.com/idris-lang/Idris-dev/wiki/Unofficial-FAQ#will-there-be-support-for-unicode-characters-for-operators),
which we highly agree with:

- Unicode operators are hard to type. This is important, as it often disables
  the possibility of using someone else's code. Various code editors provide
  their users with their own input methods, but we haven't experienced an
  efficient UX yet.
- Not every piece of software easily supports it. Unicode does not render
  properly on some phone browsers, email clients, or IRC clients to name a few.
  All of these can be fixed by the end user, for example by using a different
  software. However, it sets a higher barrier to entry to using a programming
  language.
- Many Unicode characters look very similar. We had enough trouble with
  confusion between 0 and O without worrying about all the different kinds of
  colons and brackets.

Surely, Unicode operators can make the code look pretty, however, proper font
with a well-designed ligatures is able to provide the same, or very similar
results. We are very open to revisit this topic in a few years from now,
however, for now Unicode characters are disallowed in Enso operators. If you
want to help us design an Enso Font, don't hesitate to tell us about it!

## Layout rules

Enso uses indentation to determine the structure of the code. The layout rules
were designed to be both flexible yet enforce good practices.

In general, every indented line consists a sub-structure of the nearest previous
line with a smaller indentation. We refer to them as child line and parent line,
respectively. There are a few additional layout rules:

- **Operator on the end of a parent line**  
  If a line ends with an operator then all of its child lines form a code block.
  Code blocks in Enso are a syntactic sugar for a monadic bindings, you will
  learn about them in later chapters. The most common usage is a function
  definition body after the last arrow operator:

  ```haskell
  test = a -> b ->
      sum = a + b
      print 'The sum is `sum`'
  ```

* **Operator on the beginning of a child line**  
  If all the children lines start with operators, they form a single expression,
  while the operators behave left associative with the lowest precedence level.
  In other words, every line forms a separate expression and the final
  expression is build line-by-line, top to bottom. The most common usage is to
  use the dot operator to create chained method calls. Please note that the
  operator on the beginning of a child line is used after the line expression is
  formed, so in the following code both `tst1` and `tst2` have exactly the same
  value.

  ```haskell
  nums = 1..100
       . each random
       . sort
       . take 100

  tst1 = 12 * (1 + 2)
  tst2 = 12
       * 1 + 2
  ```

* **Otherwise**  
  In all other cases, every child line is considered to form a separate
  expression passed as an argument to the parent expression. The most common
  usage is to split long expressions to multiple lines. The following example
  uses the named argument mechanism.

  ```haskell
  geo1 = sphere (radius = 15) (position = vector 10 0 10) (color = rgb 0 1 0)
  geo2 = sphere
      radius   = 15
      position = vector 10 0 10
      color    = rgb 0 1 0
  ```

- **Debug line breaker `\\`**

  There is also a special, debug line-break operator `\\` which placed on the
  beginning of a child line tells Enso to just glue the line with the previous
  one. However, the line-break operator should not be used in production code,
  as it's always better to re-structure the code to separate method calls
  instead. In the following code, both `debugFunc` and `validFunc` work in the
  same way, but the definition of `validFunc` is formatted properly.

  ```haskell
  debugFunc = v -> v2 ->
    print (v2.normalize * ((v.x * v.x) + (v.y * v.y)
      \\ + (v.z * v.z)).sqrt)

  validFunc = v -> v2 ->
    len = ((v.x * v.x) + (v.y * v.y) + (v.z * v.z)).sqrt
    v'  = v2.normalize * len
    print v'
  ```

## Naming Rules

Naming convention unifies how code is written by different developers, increases
the immediate understanding and allows to provide compiler with useful
information in a very convenient fashion. In particular, pattern matching
requires a way to distinguish between free variables and already declared ones.
Enso uses a simple naming convention to provide such information:

- You are free to use capitalized and uncapitalized identifiers as function and
  variable names. Type definition identifier should always be capitalized.
- Capitalized and uncapitalized identifiers are not distinguishable and always
  refer to the same value.
- Using capitalized identifier to refer to uncapitalized one is allowed in
  pattern matching only. Using uncapitalized identifier to refer to capitalized
  one is disallowed.
- In pattern matching capitalized identifiers refer to values in the scope,
  while uncapitalized identifiers are used for free variables only.

Using upper case letters for constructors in pattern matching has important
benefits. Whenever you see an upper case identifier, you know it is a data
structure being taken apart, which makes it much easier for a human to see what
is going on in a piece of code. Moreover, while using this convention,
construction and pattern matching is as simple as writing the right name and
does not require any magic from the compiler or usage of special symbols.

# Functions

Enso is a purely functional programming language. It supports
[first-class and higher-order functions](https://en.wikipedia.org/wiki/Functional_programming#First-class_and_higher-order_functions),
which means that you can pass functions as arguments to other functions, return
them as functions results, assign them to variables, and store them in data
structures.

## Using Functions

Putting a space between two things is simply _function application_. For
example, to sort a list, write `sort [5,3,8]`, simply. Of course, you can define
much more complex expressions. The following code defines a sequence of one
hundred numbers, uses each of them to get a new random number, discards
everything but the first 10 numbers, and then sorts them. Please note the usage
of the `each` function, which takes an action and a list as arguments, and
applies the action to every element of the list. The `random` returns a
pseudo-random number if applied with a seed value (it always returns the same
value for the same seed argument).

```haskell
list       = 1 .. 100
randomList = each random list
headOfList = head 10 randomList
result     = sort headOfList
```

### Currying

Every function in Enso officially only takes one parameter. So how is it
possible that we provided the `each` function with more than one parameter?
Well, it's a clever trick! All the functions that accepted _several parameters_
are so called _curried functions_. What does that mean? Consider a `max 1 2`
expression. The `max` function looks like it takes two parameters and returns
the one that's bigger. In reality, doing `max 1 2` first creates a function that
takes a parameter and returns either `1` or that parameter, depending on which
is bigger. Then, `2` is applied to that function and that function produces our
desired result. That sounds like a mouthful but it's actually a really cool
concept. The following two calls are equivalent:

```haskell
max 1 2
(max 1) 2
```

### Using Default Arguments

Unlike the majority of purely functional programming languages, Enso supports
functions with default arguments. Let's see why they are useful. Consider a
function that creates a sphere based on the provided radius, position, color and
geometry type (like polygons or
[NURBS](https://en.wikipedia.org/wiki/Non-uniform_rational_B-spline)). Providing
always all the arguments manually is both cumbersome and error prone:

```haskell
s1 = sphere 10 (point 0 0 0) (color.rgb 0.5 0.5 0.5) geometry.NURBS
```

Function definition allows providing a default value to some of the arguments.
The value will be automatically applied if not provided explicitly. For example,
the above code is equivalent to:

```haskell
s1 = sphere 10
```

Informally, when you call a function, Enso will traverse all not provided
arguments in order and will apply the default values unless it founds the first
argument without a default value defined. To disable this behavior, you can use
the special `...` operator. The following code creates a curried function which
accepts position, color and geometry type and creates a sphere with radius of
`7`:

```haskell
sphere7 = sphere 7 ...
```

### Using Positional Arguments

Enso supports so called positional arguments call syntax. Consider the sphere
example above. How can you define a new function which accepts radius, color and
geometry type and returns a sphere always placed in the center of the coordinate
system? There are few ways. First, you can create the function explicitly (you
will learn more about function definition in the following chapters):

```haskell
originSphere radius color creator = sphere radius (point 0 0 0) color creator
```

Alternatively, you can use the positional arguments call syntax:

```haskell
originSphere = sphere _ (point 0 0 0) _ _
```

Of course, you can combine it with the operator canceling default argument
application:

```haskell
originSphere = sphere _ (point 0 0 0) ...
```

There is an important rule to remember. Enso gathers all positional arguments
inside a particular function body or expression enclosed in parentheses in order
to create a new function, so the following code creates a function accepting two
arguments. It will result the sum of the first argument squared and the second
argument.

```haskell
squareFirstAndAddSecond = _ ^2 + _
```

### Using Named Arguments

Unlike the majority of purely functional programming languages, Enso supports
functions with named arguments.Consider the sphere example above again and the
explicit positional function usage:

```haskell
s1 = sphere 10 (point 0 0 0) (color.rgb 0.5 0.5 0.5) geometry.NURBS
```

Using the named arguments, we can transform the code to:

```haskell
s1 = sphere (radius = 10) (position = point 0 0 0) (color = color.rgb 0.5 0.5 0.5)
            (creator = geometry.NURBS)
```

By applying the layout rules described above, we can transform the code to a
much more readable form:

```haskell
s1 = sphere
    radius   = 10
    position = point 0 0 0
    color    = color.rgb 0.5 0.5 0.5
    creator  = geometry.NURBS
```

We can now define the function which places the sphere in the center of the
coordinate system even simpler:

```haskell
originSphere = sphere
    position = point 0 0 0
    ...
```

### Operators

Operators are functions with non alphanumeric names, like `+`, `-` or `*`.
Operators are always provided with two arguments, one on the left, one one the
right side, for example, in order to add two numbers together you can simply
write `1 + 2`. It could be a surprise, but we've been using a lot of operators
so far – a space is a special operator which applies arguments to functions!
Space has a relatively high precedence, higher than any operator, so the code
`max 0 10 + max 0 -10` is equivalent to `(max 0 10) + (max 0 -10)`. Another
interesting operator is the field accessor operator, often referred to as the
dot operator. It is used to access fields of structures. For example, to print
the first coordinate of a point `pt` you can simply write `print pt.x`. However,
please note that the way the accessor function behaves differs from probably
every language you've learned so far. You'll learn more about it in the
following sections.

### Uniform Calling Syntax (UCS)

Enso uses Uniform Calling Syntax which generalizes two function call notations
`lst.map +1` and `map +1 lst`. The generalization assumes flipped argument order
for operators, so `a + b` is equal to `a.+ b`. Paraphrasing Bjarne Stroustrup
and Herb Sutter, having two call syntaxes makes it hard to write generic code.
Libraries authors will either have to support both syntaxes (verbose,
potentially doubling the size of the implementation) or make assumptions about
how objects of certain types are to be invoked (and we may be wrong close to 50%
of the time).

Each of these notations has advantages but to a user the need to know which
syntax is provided by a library is a bother. Thus implementation concerns can
determine the user interface. We consider that needlessly constraining.

The following rules apply:

- Two notations, one semantics. Both notations are equivalent and always resolve
  to the same behavior.
- The argument on a position of the function arity (informally, the last
  function argument) is considered to be the self element.

Function resolution:

- Always prefer a member function for both `x.f y` and `f y x` notations.
- Only member functions, current module's functions, and imported functions are
  considered to be in scope. Local variable `f` could not be used in the `x.f y`
  syntax.
- Selecting the matching function:
  1. Look up the member function. If it exists, select it.
  2. If not, find all functions with the matching name in the current module and
     all directly imported modules. These functions are the _candidates_.
  3. Eliminate any candidate `X` for which there is another candidate `Y` whose
     `me` argument type is strictly more specific. That is, `Y` self type is a
     substitution of `X` self type but not vice versa.
  4. If not all of the remaining candidates have the same self type, the search
     fails.
  5. Eliminate any candidate `X` for which there is another candidate `Y` which
     type signature is strictly more specific. That is, `Y` type signature is a
     substitution of `X` type signature.
  6. If exactly one candidate remains, select it. Otherwise, the search fails.

For example, the following code results in a compile time error. The self type
`[Int, Int]` is strictly more specific than the type `[a,b]` and thus this
candidate was selected in the step 3 of the algorithm. However, it is impossible
to unify `1` and `Text`.

```haskell
test = n -> [a,b] ->
    [a+n, b+n]

test : Text -> [Int, Int] -> [Text, Text]
test = s -> [a,b] ->
    [s + a.show , s + b.show]

[1,2].test 1
```

### Operator Precedence and Space-based Precedence

Operator precedence is a collection of rules that reflect conventions about
which procedures to perform first in order to evaluate a given mathematical
expression. For example, multiplication operator is granted with a higher
precedence than addition operator, which means that multiplication will be
performed before addition in a single expression like `2 + 5 * 10`.

However, in contrast to most languages, the operator precedence depends on the
fact if a particular operator was surrounded with spaces or not. **The
precedence of any operator not surrounded with spaces is always higher than the
precedence of any operator surrounded with spaces.** For example, the code
`2+5 * 10` results in `70`, not `50`!

The space-based precedence allows for writing much cleaner code than any other
functional language, including all languages from the ML family, like Haskell,
Agda or Idris. Let's consider the previous example:

```haskell
list       = 1 .. 100
randomList = each random list
headOfList = head 10 randomList
result     = sort headOfList
```

It could be easily refactored to a long one-liner:

```haskell
result = sort (head 10 (each random (1 .. 100)))
```

Such expression is arguably much less readable than the original code, as it
does not allow to read in a top-bottom, left-right fashion. However, by using
the Uniform Calling Syntax, we can further transform the code:

```haskell
result = (((1 .. 100).each random).head 10).sort
```

Much better. We can now read the expression from left to right. The result is
still a little bit verbose, as we need to use many nested parentheses. The
space-based precedence combined with the fact that the accessor is just a
regular operator in Enso allow us to throw them away! The rule is simple – the
space operator has higher precedence than any operator surrounded with spaces:

```haskell
result = 1..100 . each random . head 10 . sort
```

### Operator Sections

Operator section is just a handy way to apply the left or the right argument to
an operator and return a curried function. For example, the expression `(+1)` is
a function accepting a single argument and returning an incremented value.
Incrementing every value in a list is a pure joy when using sections:

```haskell
list  = 1 .. 100
list2 = list.each (+1)
```

Because the space-based precedence applies to sections as well, the above code
may be further simplified to:

```haskell
list  = 1 .. 100
list2 = list.each +1
```

Another interesting example is using the accessor operator with the section
syntax. The following code creates a list of one hundred spheres with random
positions sorts them based on the first position coordinate. The `.position.x`
is just a section which defines a function taking a parameter and returning its
nested field value.

```haskell
spheres       = 1..100 . each i -> sphere (position = point i.random 0 0)
sortedSpheres = spheres . sortBy .position.x
```

## Defining Functions

Functions are defined in a similar way that they are called. The function name
is followed by parameters seperated by spaces. But when defining functions,
there's an equal symbol and after that we define what the function does. For
example, the following code defines a function taking two values and returning
their sum.

```haskell
sum x y = x + y
```

You can also create an unnamed function, often referred to as lambda. In fact,
the above code is just a syntactic sugar for more explicit lambda syntax:

```haskell
sum = x -> y -> x + y
```

### Operators

Enso gives a lot of flexibility to developers to define custom operators.
Formally, any sequence of the following characters forms an operator
`.!$%&*+-/<>?^~\`. The operator definition is almost the same as function
definition, with an optional precedence relation declaration. Consider the
following definition from the standard library:

```haskel
a ^ n =
    prec > *
    prec < $
    assoc left
    a * a ^ (n-1)
```

There are two special syntax forms allowed in the operator definition body. The
`prec` keyword specifies the
[precedence relation](https://en.wikipedia.org/wiki/Order_of_operations) to
other operators. Here, we specified that the precedence is bigger than the
multiplication operator. The precedences are inherited in Enso, so if the
multiplication operator was provided with information that it has a bigger
precedence than addition, the new operator above will inherit this dependency as
well. The `assoc` keyword defines the
[operator associativity](https://en.wikipedia.org/wiki/Operator_associativity) –
it is either left, right or none. If you do not provide the information, no
precedence relations would be defined and the associativity will default to
none.

### Named Arguments

Using the same name in a single function definition will result in a compilation
error. Functions are disallowed to have several arguments named the same way.
Even if you create a function returning another function in such a way that the
argument names overlap, you will not be allowed to provide arguments by name
starting with the argument which name collided first. Consider the following
example:

```haskell
fn1 c b d = a + 1
fn2 a b   = fn1
```

The function `fn2` accepts two named arguments `a` and `b` and returns another
function which accepts three named argument `c`, `b`, and `d`. In such a
situation you can provide `fn2` with named arguments `a`, `b`, and `c`, but you
are not allowed with providing it with the second `b` nor `d`.

### Default Arguments

...

### Argument Holes

## Function Type

As the function definition is an ordinary variable assignment, you can use the
type expression to provide Enso with an additional information about arguments
and the result types. If no explicit type is provided, Enso infers the most
specific type, the function body.

```haskell
sum : x -> y -> x + y
sum = x -> y -> x + y
```

An explicit type narrows the scope of possible values accepted by the function.
For example, we can tell that the function accepts only numbers, so for example
values of text type will be rejected:

```haskell
sum : Number -> Number -> Number
sum = x -> y -> x + y
```

Each function is assigned with an **arity**. It is the number of arguments and
lambdas statically used in its definition. Note that arity is not deducible from
the type. For example, the function `fn` has the arity of `1` even though its
type suggests it takes `3` arguments:

```haskell
fn : Bool -> Bool -> Bool -> Bool
  fn a = b -> case a && b of
    True  -> not
    False -> id
```

### Named and default arguments

Every lambda argument is either provided with an unique name or is unnamed, but
then cannot be accessed from within the lambda body. Lambda arguments can be
provided either in form of (`name : type`) or just (`type`). Consider the
following examples to understand it better:

```haskell
foo : x -> y -> x + y
foo = x -> y -> x + y
```

The above code defines a function which takes two polymorphic, unrelated
arguments and adds them together. The type tells about a single constraint, that
we need to know how to add `x` and `y`. No other constraints are provided. We
can change the code:

```haskell
foo : x:a -> y:a -> (x + y : a)
foo = x:a -> y:a -> (x + y : a)
```

This time another constraint is provided as well. All `x`, `y` and the result
are unified to the same type variable `a`. Of course you can drop the explicit
type signature here and get the same results or simplify both lines to:

```haskell
foo : a -> a -> a
foo = x -> y -> z
```

This code has exactly the same meaning as the previous one. You can omit
argument names in explicit type signature if used next to the definition. If you
would like to use the function signature in an interface and use named
arguments, you would also need to provide names explicitly.

### Named arguments

Every part of language, which affects how data could be accessed or modified
have to expressible on type level. For example, interfaces have to allow
declaring that some of function's parameters are named or provided with a
default value.

Assuming that the arrow operator (`->`) is just a normal operator, not some
deeply magical symbol, then based on `{invariant:4}` the following code has to
be accepted:

```haskell
a : x -> y -> z
a = x -> y -> z
```

Which consequently makes the following usage invalid:

```haskell
-- INVALID --
sum : int -> int -> int
sum a b = a + b
```

Moreover, the following code is valid:

```haskell
sum : (a : int) -> (b : int) -> (a + b : int)
sum (a : int) (b : int) = a + b
```

Let's consider now how sub-typing in the Enso sense works for functions. In
order to get some intuition, visualize the left hand side just as a set
transformation to other set, as every possible arrow from one set to other set.
Such transformation is a sub-type of every transformation that contains all the
arrows, in particular the following expressions are valid:

```haskell
(nat -> string) : (int -> string)
(nat -> nat -> string) : (int -> int -> string)
(int -> nat) : (int -> int)
```

After thinking for a while about it, the rules are rather straightforward.
Moreover, Enso allows typing an expression using any supertype, for example we
are allowed to type `0 : int` or `Vector 1 2 3 : vector int`. Thus, each of the
following definitions is valid (see type-patterns construction):

```haskell
sum : (a : int) -> (b : int) -> (a + b : int)
sum (a : int) (b : int) = a + b

sum : type int -> type int -> int
sum (a : int) (b : int) = a + b
```

Please note that in the above example the `type int` is a pattern which means
that it is a set-type `int` which is either named or unnamed. It does NOT mean
that it is an unnamed function, otherwise the above sub-typing rules will not be
met.

Few potential solutions emerge:

#### Special unnamed type provider syntax

We could introduce a special syntax, which just automatically drops all names in
patterns (it works just like type-case construction), lets define it as `::`
operator. Then the following would then be valid:

```haskell
sum :: int -> int -> int
sum a b = a + b
```

#### Special unnamed type provider syntax 2

This idea is almost the same as previous, but we swap `:` with `::`. In such
case we will use `:` almost always, including:

```haskell
foo : int
foo = 5

sum : int -> int -> int
sum a b = a + b
```

And we would use `::` only in interfaces when we will provide names explicitly,
like:

```haskell
type SomeIface
    foo :: x : int -> y : int -> int
```

It's worth to note that such construct would be very rare. In fact I don't
believe it would be ever used in real code, because it just defines interface of
a function containing two named arguments in a particular order. Thus maybe it
would be possible to find a syntax which just tells that we've got named
arguments in an undefined order and use it with the `:` operator then (keeping
all the assumptions from this point valid)?

#### Special arrows

Another solution would be to introduce two types of arrows - named `->` and
unnamed `=>` (or vice versa), thus the following code would be valid:

```haskell
foo : (x : int) -> (y : int) -> int
foo x y = x + y

bar : int => int => int
bar x y = x + y
```

Which has almost all information duplicated and is just equivalent to:

```haskell
sum (a : int) (b : int) = a + b
```

Assuming that on the left side of the arrow there is just standard pattern
expression, both `x` and `y` are names, while `z` is the result.

foo (x : int) (y : int) (z : int) = ...

foo = x : int -> y : int -> z : int -> ...

foo : int -> int -> int foo x y z = ...

foo : x -> y -> ... foo = x -> y -> ...

foo : a -> a -> a -> ... foo = x -> y -> z -> ...

foo : (x : int) -> (y : int) -> (z : int) -> ... foo x y z = ...

### Default arguments

# Types

Enso is a statically typed language. It means that every variable is tagged with
an information about its possible values. Enso's type system bases on the idea
that each type is denoted by a set of values, called `constructors`. Formally,
this makes the type system a
[Modular Lattice](https://en.wikipedia.org/wiki/Modular_lattice). For an
example, the type `Nat` contains constructors `1, 2, 3, ...`, and is hence
denotable by a set of the possible values.

As a result, typechecking doesn't work via _unification_ as one might expect if
they are familiar with other functional programming languages, but instead
checks if a given set of values is a valid substitution for another. We can, of
course, have the empty set (∅), and also sets containing single elements (single
constructors).

This notion is supported by an enforced equivalence between value-level and
type-level syntax in Enso, as the compiler makes no distinction between the two.
This means that it is perfectly valid to type `7 : 7`, because values always
form just a set with a single member. Because we can describe infinite number of
sets containing a particular value, every value in Enso has infinite number of
types. Taking in consideration the lucky number `7`, it is a `Natural` number,
`Integer` number, and a `Number` at the same time! This relation could be
expressed as follow:

```haskell
7 : 7 : Natural : Integer : Number
```

## The Any Type

There is a type which defines a set of all possible values in Enso. It is called
the `Any` type. As any other type, the `Any` type has its own type as well. The
type of `Any` is simply the `Any` type. Beside being the type of any value, the
`Any` type has a very unique behavior, it allows Enso to behave as a dynamic
language on demand!

TODO TODO TODO

including info about getting fields by name

## Atomic Types

Atomic types are the most primitive structures in Enso. Formally, atomic types
are [product types](https://en.wikipedia.org/wiki/Product_type). Their fields
are always named and are fully polymorphic (each field has a distinct
polymorphic type). Atoms are distinguishable. You are not allowed to pass an
atom to a function accepting other atom, even if their fields are named the same
way.

```haskell
type Vec3   x y z
type Point3 x y z

vec1 = Vec3   1 2 3 : Vec3   1 2 3 : Vec3   Int Int Int
pt1  = Point3 1 2 3 : Point3 1 2 3 : Point3 Int Int Int

test : Vec3 Int Int Int -> Int
test v = v.x + v.y + v.z

test pt1 -- Compile time error. Expected Vec3, got Point3.
```

## Algebraic Types

Enso allows you to define new types by combining existing ones, so called
[algebraic data types](https://en.wikipedia.org/wiki/Algebraic_data_type). Enso
provides you with several algebraic operations on types:

- **Types Intersection**  
  A type intersection combines multiple types into one type that has all the
  features combined. For example, `Serializable & Showable` describes values
  that provide mechanisms for both serialization and printing.

- **Types Difference**  
  A type difference combines multiple types into one type that has all the
  features of the first type but not the features of the second one. For
  example, `Int \ Negative` describes all positive integer values or zero.

- **Types Union**  
  A type union combines multiple types into one type that describes a value
  being of one of the types. For example, `Int | String` describes values that
  are either `Int` or `String`.

```haskell
type Just value
type Nothing
maybe a = just a | nothing

map : (a -> b) -> Maybe a -> Maybe b
map f = case of
    Just a  -> Just (f a)
    Nothing -> Nothing
```

### Syntax sugar

Enso provides a syntactic sugar for easy definition of algebraic data types and
related methods. You are always required to provide explicit name for all the
constructors and all its fields.

```haskell
type Maybe a
    Just value:a
    Nothing

    map : (a -> b) -> me b
    map f = case me
        Just a  -> Just (f a)
        Nothing -> Nothing
```

Please note, that all functions defined in the type definition scope are
desugared to global functions operating on that type. However, all functions
defined as constructor field are considered to be record fields. They can be
provided with a default implementation and their definition can be changed in
runtime.

```haskell
-- Difference between method and a function component
type Foo
    MkFoo
        function : Int -> self
        function = default implementation

    method : Int -> self
    method = implementation
```

## Interfaces

Types which contain only field declarations but does not have restrictions on
the accepted atoms, behave like interfaces. Any type which conforms to the shape
of the interface is considered an implementation of the interface, regardless of
its explicit definition.

```haskell
type Show
    show : Text
    show = 'default'

type Vector a
    implements Show
    V3 x:a y:a z:a

    show = 'Vector #{self.x} #{self.y} #{self.z}'
```

As Luna types are always sets of values, there is no type-level parameters
ordering like in Haskell. Consider the following code:

```haskell
type Vector a
    V3 x:a y:a z:a

type Functor a
    map: (a -> b) -> self b

test :
    t : Functor
    t a -> t Text
test = map show
```

```haskell
type Vector a
    V3 x:a y:a z:a

interface Functor t
    map: (a -> b) -> t a -> t b

test :
    Functor t
    t a -> t Text
test = map show
```

## Field Modifiers

You can add the equal sign `=` as an operator suffix to transform it into a
modifier. Modifiers allow updating nested structures fields.

In general, the following expressions are equivalent. The `%` operator can be
replaced with any other operator:

```haskell
foo' = foo.bar %= t
-- <=>
bar'  = foo.bar
bar'' = t % bar'
foo'  = foo.bar = bar''
```

Please not the inversed order in the `t % bar` application. In most cases it
does not change anything, however, it's very useful as it allow us use such
operators as `foo.bar $= f` in order to modify a nested field with an `f`
function.

Examples:

```haskell
type Vector
    V3 x:Number y:Number z:Number

type Sphere
    MkSphere
        radius   : Number
        position : Vector

-- Position modification
s1 = MkSphere 10 (V3 0 0 0)
s2 = s1.position.x += 1

-- Which could be also expressed as
p1 = s1.position
p2 = p1.x += 1
s2 = s1.position = p2

-- Or as a curried modification
s2 = s1.position.x $= +1
```

## Prisms

Alternative map implementations:

```haskell
type Shape a
    Circle
        radius:a
    Rectangle
        width:a
        height:a

map1 : (a -> b) -> Shape a -> Shape b
map1 f self = case self of
    Circle    r   -> Circle    (f r)
    Rectangle w h -> Rectangle (f w) (f h)

map2 : (a -> b) -> Shape a -> Shape b
map2 f self = self
    ? radius $= f
    ? width  $= f
    ? height $= f

map3 : (a -> b) -> Shape a -> Shape b
map3 f self = if self.is Circle
    then self . radius $= f
    else self . width  $= f
              . height $= f

map4 : (a -> b) -> Shape a -> Shape b
map4 f self =
    maybeNewCircle    = self.circle.radius $= f
    maybeNewRectangle = self.rectangle.[width,height] $= f
    case maybeNewCircle of
        Just a  -> a
        Nothing -> case maybeNewRectangle of
            Just a  -> a
            Nothing -> error "impossible"
```

# Type Inference

Because every value belongs to infinite number of types, it's not always obvious
what type to infer by looking only at the variable definitions. The expression
`fib 10` could be typed as `55`, `Int` or `Any`, `Int`, to mention a few. The
way we type it depends on two factors:

- **The optimizations we want to perform**  
  The performance implications are obvious. By computing the value during
  compilation, we do not have to compute it during runtime anymore. On the other
  side, compile time function evaluation is often costly, so such optimization
  opportunities should be always chosen carefully.

- **The information we need to proof the corectness of the program**  
  In a case we drop the results, like `print $ const 10 (fib 10)`, it's
  completely ok to stop the type checking process on assuming that the type of
  `fib 10` is just any type, or to be more precise, a `Type`. Its value is
  always discarded and we do not need anymore information to prove that the type
  flow is correct. However, if the result of `fib 10` would be passed to a
  function accepting only numbers smaller than `100`, tha value have to be
  computed during compilation time.

### Explicite type signatures

Enso was designed in a way to minimize the need for explicit type signatures.
However, you are always free to provide one to check your assumptions regarding
the types. There are two major ways explicit type signatures are used in Enso:

- **Explicit type constraints**  
  Explicit type signatures in type and function definitions constrain the
  possible value set. For example, you will not be allowed to pass a text to a
  function provided with an explicit type `fn : Int -> Int`.

- **Explicit type checks**  
  Explicit type signatures in other places in the code are used as type checks.
  If you type your variable as `Number` it does not mean that enso will forget
  about other information inferred so far. It will always check if the signature
  is correct and report an error in case it's not. For example, the following
  code will type check correctly.

  ```haskell
  dayNumber = 1 | ... | 7
  printDay : DayNumber -> Nothing
  printDay = print

  myDay = 1 : Number
  printDay myDay
  ```

**Example 1**

```haskell
square : (Text -> Text) | (Number -> Number)
square val = case val of
    Text   -> 'squared #{val}'
    Number -> val * val

action f a b = print 'The results are #{f a} and #{f b}'

main = action square "10" 10
```

**Example 2**

```haskell
foo : Number -> Text | Integer
foo = if x < 10 then "test" else 16

fn1 : Text | Number -> Number
fn1 = ...

fn2 : Text | Vector Number -> Number
fn2 = ...

fn3 : 16 -> 17
fn3 = +1

main =
    val = foo 12
    fn1 val -- OK
    fn2 val -- ERROR
    fn3 val -- OK
```

#### Simplified Type Signatures

Types in Enso can be expressed in a very detailed form. Consider an `open`
function, which reads a file from disc. It's type could be expressed as:

```haskell
open : FilePath -> Text ! FileReadError in IO
```

The are two important operators used here. The first one is the `!` operator,
which just means that instead of this value, we can get an error. The second one
is the `in` operator, which tells

```haskell
openReadAndCompare
    : FilePath -> Bool ! (IOError | ConversionError) in IO & State Int
openReadAndCompare path =
    currentNumber = State.get
    contents      = open path
    convertedNum  = contents.as Int
    convertedNum < currentNumber
```

### Implicit Type Conversions

```haskell
foo . bar.baz

just    = type a
nothing = type
maybe a = just a | nothing
```

```haskell
val1 = just 5 : Maybe Int
```

```haskell
maybe a = type
    just    = type a
    nothing = type

val1 = just 5 : Maybe Int
```

```haskell
type vector a
    x : a
    y : a
    z : a

    length : a
    length = (my.x^2 + my.y^2 + my.z^2) . sqrt


    TYPY JAKO MODULY, ELEMENTY MODULOW POLIMORFICZNYCH BEZ SATURACJI


length : Vector a -> a
length self = (self.x^2 + self.y^2 + self.z^2) . sqrt

maybe a = type
    type just a
    type nothing

val1 = just 5 : Maybe Int
```

```haskell
type maybe a
    type just a
    type nothing

val1 = just 5 : Maybe Int
```

## Record Types

```haskell

Point = {x: Number, y: Number, z: Number}

type Point
    x: Number
    y: Number
    z: Number

```

```haskell
type Nothing
type Just value
maybe = a -> Just a | Nothing


Point = {x:Number, y:Number, z:Number}

map : (Number -> Number) -> Point -> Point
map = f ->
    . x $= f
    . y $= f
    . z $= f

p1 = Point 0 1 2 (p -> p.x + p.y + p.z)
p2 = map +1 point
p3 = point.map +1

-- fn : Point -> (Point -> Number)


foo : Int -> Point -> Point
foo = i -> map +i

foo : Int -> Int
foo = +1


test : foo
test = foo 1


test = map (+1)

```

```haskell
number = 17
```

The `number` is a subtype of infinite number of types, where the most specific
type is `17` and the most general is `Type`. We can express an example relation
as follow:

```haskell
number = 17 : 17 : Natural : Integer : Number : Type
```

By looking at the definition it is not possible to tell which type should be
inferred by the compiler. We could of course always try to infer the most
specific type, but then the amount of compile-time operations would completely
kill the compiler performance. Instead, we can

```haskell


fn : a -> a < 10
fn = a -> a < 10
```

```haskell
merge = ts -> Type.from $ ts.map .type.values . concat
```

```haskell
type Showable
    show : Text

type Serializable
    encode : self -> Binary
    decode : Binary -> self


Int implements Serializable
    encode = ...
    decode = ...


decode : Binary -> Int
decode = ...

decode : Binary -> String
decode = ...


foo : Serializable -> Binary
foo = a -> encode a

```

# Types - OLD

## Types. Unified Classes, Modules and Interfaces

Enso unifies the abstraction of classes, modules and interfaces under a single
first-class umbrella. All of the following functionalities are provided by the
`type` keyword, resulting in a highly flexible language construct:

- **Classes.** Types provide containers for data and associated behavior.
- **Modules.** Types provide namespacing for code and data.
- **Interfaces.** Types provide behavior description required of a type.

At a fundamental level, the definition of a new `type` in Enso is the creation
of a (usually named) category of values described by the data and behavior it
possesses. These are first-class values in Enso, and can be created and
manipulated at runtime.

## Type Signatures

Enso allows providing explicit type information by using the colon operator. The
compiler considers type signatures as hints and is free to discard them if they
do not provide any new information. However, if the provided hint is incorrect,
an error is reported.

For example, the following code contains an explicit type signature for the `a`
variable. Although the provided type tells that `a` is either an integer number
or a text, the compiler knows its exact value and is free to use it instead of
the more general type. Thus, no error is reported when the value is incremented
in the next line.

```haskell
a = 17 : Int | Text
b = a + 1
print b
```

However, if the provided type contains more information than the currently
inferred one, both are merged together. Consider the following example for
reference.

```haskell
test : Int -> Int -> Int
test = a -> b ->
    c = a + b
    print c
    c
```

Without the explicit type signature, the inferred type would be very generic,
allowing the arguments to be of any type as long as it allows for adding the
values and printing them to the screen. The provided type is more specific, so
Enso would allow to provide this function only with integer numbers now.
However, the provided type does not mention the context of the computations. The
compiler knows that `print` uses the `IO` context, so considering the provided
hint, the final inferred type would be
`Int in c1 -> Int in c2 -> Int in IO | c1 | c2`.

It's worth to note that the type operator is just a regular operator with a very
low precedence and it is defined in the standard library.

## Types as Classes

The following chapter describes the replacement for the currently used concept
of _classes_. We have been always dreaming about true dependent typed language
and the way classes currently work stands on the way to achieve the dreams. The
change is, however, not as drastic as it seems. It is rather a process of
extending the current model to provide more fine grained control over the
objects and types.

Enso is an Object Oriented programming language. It provides the notion of
objects and methods so at first glance, Enso types may seem like conventional
_classes_ from traditional object-oriented languages. However, these concepts
differ significantly. Enso types have much more power, yet much simpler design,
disallowing concepts like inheritance in favour of composition and algebraic
data types.

### Constructors

While types in Enso describe categories of values, the constructors are the
values themselves. Constructors are used for defining new data structures
containing zero or more values, so called fields. Formally, constructors are
product types, a primitive building block of algebraic data types.

A constructor definition starts with the `type` keyword followed by the
constructor name and lists its fields by name with possible default values. It
is possible to create unnamed fields by using wildcard symbol instead of the
name. Constructors cannot be parametrized and their fields cannot be provided
with explicit type annotations. The formal syntax description is presented
below.

```
consDef   = "type" consName [{consField}]
fieldName = varName | wildcard
consField = fieldName ["=" value]
```

Below we present code snippets with constructors definitions. Constructors with
the same name are just alternative syntactic forms used to describe the same
entity. We will refer to these definitions in later sections of this chapter.

```haskell
-- Boolean values
type True
type False

-- Structure containing two unnamed fields
type Tuple _ _

-- Alternative Point definitions:
type Point x y z

type Point (x = 0) (y = 0) (z = 0)

type Point x=0 y=0 z=0

type Point
    x = 0
    y = 0
    z = 0
```

### Methods

A method is a function associated with a given constructor. The primitive method
definition syntax is very similar to function definition, however it also
includes the constructor in its head:

```haskell
True.not  = False
False.not = True
Point x y z . length = (x^2 + y^2 + z^2).sqrt
Tuple a b . swap = Tuple b a
```

Most often methods are defined in the same module as the appropriate
constructors. Please refer to sections about interfaces and extension methods to
learn more about other possibilities.

### Constructors as types

As Enso is a dependently-typed language with no distinction between value- and
type-level syntax, we are allowed to write _very_ specific type for a given
value. As described earlier, constructors are the values belonging to categories
defined by Enso types. However, they are not only members of categories, they
are also useful to describe very specific categories per se. Formally, a
constructor is capable of describing any subset of the set of all possible
values of its fields.

For example, the `True` constructor could be used to describe the set of all
possible values of its fields. While it does not have any fields, the set
contains only two value, the `True` constructor itself and an `undefined` value.
Thus it is correct to write in Enso `True : True` and assume that the only
possible values of a variable typed as `a : True` are either `True` or
`undefined`.

On the other hand, The `Point` constructor do contain fields, thus it could be
used for example to describe all possible points, whose first coordinate is an
integral number, while the second and third coordinates are equal to zero:
`a : Point int 0 0`.

### Type combinators

The careful reader will notice here, that `int` is a category of all possible
integral numbers, while the numbers are considered constructors themselves. Enso
provides an operator used to join types together, the so called pipe operator.
The hypothetical `int` definition could look like `int = .. | -1 | 0 | 1 | ...`.
We can use this mechanism to easily express even complex type dependencies. For
example we can tell Enso that a particular value has the type of `int | text`.
Enso will allow us to either use pattern matching to discover at runtime which
type are we really dealing with or will allow to use only methods which have
common interface among all constructors described by the type. It will for
example allow us to print such value to the screen.

### Pattern matching

The proposed syntax changes allow us to improve pattern matching rules and make
them much more understandable, especially for new users. As we have described
earlier, there is no need to use qualified constructor names or special cases in
patterns anymore. Moreover, a new form of pattern matching is introduced, the so
called "type pattern matching".

While constructors allow combining fields into a single structure and type
combinators allow joining types into more general ones, the pattern matching
mechanism allows going the opposite direction. In the most common use case
pattern matching will be performed during runtime, however it is worth to note
that the Enso compiler has enough information to perform pattern matching during
compilation if the appropriate values could be deduced in the compilation
process. There are two forms of pattern matching, namely constructor pattern
matching and generalized type pattern matching. The former syntax is practically
identical to the existing one, while the later uses the `type` keyword to denote
that we are performing pattern matching on type descriptor. Let's see how the
new syntax looks like in practice:

```haskell
type shape a
    type Circle
        radius :: a

    type Rectangle
        width  :: a
        height :: a


main =
    c1 = Circle 5 :: shape int
    v  = if something then c1 else 0

    print case v of
        Circle r   -> 'it is circle'
        type shape -> 'it is other shape'
        _          -> 'it is something else'

    print case v of type
        shape -> 'it is shape'
        int   -> 'it is int'

```

### Polymorphism

Formally polymorphism is the provision of a single interface to entities of
different types. Enso does not provide any special construction to support
polymorphism, because even very complex polymorphic types could be described
just by using type-level functions. Consider the following example code:

```haskell
type Point x y z
point a = Point a a a

main =
    p1 = Point 1 2 3 :: point int
    print p1
```

The `point` function is the most basic form of polymorphic type definition in
Enso. It defines all such sets of points, whose all components belong to the
provided type. To better understand this relation, please consider the following
valid expressions:

```haskell
p1 = Point 1 2 3 : Point 1 2 3
p1 = Point 1 2 3 : Point int int int
p1 = Point 1 2 3 : point int

Point 1 2 3 : Point 1 2 3 : Point int int int : point int
```

This is a very flexible mechanism, allowing expressing even complex ideas in a
simple and flexible manner. An example is always worth more than 1000 words, so
please consider yet another example usage:

```haskell
taxiDistance : point real -> point real -> real
taxiDistance p1 p2 = (p2.x - p1.x).abs + (p2.y - p1.y).abs + (p2.z - p1.z).abs

main =
    p1 = Point 1 2 3
    print $ taxiDistance p1
```

### Generalized type definitions

While we can define constructors, methods and compose them to create more
powerful types using the described methods, such definitions require significant
amount of code and do not reflect the real dependencies between the definitions.
This is the reason why Enso provides a syntactic sugar allowing to define
everything we have learned so far in more concise form.

It is worth emphasizing that generalized type definitions are only a simpler way
to define multiple constructors, combine them into a common type and define
common methods. They do not provide any additional value or functionality. The
generalized type definition syntax is presented below:

```
typeDef = "type" varName [":" interface] [({consDef} | {consField})] [method]
```

The body of a type can contain functions, data, or even _other types_, and _yes_
because you were wondering, types _can_ be defined inductively or using a GADT
style. We can re-write the earlier provided definitions using this form as
follow:

```haskell
type bool
    type True
    type False

    not = case self of
        True  -> False
        False -> True
```

```haskell
type point a
    x y z = 0 : a

    length = (x^2 + y^2 + z^2).sqrt
```

```haskell
type tuple a b
    _ : a
    _ : b

    swap = Tuple b a
```

While using this form we define common methods on a set of constructors, like
the method `not` and we use pattern matching to chose the right algorithm path,
this approach does not have any performance penalties, because the compiler is
provided with enough information to optimize this check away if the value was
known at compile time.

One important thing to note here is that if you don't define any explicit
constructors, an implicit one will be generated automatically and will be named
the same way as the type but starting with an upper-letter instead. Now we can
use the above definitions as follow:

```haskell
test check =
    p1 = Point 1 2 3 : point int
    p2 = Point 4 5 6 : point real
    px = if check then p1 else p2
    print px.length
```

**Bonus question**  
What is the most concrete type of the `px` variable above if we do not have any
information about the value of `check`? The answer is of course
`px : (Point 1 2 3 | Point 4 5 6)`, which is a sub type of the type
`Point (1|4) (2|5) (3|6)`.

## Types as Modules

The same notion of a type can be used to provide the functionality that is
traditionally expected of a _module_ (in the common, not ML sense). In most
programming languages, their module system provides a mechanism for code-reuse
through grouping and namespacing. Indeed, Enso's types provide both of these
functionalities:

- **Grouping of Code**  
  A `type` declaration acts as a container for code, with functions able to be
  declared in its scope.
- **Namespacing**  
  Unless otherwise declared (through a direct import statement), a `type` in
  Enso also provides a namespace to constructs declared inside its scope.

### Files and modules

Files in Enso should contain at least one `type` definition, with one type named
the same as the file. This `type` is known as the 'primary' type, and it is this
type that is referred to when importing the 'module'. A file `data/map.luna` may
contain `type map`, `type helper` and various other types, but the only things
visible outside the file are the primary type and things defined or imported
into its scope. Inside the file, however, everything can be seen, with no need
to forward-declare.

### Module Examples

The concepts are best illustrated by example. Consider the following type. If it
is imported simply as `import math` (see [Importing Types](#importing-types)),
then `pi` value is only accessible within the scope of `math` (by using
`math.pi`).

However, please note that `math.pi` is not some kind of a special construct for
a qualified data access. The `math` is a zero-argument constructor of the `math`
module (type). The expression `math.pi` is creating the module object and then
accessing its `pi` field. Of course such creation would be optimized away during
the compilation process.

File `math.luna`:

```haskell
type math
    pi: 3.14
```

File `main.luna`:

```haskell
type main
    import math
    main = print math.pi
```

## Types as Interfaces

A type in Enso can also act as a 'contract', a specification of the behavior
expected of a type. The use of types as interfaces in Enso is, as you might
expect, contravariant. As long as the type satisfies the category defined by the
interface, it can be used in its place. This leads to the expected semantics
where a type `Foo` implementing `Bar` can be used where a `Bar` is expected.

Interfaces in Enso can range from general to very specific. As they define a
_category_ of values, interfaces can specify anything from function signatures
that must be present, all the way to names that must be present in the type's
scope and default behavior. The following are all valid ways to define types for
use as interfaces in Enso.

```haskell
-- This interface requires a function called someFunction with the correct sig.
type Interface1
    someFunction : Int -> String

-- This interface requires a function and a variable both named appropriately.
type (a : Numeric) => Interface2 a
    someVar : a

    someFunction : a -> a
    someFunction = ...

-- This interface requires a function foo with the appropriate type.
type Interface3 a = { foo : a -> a }
```

For more information on the last example, please read the section on
[anonymous types](#anonymous-types).

### Implementing Interfaces

TODO: This section needs discussion. It is a very draft proposal for now.

The nature of Enso's type system means that any type that _satisfies_ an
interface, even without explicitly implementing it, will be able to be used in
places where that interface is expected. However, in the cases of named
interfaces (not [anonymous types](#anonymous-types)), it is a compiler warning
to do so. (TODO: Explain why. What bad would happen otherwise?)

You can explicitly implement an interface in two ways. Examples of both can be
found at the end of the section.

1. **Implementation on the Type**  
   Interfaces can be directly implemented as part of the type's definition. In
   this case the type header is annotated with `: InterfaceName` (and filled
   type parameters as appropriate). The interface can then be used (if it has a
   default implementation), or the implementation can be provided in the type
   body.

2. **Standalone Implementation:**  
   Interfaces can be implemented for types in a standalone implementation block.
   These take the form of `instance Interface for Type`, with any type
   parameters filled appropriately.

Both of these methods will support extension to automatic deriving strategies in
future iterations of the Enso compiler.

It should also be noted that it is not possible to implement orphan instances of
interfaces in Enso, as it leads to difficult to understand code. This means that
an interface must either be implemented in the same file as the interface
definition, or in the same file as the definition of the type for which the
interface is being implemented. (TODO: To be discussed)

Consider an interface `PrettyPrinter` as follows, which has a default
implementation for its `prettyPrint` method.

```haskell
type (t : Textual) => PrettyPrinter t =
    prettyPrint : t
    prettyPrint = self.show
```

For types we own, we can implement this interface directly on the type. Consider
this example `Point` type.

```haskell
type Point : PrettyPrinter Text
    x : Double
    y : Double
    z : Double

    prettyPrint : Text
    prettyPrint = ...
```

If we have a type defined in external library that we want to pretty print, we
can define a standalone instance instead. Consider a type `External`.

```haskell
instance PrettyPrint Text for External =
    prettyPrint = ...
```

HOLES!!!

<!-- #### On the Semantics of Standalone Implementations
Standalone implementations allow for limited extension methods on types. The
interface methods implemented for a type in the standalone definition can be
used like any other method on a Enso type.

#### Overlapping Interface Implementations
Sometimes it is beneficial to allow interfaces to overlap in one or more of
their type parameters. This does not mean Enso allows _duplicate_ instances (
where all of the type parameters are identical). These can be implemented by
either of the methods above, but the user may often run into issues when
attempting to make use of these interfaces.

Enso thus provides a mechanism for the programmer to manually specify which
instance of an interface should be selected in the cases where resolution is
ambiguous. Consider the following example, using the `PrettyPrinter` interface
defined above.

```
type Point2D : PrettyPrinter Text | PrettyPrinter ByteArray =
    x : Double
    y : Double

    prettyPrint : Point2D -> Text
    prettyPrint self = ...

    prettyPrint : Point2D -> ByteArray
    prettyPrint self = ...

loggerFn (a : PrettyPrinter b) -> Text -> a -> Text
loggerFn msg item = msg <> prettyPrint(Text) item
```

As you can see, the syntax for specifying the instance in the ambiguous case
uses parentheses to apply the type to the `prettyPrint` function.  -->

## Imports

To go along with the new system proposed in this RFC around code modularity, the
syntax for dealing with imports has been tweaked slightly. The following import
syntaxes are valid:

- **Direct Imports:** These import the primary module from the file. This brings
  the type and its constructors into scope. For example `import Data.Map` would
  bring `Map` and its constructors into scope.
- **Specified Imports:** These allow the specification of additional functions
  to be brought into the current scope. For example `import Data.Map: fromList`
  would bring `Map`, its constructors and `fromList` into scope.
- **Renamed Imports:** These allow for the programmer to rename the imported
  type. For example `import Data.Containers.Map as MapInterface` brings `Map`
  into scope named as `MapInterface`. Here, constructors are also imported.
- **Specialised Imports:** These allow the programmer to specialise type
  arguments as part of the import. For example `import Data.Map String` will
  import `Map` and its constructors with their first type arguments specialised
  to `String`.

These above import styles can be combined, for example renaming a partially
specialised import (`import Data.Map String as StringMap`), or specialising
functions imported into scope (`import Data.Map String: fromList`). Much like
curried type application seen elsewhere in this proposal, it is possible to
partially apply the type arguments of an import, as seen above.

<!-- #### The File Scope
Files in Enso should contain at least one `type`, with one type named the same
as the file. This `type` is known as the 'primary' type, and it is this type
that is referred to when importing the 'module'. A file `Data/Map.luna` may
contain `type Map`, `type Helper` and various other types, but the only things
visible outside the file are the primary type and things defined in its scope.
Inside the file, however, everything can be seen, with no need to
forward-declare. -->

### Scoping Rules and Code Modularity

Imports in Enso can be performed in _any_ scope, and are accessible from the
scope into which they are imported. This gives rise to a particularly intuitive
way of handling re-exports.

Consider the following file `Test.luna`. In this file, the imports of `Thing`
and `PrettyPrint` are not visible when `Test.luna` is imported. However,
`PrettyPrint` and `printer` are made visible from within the scope of `Test`.
This means that a user can write `import Test: printer` and have it work.

```
import Experiment.Thing
import Utils.PrettyPrint

type Test a : PrettyPrint Text (Test a) =
    import Utils.PrettyPrint: printer

    runTest : a -> Text
    runTest test = ...

    prettyPrint : Test a -> Text
    prettyPrint self = ...
```

## Anonymous Types

In addition to the syntax proposed above in [Declaring Types](#declaring-types),
this RFC also proposes a mechanism for quickly declaring anonymous types. These
types are anonymous in that they provide a category of values without applying a
name to their category, and can be created both as types and as values.

While it is possible to use the primary type declaration syntax without
providing an explicit name, this is highly impractical for most places where an
anonymous type becomes useful. This shorthand provides a way to get the same
benefit without the syntactic issues of the former.

### Anonymous Types as Types

When used in a type context, an anonymous type acts as a specification for an
interface that must be filled. This specification can contain anything from
types to names, and features its own syntax for inline declarations.

Consider the following examples:

- `{Int, Int, Int}`: This type declares a set of values where each value
  contains three integers.
- `{Int, foo : Self -> Int}`: This type declares a set of values with an integer
  and a function from `Self` to an Integer with name `foo`.
- `{Self -> Text -> Text}`: This defines an unnamed function. This may seem
  useless at first, but the input argument can be pattern-matched on as in the
  following example:

  ```
  foo : { Int, Int, Self -> Int } -> Int
  foo rec@{x, y, fn} = fn rec
  ```

`Self` is a piece of reserved syntax that allows anonymous types to refer to
their own type without knowing its name.

### Anonymous Types as Values

Anonymous types can also be constructed as values using similar syntax. You can
provide values directly, which will work in a context where names are not
required, or you can provide named values as in the following examples:

- `{0, 0}`: This anonymous value will work anywhere a type with two numbers and
  no other behaviour is expected.
- `{x = 0, y = 0, z = 0}`: This one provides explicit names for its values, and
  will work where names are required.
- `{x = 0, fn = someFunction}`: This will also work, defining the value for `fn`
  by use of a function visible in the scope.
- `{x = 0, fn = (f -> pure f)}`: Lambda functions can also be used.

## Nested Types

As any kind of language construct can be nested inside a `type`, we are able to
nest types arbitrarily in Enso. This leads to a very expressive language, but
the semantics of such nested types need careful attention.

This RFC proposes that a nested type becomes a separate type held by each
_instance_ of a type. This means that `foo.A != bar.A`, where `foo : Foo` and
`bar : Foo`. The benefits of doing this means that you have true associated
types, rather than just types contained by other types.

## Example - Dependent Vector

```
type (n : Nat) => Vector n a =
    # Constructor
    vector : (n = 1 : Nat)

    # The parameter name is visible at the 'type level', allowing dependent type
    mkVec : Nat -> Vector n a
    mkVec (n = 1) = ...
```

## Example - Linked List

This example uses currently forbidden syntax for declaring operators, but let's
pretend it works.

```
type List a =
    nil
    cons a (List a)

    (:) = cons
    []  = nil
```

## First-class sequential code blocks.

### Current problems <!-- omit in toc -->

Consider the following PSEUDO-code:

```haskell
result = State.run Map.empty
    {
    samples.each sample->
        print "Sample `sample`"
        out = runSimulation sample
        State.modify (.insert sample out)
    }
```

It is currently not possible to express this code in Enso. You cannot pass a
sequential code block as a an argument. In order to make the code working it
should be currently refactored to:

```haskell
helper = samples.each sample->
    print "Sample `sample`"
    out = runSimulation sample
    State.modify (.insert sample out)

result = State.run Map.empty helper
```

This design could be considered both a problem as well as a feature. The need to
refactor shows the preferred, modular way to write the code. Arguably such
limitation could be considered too severe, especially in simpler examples like:

```haskell
helper =
   print "I've got a new number!"
   print "I'm happy now."

100.times helper
```

### Proposed solution <!-- omit in toc -->

We are not convinced that code blocks should become a first class citizen.
Definitely more real-life examples are needed to judge if Enso will benefit from
their introduction or they will contribute to lower code quality instead. Deeply
nested code blocks are almost never a good idea and keeping them as separate
functions makes the code more modular and structured. If the language supports
an easy to use syntax for creating such helper functions the problem is almost
non-existent. Moreover, the code blocks need to be supported by the visual
environment and while functions are just nodes, unnamed code blocks are a very
strange entity, which the visual language would not benefit from.

In case the results will be positive and we will need to introduce them, a
possible solution would be to introduce a `do` keyword, which will open a new
code block. The keyword will be optional after `=` and `->` operators. The above
examples could be then rewritten to:

```haskell
result = State.run Map.empty do
    samples.each sample->
        print "Sample `sample`"
        out = runSimulation sample
        State.modify (.insert sample out)
```

```haskell
100.times do
   print "I've got a new number!"
   print "I'm happy now."
```

## TODO:

- extension methods (including imports to local scope and local scope
  overriding)
- strictness
- postponing computations
- dynamic types
- unnamed multiline types

<!--
## Overview of the proposed design

Let's discuss the proposed design based on a comparison with the old one:

Sample code using the current syntax design:

```haskell
01 | def inc :: a -> a + 1
02 | def inc a: a + 1
03 |
04 | def main :: Nothing in IO
05 | def main:
06 |     file = "config.yaml"
07 |     cfg  = open file . parse Config . catch error:
08 |         log.debug "Cannot open `file`: `error`"
09 |         defaultConfig
10 |
11 |     samples = (1 .. cfg.maxSamples) . each random . sort
12 |             . filter (< cfg.maxValue)
13 |
14 |     result = State.run Map.empty { -- impossible construction
15 |         samples.each sample:
16 |             print "Sample `sample`"
17 |             out = runSimulation sample
18 |             State.modify (.insert sample out)
19 |         }
20 |
21 |     print "Results:"
22 |     results.each [k,v]:
23 |         print "`k`: `v`"
```

Sample code using the proposed syntax design:

```haskell
01 | inc : a -> a + 1
02 | inc = a -> a + 1
03 | inc a = a + 1 -- sugar
04 |
05 | main : Nothing in IO
06 | main =
07 |     file = "config.yaml"
08 |     cfg  = open file . parse Config . catch error->
09 |         log.debug "Cannot open `file`: `error`"
10 |         defaultConfig
11 |
12 |     samples = (1 .. cfg.maxSamples) . each random . sort
13 |             . filter (< cfg.maxValue)
14 |
15 |     result = State.run Map.empty do
16 |         samples.each sample->
17 |             print "Sample `sample`"
18 |             out = runSimulation sample
19 |             State.modify (.insert sample out)
20 |
21 |     print "Results:"
22 |     results.each [k,v]->
23 |         print "`k`: `v`"
​``` -->

## Implementation notes

Types are modules, but in fact they are just created with pipe operator. We need
to access them at runtime. It seems like it is just a structure with set of
sub-types, methods etc. To be refined and described here including the dynamic,
runtime representation.

<!--

### Types as Generics
To start off, types in Enso can be made _generic_ over other types. This can be
as simple as `Map k v`, where `k` is the type of the key and `v` is the type of
the value, but actually has more sophisticated use-cases.

One of these is to be able to parametrise your types over different
implementations of functionality, such as strings. This means that your type can
be generic over anything that implements the required interface, as shown in the
following example:

in `util/logger.luna`:

```
type logger (a : textual) =
    ...
```

in `main.luna`:

```
import data.text
import util.logger text

...
```
This means that in the scope of `main.luna`, any instance of the `logger` type
will use `text` as its underlying implementation. This works because `text` is
an instance of the `textual` interface.

Another useful extension of this is that type arguments to generic types can be
partially applied via currying. If, for example, we have a `map k v`, we can
produce a `stringMap = map string` just by applying one of the type
arguments. This is equivalent to explicit specification of the free type
variable: `stringMap v = map string v`.






It is possible, then, to dynamically determine a map
implementation to use based on runtime data. For example:

```
efficientMap k v =
    if expectedBuckets > threshold
    then hashMap k v
    else treeMap k v

myMap = empty : efficientMap k v
``` -->

### To be done

- Discuss the look and feel of type constraints (`type (n : Nat) => Vector n a`)
- Describe the "GADT" style

# Unresolved Questions

This section should address any unresolved questions you have with the RFC at
the current time. Some examples include:

- We definitely need further discussion on the situation with constructors and
  pattern matching.
- We definitely need further discussion on nested types.
- Some syntax can likely be cleaned up.

# Appendix

## Choosing the right syntax

In order to choose the best syntactic rules, let's analyse how they are
connected with each other and how particular choices affect the rest of the
syntax. The following syntax elements are very tightly connected and thus we
will be checking how our choices affect each of them separately.

1. **Function definition** Function definitions need to be easy to use and
   consistent with all other choices.

   ```haskell
   foo : Int -> Int -> Int
   foo a b: a + b
   ```

```

```

2. **Irrefutable pattern matches** We need to be able to deconstruct objects
   in-line, without full case expressions.

   ```haskell
   Point a           = t -- set type pattern matching
   Point.Point x y z = t -- constructor pattern matching
   ```

````

3. **Value creation**
   We need to be able to easily distinguish constructor and type names

   ```haskell
   t = Point.Point 1 2 3 : Point.Point 1 2 3
   ```

4. **Function signatures**
   We need a way to define nice and concise function signatures. Especially we need a clear way to define a super-type of a function in a simple and readable way.

   ```haskell
    type MyType = Int | Text

    foo : MyType -> MyType -> MyType
    foo : (x : MyType) -> (y : MyType) -> (x + y : MyType)
    foo x y = x + y

    bar : Point a -> Point a -> Point a
    bar p1 p2 = p1 + p2
   ```
````
