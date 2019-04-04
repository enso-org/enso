NOT WORKING!!!



In Haskell we've got positional type variables and thus we can traverse
X (Y (Z Int)) using recursive type classes. How to do it in Luna? Some Generics?


type Vector x y z


vector a = Vector a a a


v = Vector 1 2 3 : vector int

ivector = Vector int int int


case v of
   ivector -> ... # WRONG
   a -> ... 
   



-----------------------

type Vector x y z

vector a = Vector a a a

VectorInt = Vector Int Int Int


v = Vector 1 2 3 : vector Int


case v of 
    Vector x y z -> ... 
    vector a     -> ...


-----------------------




type Rectangle width height
type Circle    radius

primitive a = Rectangle a a | Circle a a

Rectangle.area = self.width * self.height
Circle.area    = self.radius.pow 2 * Math.pi 



primitive a = type

    type Dim

    Rectangle 
        width  : a 
        height : a

    Circle
        radius : a

    area = case self
        Rectangle w h -> w * h
        Circle    r   -> Math.pi * self.radius.pow 2
    



f {x=5, y=10}

f (type x=5 y=10)



type Math
    type Vector


Math.Vector

Vector Math



foo : a -> a.item




v = Math.Vector 




type Vector x y z
vector = Vector Int Int Int

Vector x y z = foo bar
vector       = foo bar


type Vector
    type V3
        x : Int
        y : Int
        z : Int


case foo bar of
    Vector            -> ...
    Vector.Cons x y z -> ...


t = Vector.Cons x y z




vector = a -> type 
    type Vector
        x : a 
        y : a 
        z : a
    


type Vector x y z
vector = a -> Vector a a a

(vector a).length = ...


-------------------

komentarze: 

foo #= a -> b ->
    ...

foo #= bar 100 50

foo #= bar.baz 11



aplikacje argumentow:

foo 1 2 3

to samo co :

foo <- 1 <- 2 <- 3

w ten sposob mozna nowe linijki kontynuowac:

foo bar baz dlugie rozne argumenty
    <- bax bax2 bax3


----------------------

Currently we have no specification for the function and form of extension 
methods.

-------------------

what is the difference between just types and types - modules ? 
types mean set of constructors and they are also interfaces that their elements
have to match, so:

```haskell

type Math 

    type Int
        ...

    foo : ...
```

Here, `foo` is meant to be module function, `Int` should not have it. 

Possible solution1.
We assume that a type is:
    - a set of elements 
    - interface the elements have to match 
    - a set of other sets (types), which are independent from the interface

This way: 

```haskell
type Vector
    data V3 
        x : Int
        y : Int
        z : Int
    
    type Internal 
        data InternalData

        foo : ...

    v3test : ...
```

the `v3test` is method of `V3` but NOT of `Internal` nor `InternalData`.


However this model is strange, because normally we've got sets. These sets 
contain elements and other sets which are subsets (!). This model assumes that
sets have reference to completely unrelated sets.

Using other words, it means that `type` is no longer a set of behavior and elements.
It is set of behavior, elements and separate set of "references" to other sets.

It could be actually ok if we think about it this way: the set Math contains some
subsets like Int or Vector. It does NOT have any methods as interface. It provides, 
however, a data `Math.Math` (or whatever its called) which has some associated methods
and they are not descirbed in any interface, especially not in the one containing `Int` etc.
This is sound, but its not what the syntax tells, or it is?


```haskell
data MathCons

MathCons.foo : ...
MathCons.foo = ...


type Int
    ...


type Math
    include MathCons
    include Int

```

What is the correct syntax? How can we define set with subsets and their common interface? 
Do we really want to do it?


----------------------


type Foo
    foo : Int -> Int
    bar : Int -> Int


type Bar 
    foo = a -> a + 1
    bar = a -> a + 1





a = Bar

b = a.foo = a -> a + 2

b : Bar -- NO!

--> w jaki sposob podac w typie zarowno sygnature jak i implementacje tak by bylo
    to podmienialne i by implementacja byla defaultowa ? 



Tak ?
type Foo 
    x : Int = 0

    foo : Int -> Int
        = ... 















___
- **Feature Name:** Syntax Overhaul
- **Start Date:** 2018-06-26
- **Change Type:** Breaking 
- **RFC Dependencies:** 
- **RFC PR:** 
- **Luna Issue:** 
- **Implemented:** 


Table of Contents
=================

<!-- MarkdownTOC Style autolink="true" levels="1,2,3" -->

- [Table of Contents](#table-of-contents)
- [Summary](#summary)
- [Motivation](#motivation)
- [Design Principles](#design-principles)
- [Layout rules](#layout-rules)
- [Naming rules](#naming-rules)
- [Types](#types)
    - [The Type System](#the-type-system)
    - [Types. Unified Classes, Modules and Interfaces](#types-unified-classes-modules-and-interfaces)
    - [Type Signatures](#type-signatures)
- [Functions](#functions)
        - [Function Definition](#function-definition)
        - [Function Type](#function-type)
        - [Function Call](#function-call)
            - [Uniform Function Call Syntax (UFCS)](#uniform-function-call-syntax-ufcs)
            - [Extension Methods](#extension-methods)
        - [Argument Names](#argument-names)
        - [Named and default arguments](#named-and-default-arguments)
        - [Function declaration](#function-declaration)
        - [Named arguments](#named-arguments)
            - [Special unnamed type provider syntax](#special-unnamed-type-provider-syntax)
            - [Special unnamed type provider syntax 2](#special-unnamed-type-provider-syntax-2)
            - [Special arrows](#special-arrows)
        - [Default arguments](#default-arguments)
        - [Anonymous functions](#anonymous-functions)
    - [Types as Classes](#types-as-classes)
        - [Constructors](#constructors)
        - [Methods](#methods)
        - [Constructors as types](#constructors-as-types)
        - [Type combinators](#type-combinators)
        - [Pattern matching](#pattern-matching)
        - [Polymorphism](#polymorphism)
        - [Generalized type definitions](#generalized-type-definitions)
    - [Types as Modules](#types-as-modules)
        - [Files and modules](#files-and-modules)
        - [Module Examples](#module-examples)
    - [Types as Interfaces](#types-as-interfaces)
        - [Implementing Interfaces](#implementing-interfaces)
    - [Imports](#imports)
        - [Scoping Rules and Code Modularity](#scoping-rules-and-code-modularity)
    - [Anonymous Types](#anonymous-types)
        - [Anonymous Types as Types](#anonymous-types-as-types)
        - [Anonymous Types as Values](#anonymous-types-as-values)
    - [Nested Types](#nested-types)
    - [Example - Dependent Vector](#example---dependent-vector)
    - [Example - Linked List](#example---linked-list)
    - [First-class sequential code blocks.](#first-class-sequential-code-blocks)
    - [TODO:](#todo)
    - [Implementation notes](#implementation-notes)
        - [To be done](#to-be-done)
- [Unresolved Questions](#unresolved-questions)
- [Appendix](#appendix)
    - [Choosing the right syntax](#choosing-the-right-syntax)
            - [Solution 1: Capitalized constructors](#solution-1-capitalized-constructors)
            - [Solution 2: Capitalized monomorphic types](#solution-2-capitalized-monomorphic-types)
            - [Solution 2.1. Capitalized functions whose final value is a type](#solution-21-capitalized-functions-whose-final-value-is-a-type)
            - [Solution 2.2. Capitalized monomorphic types, new function syntax](#solution-22-capitalized-monomorphic-types-new-function-syntax)
            - [Solution 3. Capitalized matches](#solution-3-capitalized-matches)

<!-- /MarkdownTOC -->






Summary
=======
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
OCaml, take the concept of a module even further. Luna, with its unique system
for categorical typing, is in a position to provide the most flexible 
implementation of modules yet, unifying the concepts of modules, classes and
interfaces.

This proposal introduces a major breaking change for Luna, wholesale replacing
portions of the Language's syntax and semantics with an entirely new model. As a
result, this RFC aims to describe the whole new design in a form of a
documentation with frequent annotations providing rationale for the changes.





Motivation
==========
The focus of the first Luna syntax was that it played nicely with the language's
graphical representation, but while it was relatively easy to use, we have found
it to be unsatisfactory for the future goals of Luna as a programming language.
Moreover, the current implementation has only a rudimentary module system, a
poor state of affairs for a sophisticated language.

This RFC proposes a wholesale redesign of Luna's syntax. It covers all aspects 
of the language and aims to provide a design that is both unified and 
future-proof. It also addresses the code modularity issue with a _unified_ 
design that combines the notions of modules (in both the conventional and ML
senses), classes, and interfaces under the umbrella of the first-class `type`.

The syntax contained in this proposal supports a variety of diverse use-cases.
It supports the first-class manipulation of types, including the creation of
anonymous types. In doing so, the users of Luna have first-class modularity for
their code, with intuitive mechanisms for working within Luna's type system. 
This unification greatly simplifies Luna as a language, providing one powerful
mechanism that results in many key language features, without requiring more 
from the users than an understanding of `type` and the principles of Luna's type
system.






Design Principles
=================

It is impossible to re-design even small part of the syntax without considering
almost every other design decision. Over the past years we have learned that the
only way which will brings us a step closer to a design that fits well into all
requirements is a design that bases on a small set of well defined invariants.
Invariants derive from a careful analysis of the needs. Their definition should
always be the first step when searching for a solution to a complex problem.
They should be used as a very efficient filter to test new ideas and discovering
bad decisions.

Below we present fundamental assumptions regarding how the Luna language should
look and feel. 

### Invariants <!-- omit in toc -->

1. **The textual syntax must play well with the visual representation.**  
   Both visual and textual representations are equivalently important. Any rule
   which does not fit both worlds at the same time will be rejected.

2. **Easiness in understanding is more important than design minimalism.**  
   Luna is meant to be production, not a research language. It targets a broad
   range of developers and domain experts. Thus it should be fast to write,
   comfortable to read and easy to reason about. In particular, it should
   provide easy to understand compile time errors, which is why for example
   monads in Luna are a special entity handled by the compiler.

3. **There should be one (and preferably only one) way to achieve a goal.**  
   One of the greatest power of a good syntax is that it is easy to read by
   different people from different organizations. The more coding styles or
   design pattern rules users have to learn, the more codebases with different,
   often incompatible approaches will appear. In the ideal world, a language
   would provide one and only one way to write and format code, which would also
   be fast to write and easy to understand by people. Luna design should be
   aligned with this vision.

4. **Type level syntax = value level syntax.**  
   Luna type system is designed to be as expressive and as natural to use as
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
   affect the performance. Again, consider Haskell here. Changing `func2 a =
   func1 a` to `func2 = func1` can affect performance and it makes Haskell
   programs very hard to reason about.






Layout rules
============
 
Luna uses indentation to determine the structure of the code. It also provides a
clear guidance on how a code should be formatted. The layout rules were designed
to be both flexible yet enforce good practices.

There is one _master rule_. Every expression with a non zero indentation is a
sub-structure of the first earlier expression with a smaller indentation.

However, there are few cases regarding how an expression with its subsequent
indented expressions are threated: 

- **Operator on the end of line**  
  If the line ends with an operator and the subsequent line is indented, it is
  considered to be the beginning of a new code block. The most common usage is
  by using the arrow operator to define a new function.
  ```haskell
  test = a -> b -> 
      sum = a + b
      print 'The sum is `sum`'
  ```

- **Operator on the beginning of the subsequent line**  
  If the subsequent line starts with an operator, it is considered to be just a
  continuation of the previous line. The most common usage is by using the dot
  operator to create chained method calls.
  ```haskell
  nums = [1..100]
       . each random
       . sort
       . take 100
  ```

- **Otherwise**  
  If there is no operator on the end of the line nor there is an operator on the
  beginning of the subsequent line, then the indented line is considered to be a
  separate expression, just like it was inside parentheses. The most common
  usage is to provide named arguments.
  ```haskell
  geo = sphere
      radius   = 15
      position = vector 10 0 10
      color    = rgb 0 1 0
  ```


There is sometimes a rare situation when you have a very long expression and you
want to split it across lines and you don't want the subsequent lines to be
threated as separate expressions. In most cases such a situation means that the
code is wrongly structured and you re-structure it to separate method calls.
However, if you're just looking for a dirty hack, you can use an id-operator
(`\`) defined in the standard library, which acts just like a space:

```haskell
test = my very long expression
    \ written in few lines
```






Naming rules
============

### Design <!-- omit in toc -->

Naming convention unifies how code is written by different developers, increases
the immediate understanding and allows to provide compiler with useful
information in a very convenient fashion. In particular, pattern matching
requires a way to distinguish between free variables and already declared ones.
Luna uses a simple naming convention to provide such information:

- All monomorphic types and constructors use capitalized names. 
- All other entities use uncapitalized names. 

Using upper case letters for constructors has important benefits. Whenever you
see an upper case identifier, you know it is a data structure being taken apart
or being constructed, which makes it much easier for a human to see what is
going on in a piece of code. Moreover, while using this convention, construction
and pattern matching is as simple as writing the right name and does not require
any magic from the compiler.

```
consName = upperLetter, {nameChar}
varName  = lowerLetter, {nameChar}
```

### Examples <!-- omit in toc -->
Consider the following example:

```haskell
1: case v of
2:     vector Int   -> ...
3:     vector a     -> ...
4:     Vector x y z -> ...
```

- In line 2, `vector` refers to an existing function, because it is applied with
  a parameter.
- In line 3, `a` is a free variable, because it is not applied to any argument
  and has lowercase name.
- In line 4, `Vector` is a constructor, while `x`, `y` and `z` are free
  variables.



### Current design problems <!-- omit in toc -->
```haskell
-- OLD SYNTAX --

class Point a:
    x, y, z :: a
```

Let's consider the above code written using old Luna syntax. It defines a new
class (a new type) and an implicit constructor with the same name as the type.
Currently all types, including polymorphic ones, start with an upper-case
letter. 

This approach has one major issue, namely accessing constructors is more complex
than it should be. The original idea assumes that we can access the constructor
using qualified name, like `p = Point.Point 1 2 3 :: Point.Point 1 2 3 :: Point
Int`. Other ideas proposed generating smart constructors starting with
lower-case letter, but then monomorphic smart constructors cannot be easily
distinguished from free variables in pattern expressions.

In order to properly solve the problem, let's carefully analyse all needs and
use cases. If we allow polymorphic type names to start with upper-case letter,
then we have to allow for some syntax to create new type sets, like the
following one:

```haskell
-- OLD SYNTAX --

type Foo t = Point t | String
a = Point 1 2 3 :: Foo Int
```

The pipe (`|`) is an ordinary operator used to join type sets. Based on the
`{invariant:4}` the following code have to be correct as well, because we can
refactor every type level expression to a function / variable:

```haskell
-- OLD SYNTAX --

foo t = Point t | String
a = Point 1 2 3 :: foo Int
```

Which just breaks our original assumption. Even worse, it seems that such syntax
allows creating functions named with capitalized first letter. The new type
alias have to accept any valid type level expression, like `type Foo x = if x
then Int else String`, so the following has to be accepted as well:

```haskell
-- OLD SYNTAX --

type Sum a b = a + b
def main: 
    print (Sum 3 4)
```

Which clearly shows a flow in the design.






Types
=====

The Type System
---------------
Luna is a statically typed language. It means that every variable is tagged with
an information about its possible values. Luna's type system bases on the idea
that each type is denoted by a set of values, called `constructors`. Formally,
this makes the type system a [Modular
Lattice](https://en.wikipedia.org/wiki/Modular_lattice). For an example, the
type `Nat` contains constructors `1, 2, 3, ...`, and is hence denotable by a set
of the possible values. 

As a result, typechecking doesn't work via _unification_ as one might expect if
they are familiar with other functional programming languages, but instead 
checks if a given set of values is a valid substitution for another. We can, of
course, have the empty set (∅), and also sets containing single elements 
(single constructors). 

This notion is supported by an enforced equivalence between value-level and
type-level syntax in Luna, as the compiler makes no distinction between the two.
This means that it is perfectly valid to type `Vector 1 2 3 : Vector 1 2 3`, a
set with a single member. The resultant flexibility is very intuitive, and gives
Luna a very useful type-system in practice, allowing a form of strong,
structural typing.



Types. Unified Classes, Modules and Interfaces
----------------------------------------------
Luna unifies the abstraction of classes, modules and interfaces under a single
first-class umbrella. All of the following functionalities are provided by the
`type` keyword, resulting in a highly flexible language construct: 

- **Classes.** Types provide containers for data and associated behavior.
- **Modules.** Types provide namespacing for code and data.
- **Interfaces.** Types provide behavior description required of a type.

At a fundamental level, the definition of a new `type` in Luna is the creation
of a (usually named) category of values described by the data and behavior it
possesses. These are first-class values in Luna, and can be created and 
manipulated at runtime. 


#### Why the `type` name? <!-- omit in toc -->
While it would've been possible to use an existing keyword for this unified
concept, we feel that existing keywords such as `module` and `class` carried too
much baggage from their uses elsewhere. The chosen `type`, however, is very
explicit as it describes exactly what it does in Luna. Furthermore, with the
concept `module = class = interface`, and all are members of the type-universe
`Type`, making it an even more appropriate choice. 



Type Signatures
---------------

Luna allows providing explicit type information by using the colon operator. The
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
Luna would allow to provide this function only with integer numbers now.
However, the provided type does not mention the context of the computations. The
compiler knows that `print` uses the `IO` context, so considering the provided
hint, the final inferred type would be `Int in c1 -> Int in c2 -> Int in IO | c1
| c2`.

It's worth to note that the type operator is just a regular operator with a very
low precedence and it is defined in the standard library.


### Current design problems <!-- omit in toc -->
The current type signature operator `::` is used by small group of languages
(mostly Haskell related), is not used in math and is harder to type than just a
single `:` mark. Currently, the single colon operator `:` is used as lambda
syntax, however a change to the lambda syntax is proposed in this document as 
well.






Functions
=========

### Function Definition

Luna uses the arrow operator to define unnamed functions, often referred to as
lambda expressions. For example, the following code defines a lambda taking two
values and returning their sum.

```haskell 
x -> y -> x + y
```

Lambdas, like any other expression, can be assigned to variables. This way we
can define a named function.

```haskell 
sum = x -> y -> x + y
```

### Function Type

As the function definition is an ordinary variable assignment, you can use the
type expression to provide Luna with an additional information about arguments
and the result types. If no explicit type is provided, Luna infers the most
general type, the function itself. 

```haskell
sum : x -> y -> x + y
sum = x -> y -> x + y
``` 

An explicit type narrows the scope of the function. For example, we can tell
that the function accepts only integer numbers.

```haskell
sum : Int -> Int -> Int
sum = x -> y -> x + y
```

Please note, that you are never required to provide all possible information
about argument types. Every missing information will be automatically inferred.
For example, the above definition does not mention the contexts of arguments and
could be written more explicit as follows. Contexts are covered in later
chapters of this document.

```haskell
sum : Int in c1 -> Int in c2 -> Int in c1 | c2
sum = x -> y -> x + y
```

Every expression, including the pattern expression, can be typed by using the
type operator. The following code is equivalent to the above one, however, using
external type definition is much more readable, so in-place types should be used
for debug purposes only.

```haskell
sum = (x : Int in c1) -> (y : Int in c2) -> (x + y : Int in c1 | c2)
```


### Function Call

Luna uses whitespace to separate function arguments, which allows for very
readable and concise code. The following code defines a function, calls it and
prints it's result to the screen:

```haskell
sum = x -> y -> x + y
result = sum 5 4
print result
```

#### Uniform Function Call Syntax (UFCS)

For the last decades we've been proven that the easiest way for people to thing
about about software is to think in terms of objects that communicate with each
other by using methods. However, the classical Object Oriented approach has many
flaws and its concepts are often hard to be used correctly even by advanced
developers. 

Luna allows you to think in the terms of objects, but the concepts are very
different from the ones you may know from other languages like Java or Python.
The biggest power of the approach is that it is very simple. There is no
inheritance or even a separate formal concept of an object! Luna has data
structures, functions and a very powerful type system and syntax. 

The Uniform Function Call Syntax unifies the ideas behind functions and methods.
Basically, every function can be called using two equivalent syntax
representations and you can consider every function to be a method on the first
argument it accepts. Formally, every expression of form `func a b` is isomorphic
to `a.func b`. For example, the following lines are equivalent:

```haskell
result = sum 4 5
result = 4.sum 5
```

Thus, you can think about `add` both as a function as well as method of numbers.
One important thing to note here is that the dot operator is just like any other
operator in Luna and could be theoretically defined as:

```haskell
(.) a func = func a
```

However, its association rules are very special, but also intuitive. The
expression `a . func b` is parsed as `(a . func) b`, while normal operators are
parsed the other way around, like `a + func b` is parsed as `a + (func b)`.


#### Extension Methods

The first argument of a function is often referred to as the `self` argument and
Luna allows a special syntax for defining functions with the first argument
explicitly typed. The syntax is named `Extension Method` and it's just a
syntactic sugar allowing thinking in terms of extending object possibilities
with a new method. The following definitions are equivalent:

```haskell
sum = (a : Int) -> b -> a + b
Int.sum = a -> b -> a + b
```




### Argument Names

In most cases, function arguments are named 



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




### Design <!-- omit in toc -->
We propose removing the `def` keyword in favor of using the assignment operator
to define both variables as well as functions. Moreover, values defined in a new
type declaration (every non-nested function) will have postponed effects by
default.

This solution is simple, intuitive and provides only one valid syntax for every
use case. To better understand the concepts, please refer to the following code
examples.

```haskell
-- OLD SYNTAX --

def test :: Text in IO
def test: 
    def mkMsg s: s + '!' 
    print 'Whats your name?'
    name = readLine
    print (mkMsg name)
```

```haskell
-- NEW SYNTAX -- 

test : text in IO
test =
    mkMsg s = s + '!' 
    print 'Whats your name?'
    name = readLine
    print (mkMsg name)
```

### Function declaration
#### Current problems <!-- omit in toc -->
The are two problems with the current function definition syntax. The signature
definition starting with the `def` keywords looks awkward and in some situations
there are multiple ways to define the same thing, which leads to confusion and
not intuitive code. Consider the following, valid Luna code:

```haskell
-- OLD SYNTAX --

def foo :: Int
def foo: 15
```

It's completely valid, because its a definition of "function without arguments".
Function without arguments returning a pure value is obviously the same as just
the value, thus the above code could be re-written as:

```haskell
-- OLD SYNTAX --

foo :: Int
foo = 15
```

If both syntaxes are valid, then the following codes are valid as well:

```haskell
-- OLD SYNTAX --

def foo :: Int
foo = 15

foo :: Int
def foo = 15
```

Which clearly shows problems arising from the value definition ambiguity. It's
important to note that this design breaks the {invariant-3}.

It is important to note however, that this situation is true only for pure
computations. Any monadic computation has different semantics when assigned to a
variable or used within a "no-argument function definition". The `=` symbol
evaluates the outer most monad and wraps the result in `Pure` monad, while the
`def` postpones the computation having the same effect like manual postpone
operator (`@`). For example, the following definitions are equivalent:

```haskell
-- OLD SYNTAX --

def foo: print "Hi!"
foo = @ print "Hi!"
```

In order to better understand the evaluation mechanism, please consider the
following examples:

```haskell
-- OLD SYNTAX --

-- This code prints "hello" 3 times:
def main:
    def foo:
        print "hello"
    foo
    foo
    foo

-- This code prints "hello" a single time:
def main:
    foo = print "hello"
    foo
    foo
    foo

-- This code does not print "hello" (the computation is postponed):
def main:
    foo = @ print "hello"
    foo
    foo
    foo
```





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


Let's consider now how sub-typing in the Luna sense works for functions. In
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
Moreover, Luna allows typing an expression using any supertype, for example we
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
Another solution would be to introduce two types of arrows - named `->` and unnamed `=>` (or vice versa), thus the following code would be valid:

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

foo : int -> int -> int
foo x y z = ...


foo : x -> y -> ...
foo = x -> y -> ...

foo : a -> a -> a -> ...
foo = x -> y -> z -> ...

foo : (x : int) -> (y : int) -> (z : int) -> ...
foo x y z = ...


### Default arguments


### Anonymous functions

#### Current problems <!-- omit in toc -->
Consider the following simple function:

```haskell
def foo :: Int -> Int
def foo a = a + 1
```

The problem with the definition is that the type level symbol `->` does not have
any value level counterpart, which breaks the `{invariant:4}`. We can of course
assume that it is a special syntax for a "function signature" constructor, but
then its unnecessarily magical. Moreover, what value could have an expression 
which type would be expressed as `a : a + 1`?


#### Proposed solution <!-- omit in toc -->
We propose unification of the value level lambda syntax `:` and the type level
arrow syntax `->`. It makes the rules much more consistent. To better understand
the concept, please refer to the following examples:

```haskell
foo : a -> b -> a + b
foo = a -> b -> a + b

-- see the new function definition proposal
bar : x -> x + 1
bar x = x + 1
```

There is however one important thing to note here. The `:` symbol association
rules were deeply magical. Only single variable on the left side was considered
the lambda argument. Such design allows for a very fancy code snippets, but on
the other hand is an exception to all other operator rules. After the
unification, the arrow symbol `->` is just an ordinary operator and all the 
standard association rules apply. Thus the following code snippets are 
equivalent:

```haskell
-- OLD SYNTAX --
out = foo x: x + 1

-- NEW SYNTAX --
out = foo (x -> x + 1)
out = foo x-> x + 1 -- no space = strong association 
```

```haskell
-- OLD SYNTAX --
cfg = open file . parse Config . catch error:
    log.debug "Cannot open `file`: `error`"
    defaultConfig

-- NEW SYNTAX --
cfg = open file . parse Config . catch error->
    log.debug "Cannot open `file`: `error`"
    defaultConfig
```




Types as Classes
----------------
The following chapter describes the replacement for the currently used concept
of _classes_. We have been always dreaming about true dependent typed language
and the way classes currently work stands on the way to achieve the dreams. The
change is, however, not as drastic as it seems. It is rather a process of
extending the current model to provide more fine grained control over the
objects and types.

Luna is an Object Oriented programming language. It provides the notion of
objects and methods so at first glance, Luna types may seem like conventional
_classes_ from traditional object-oriented languages. However, these concepts
differ significantly. Luna types have much more power, yet much simpler design,
disallowing concepts like inheritance in favour of composition and algebraic
data types.

### Constructors
While types in Luna describe categories of values, the constructors are the
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
As Luna is a dependently-typed language with no distinction between value- and
type-level syntax, we are allowed to write _very_ specific type for a given
value. As described earlier, constructors are the values belonging to categories
defined by Luna types. However, they are not only members of categories, they
are also useful to describe very specific categories per se. Formally,
a constructor is capable of describing any subset of the set of all possible
values of its fields. 

For example, the `True` constructor could be used to describe the set of all
possible values of its fields. While it does not have any fields, the set
contains only two value, the `True` constructor itself and an `undefined` value.
Thus it is correct to write in Luna `True : True` and assume that the only
possible values of a variable typed as `a : True` are either `True` or
`undefined`.

On the other hand, The `Point` constructor do contain fields, thus it could be
used for example to describe all possible points, whose first coordinate is an
integral number, while the second and third coordinates are equal to zero: `a :
Point int 0 0`. 


### Type combinators 
The careful reader will notice here, that `int` is a category of all possible
integral numbers, while the numbers are considered constructors themselves. Luna
provides an operator used to join types together, the so called pipe operator.
The hypothetical `int` definition could look like `int = .. | -1 | 0 | 1 | ...`.
We can use this mechanism to easily express even complex type dependencies. For
example we can tell Luna that a particular value has the type of `int | text`.
Luna will allow us to either use pattern matching to discover at runtime which
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
that the Luna compiler has enough information to perform pattern matching during
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
different types. Luna does not provide any special construction to support
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
Luna. It defines all such sets of points, whose all components belong to the
provided type. To better understand this relation, please consider the following valid expressions:

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
This is the reason why Luna provides a syntactic sugar allowing to define
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
information about the value of `check`? The answer is of course `px : (Point 1 2
3 | Point 4 5 6)`, which is a sub type of the type `Point (1|4) (2|5) (3|6)`. 




Types as Modules
----------------
The same notion of a type can be used to provide the functionality that is
traditionally expected of a _module_ (in the common, not ML sense). In most
programming languages, their module system provides a mechanism for code-reuse
through grouping and namespacing. Indeed, Luna's types provide both of these functionalities:

- **Grouping of Code**  
  A `type` declaration acts as a container for code, with functions able to be
  declared in its scope. 
- **Namespacing**  
  Unless otherwise declared (through a direct import statement), a `type` in
  Luna also provides a namespace to constructs declared inside its scope.


### Files and modules
Files in Luna should contain at least one `type` definition, with one type named
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



Types as Interfaces
-------------------
A type in Luna can also act as a 'contract', a specification of the behavior 
expected of a type. The use of types as interfaces in Luna is, as you might 
expect, contravariant. As long as the type satisfies the category defined by
the interface, it can be used in its place. This leads to the expected semantics
where a type `Foo` implementing `Bar` can be used where a `Bar` is expected.

Interfaces in Luna can range from general to very specific. As they define a 
_category_ of values, interfaces can specify anything from function signatures
that must be present, all the way to names that must be present in the type's 
scope and default behavior. The following are all valid ways to define types
for use as interfaces in Luna.

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

The nature of Luna's type system means that any type that _satisfies_ an 
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
future iterations of the Luna compiler. 

It should also be noted that it is not possible to implement orphan instances of
interfaces in Luna, as it leads to difficult to understand code. This means that
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

<!-- #### On the Semantics of Standalone Implementations
Standalone implementations allow for limited extension methods on types. The
interface methods implemented for a type in the standalone definition can be 
used like any other method on a Luna type. 

#### Overlapping Interface Implementations
Sometimes it is beneficial to allow interfaces to overlap in one or more of 
their type parameters. This does not mean Luna allows _duplicate_ instances (
where all of the type parameters are identical). These can be implemented by
either of the methods above, but the user may often run into issues when 
attempting to make use of these interfaces.

Luna thus provides a mechanism for the programmer to manually specify which 
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
To go along with the new system proposed in this RFC around code modularity, 
the syntax for dealing with imports has been tweaked slightly. The following
import syntaxes are valid:

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
Files in Luna should contain at least one `type`, with one type named the same
as the file. This `type` is known as the 'primary' type, and it is this type
that is referred to when importing the 'module'. A file `Data/Map.luna` may 
contain `type Map`, `type Helper` and various other types, but the only things
visible outside the file are the primary type and things defined in its scope.
Inside the file, however, everything can be seen, with no need to 
forward-declare. -->

### Scoping Rules and Code Modularity
Imports in Luna can be performed in _any_ scope, and are accessible from the 
scope into which they are imported. This gives rise to a particularly intuitive
way of handling re-exports. 

Consider the following file `Test.luna`. In this file, the imports of `Thing`
and `PrettyPrint` are not visible when `Test.luna` is imported. However, 
`PrettyPrint` and `printer` are made visible from within the scope of `Test`. This means that a user can write `import Test: printer` and have it work. 

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
types are anonymous in that they provide a category of values without applying 
a name to their category, and can be created both as types and as values. 

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
Anonymous types can also be constructed as values using similar syntax. You can provide values directly, which will work in a context where names are not 
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
nest types arbitrarily in Luna. This leads to a very expressive language, but
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

It is currently not possible to express this code in Luna. You cannot pass a
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
Definitely more real-life examples are needed to judge if Luna will benefit from
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
- extension methods (including imports to local scope and local scope overriding)
- default arguments (definition, evaluation, explicite currying)
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
``` -->



Implementation notes
--------------------

Types are modules, but in fact they are just created with pipe operator. We need
to access them at runtime. It seems like it is just a structure with set of 
sub-types, methods etc. To be refined and described here including the dynamic,
runtime representation.






<!-- 

### Types as Generics
To start off, types in Luna can be made _generic_ over other types. This can be
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

1. **Function definition**
   Function definitions need to be easy to use and consistent with all other
   choices.
   ```haskell
   foo : Int -> Int -> Int
   foo a b: a + b
   ```

2. **Irrefutable pattern matches**  
   We need to be able to deconstruct objects in-line, without full case
   expressions.
   ```haskell
   Point a           = t -- set type pattern matching
   Point.Point x y z = t -- constructor pattern matching
   ```

3. **Value creation**  
   We need to be able to easily distinguish constructor and type names
   ```haskell
   t = Point.Point 1 2 3 : Point.Point 1 2 3
   ```

4. **Function signatures**  
   We need a way to define nice and concise function signatures. Especially 
   we need a clear way to define a super-type of a function in a simple and 
   readable way.
   ```haskell
    type MyType = Int | Text

    foo : MyType -> MyType -> MyType
    foo : (x : MyType) -> (y : MyType) -> (x + y : MyType)
    foo x y = x + y

    bar : Point a -> Point a -> Point a
    bar p1 p2 = p1 + p2
    ```


#### Solution 1: Capitalized constructors
The rules are simple. Constructor names are capitalized, while everything else 
is just an expression label and thus uses uncapitalized names.

```haskell
-- 1 --
foo a b : a + b -- OK

-- 2 --
type point a = t -- OK. Just a little bit ugly (rarely used).
Point x y z  = t -- OK.

-- 3 --
t = Point 1 2 3 : point Int -- OK.

-- 4 --
myType = Int | String

foo : type myType -> type myType -> type myType -- Option 1. WRONG. Ugly
foo : myType -> myType -> myType                -- Option 2. WRONG. (a) 
-- (a): Will be OK if free variables could be aliased with names from scope
--      but then it is very error prone.

bar : point a -> point a -> point a -- OK, but Inconsistent with irrefutable 
                                    -- syntax.
```

There is no clear way how to express signatures of a function in a clear and not 
ambiguous way using this syntax.


#### Solution 2: Capitalized monomorphic types
The rules are also simple. All values whose type is `Type` use capitalized
names, while everything else is just an expression label and thus uses
uncapitalized names.

```haskell
-- 1 --
foo a b : a + b -- OK

-- 2 --
type point a = t -- OK, but ugly (rarely used)
Point x y z  = t -- OK.

-- 3 --
t = Point 1 2 3 : point Int -- OK.

-- 4 --
MyType = Int | String

foo : MyType -> MyType -> MyType    -- OK.
bar : point a -> point a -> point a -- OK, but Inconsistent with irrefutable 
                                    -- syntax.
```

This syntax seems ok, however it introduces some inconsistency. We can use clean
form of pattern matching in function signatures, however while defining
irrefutable patterns we need some additional keyword to indicate that it is not
a function definition. 


#### Solution 2.1. Capitalized functions whose final value is a type
All values whose type is either `Type` or is a function with final value of type
`Type` (e.g. `a -> b -> (c : Type)`) is capitalized. This proposition solves all
ambiguity described earlier, but it's not clear if we can easily enforce the
rule. Discovering the final type, especially in polymorphic functions is often
impossible, so it could be possible to define an uncapitalized function, which
would break the rule after passing some values. Moreover this solution
introduces brings back constructor - type names conflict to scope.

```haskell
-- 1 --
foo a b : a + b -- OK

-- 2 --
Point a           = t -- OK
Point.Point x y z = t -- WRONG.

-- 3 --
t = Point.Point 1 2 3 : Point Int -- WRONG. (We revert to constructor naming
                                  -- problem here) 

-- 4 --
MyType = Int | String

foo : MyType -> MyType -> MyType    -- OK.
bar : Point a -> Point a -> Point a -- OK.
```


#### Solution 2.2. Capitalized monomorphic types, new function syntax
This solution is an evolution of solution 2. It makes the whole syntax much more
consistent just by simplifying the semantics of the assignment expression. Until
now, the assignment semantics could be expressed as:

1. Variable assignment (special case of pattern matching)  
   `(uncapitalized identifier) = value`
2. Constructor pattern matching  
   `(capitalized identifier) (args) = value`
3. Set type pattern matching  
   `type (uncapitalized identifier) (args) = value`
4. Function definition  
   `(uncapitalized identifier) (args) = value`

Since Luna allows mixing values and types and we allow using arbitrary
expressions on type level, we either need a form of pattern matching as
described in point 3 or we need to introduce complex naming rules (as described
earlier).

We can simplify all of the above cases to just single one if we remove the
special case of function definition. The syntax `foo a b = ...` was introduced
by Haskell because it was not used by other syntax forms - Haskell allows
pattern matching on constructors (2) or variables (1) only. If we change the
function definition syntax, we can redefine assignment rules just to:

```haskell
pattern = value
```

Let's see how it affects all the constructs.

```haskell
-- 1 --
foo = a -> b -> a + b -- Option 1. OK.
foo = a b => a + b    -- Option 2. OK. (shorter form for multi arg lambda)

-- 2 --
point a     = t -- OK.
Point x y z = t -- OK.

-- 3 -- 
t = Point 1 2 3 : point Int -- OK.

-- 4
MyType = Int | String

foo : MyType -> MyType -> MyType    -- OK.
bar : point a -> point a -> point a -- OK.

-- Defaulting and naming, even the return parameter can be named
myInterfaceFn : (a = 1 : Real) -> (b : MyType) -> (c : MyType) 
```

So far this is the only solution which does not introduce any ambiguity and is
consistent. However it makes function definition a little more verbose, which
could be considered bad thing. On the other hand, function and lambda
definitions are using now the same syntax and are thus even simpler to
understand and learn. Moreover, while testing this solution it appears that the
code is only sometimes slightly longer, while maintaining the same or better
readability. 


#### Solution 3. Capitalized matches
This is a controversial proposal, which assumes that no matter if we use upper
or lower first identifier letter, we are referring to the same value. This way
we can use capitalized names in pattern matches and signatures to disambiguate
which names are free variables. 

```haskell
-- 1 -- 
-- unrelated 

-- 2 ---
Point a           = t -- OK.
Point.Point x y z = t -- WRONG. (Since "point" = "Point")

-- 3 --
t = Point.Point 1 2 3 : Point Int -- WRONG.

-- 4 --
myType = Int | String

foo : MyType -> MyType -> MyType    -- OK.
bar : Point a -> Point a -> Point a -- OK.
bar : Point a -> Point a -> point a -- WRONG. It means the same as ^^^
                                    -- so we've got 2 ways to express the same
                                    -- thing.
```







```haskell

sphere = radius = 1 -> position = Point 0 0 0 -> ... 

sphere (radius = 8) (position = Point 1 2 3)

shape1 = sphere 
    radius   = 8 
    position = Point 1 2 3


-- = Point 0 0 0
-- = 1
```



------------------------------

```
# How might this all work with multiparam interfaces?

type Functor t =
    map : (a -> b) -> (f : t.functor a) -> (g : t.functor b)

# Can be written with a nested type to make things clearer
type Applicative (t : Functor) =
    type f = t.applicative
    
    # Arguments can be named but don't need to be
    pure  : a -> f a
    (<*>) : f (a -> b) -> (g : f a) -> (h : f b)

type Monad (t : Applicative) = 
    type m = t.monad
    
    (>>=) : m a -> (a -> m b) -> m b

type Semigroup t =
    type s = t.semigroup
    
    (<>) : s -> s -> s
        
type Monoid (t : Semigroup) =
    type m = t.monoid
    
    mempty = m
    
type Convertible a b =
    convert : a -> b
    
type Default a =
    def : a

type Vector =
    x : (Monoid, Default)
    y : (Monoid, Default)
    z : (Monoid, Default)
   
    add : Vector -> Vector -> Vector
    add (Vector x y z) (Vector x' y' z') = Vector (x + x') (y + y') (z + z')
   
    # What happens with these names?
    type functor a = Vector a a a

    # Signature not needed here, but can exist for reference
    map : (a -> b) -> self.functor a -> self.functor b
    map f (Vector x y z) = Vector (f a) (f b) (f c)

    type semigroup a = Vector a a a

    (<>) : self.semigroup a -> self.semigroup a -> self.semigroup a
    (<>) v1 v2 = add v1 v2

    type monoid a = Vector a a a

    mempty : self.monoid a
    mempty = Vector def def def

    # How would we express constraints like `Convertible b a, Convertible c a`?
    # convert : (b.convertible a, c.convertible a) => Vector a b c -> [a] ?
    convert : Vector a b c -> [a]
    convert (Vector x y z) = [x, y, z]
    
type Text =
    underlyingByteArray : ByteArray
    length : UInt64

    text : String -> Self
    text str = ... # do stuff

    # Can explicitly ignore type arguments in producing the projection
    type functor _ = Text
    
    map : (CodePoint -> CodePoint) -> self.functor CodePoint -> self.functor CodePoint
    map f tx = ...

    type semigroup _ = Text
    
    (<>) : self.semigroup Void -> self.semigroup Void -> self.semigroup Void
    (<>) tx tx' = concat tx tx'

    type monoid _ = Text
    
    mempty : self.monoid Void
    mempty = ""
    
```

Unanswered questions:

- How should this interact with multiparameter typeclasses
- How do we express constraints on the instances
- How exactly should it work on Text and the like?
