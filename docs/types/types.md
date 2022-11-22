# A note on conventions

Due to two things that feel like 'primes' popping up in the conversation below,
we've chosen to denote one with 'x.type' and the other with 'Code x'.

# `type` declarations and `type` elaboration

We shall have one keyword for defining new types.

```
type Foo x y z
```

This creates an Atom constructor named 'Foo' and allows us to talk about the
named fields `x`, `y`, and `z`.

Arguments to atom constructors can have defaults and explicit type signatures.
However, there are some limitations on type signatures that will be discussed
later.

Nesting type declarations allow the creation of subtyping relationships and
allow us to express a concept like 'data' declarations in Haskell.

```
type List a
  type Nil
  type Cons (head : a) (tail : List a)
```

There is a distinction between terms of a type and the types themselves, one
that it is sometimes beneficial to blur.

Types themselves are effectively represented as atoms and type constructors are
atom constructors as well.

`List` plays multiple roles here. It acts as the type constructor which can be
fed type arguments to build a fully saturated type:

```
Int : Type
List : Type -> Type
List Int : Type
```

Given a type declaration, we desugar it into atoms and atom constructors and
define subtyping relationships in a two-step process.

First, we gather all of the type variables used in its definition and build a
new type with those type variables expressed in sequence. Then if certain
conditions are met (e.g. the two have the exact same shape), we allow punning
between the basic type constructor and this elaborated type constructor.

e.g.

```
type Book author title year
```

is equivalent to defining

```
type Book.type author_type title_type year_type
  type Book (author : author_type) (title : title_type) (year : year_type)
```

whereupon we notice that Book.type and Book are the same shape, and allow
punning between `Book.type` and `Book`.

```
Book "Jane Austen" "Pride and Prejudice" 1832 : Book.type Text Text Int
Book "Jane Austen" "Pride and Prejudice" 1832 : Book Text Text Int
```

The eagle-eyed among you might note that author_type and author are different
field names, so this would actually elaborate out to

```
type Book.type author title year
  type Book (author : author) (title : title) (year : year)
```

where we are careful with lexical scopes to distinguish between the term level
author and the type level author, enabling this pun.

In the List example

```
type List a
  type Nil
  type Cons (head : a) (tail : List a)
```

this elaborates on each of these type declarations and finds a couple of puns:

```
type List.type a = List a
type Nil.type = Nil
```

and one definition that emerges as not a pun:

```
type Cons.type a
  type Cons (head : a) (tail : List a)
```

# static methods and type formation

List serves double duty in the above. It acts as the type constructor for lists,
so that `List Int` is a well formed `Type`, but it also acts as the container
holding its static methods.

There are two forms of static methods that we are interested in in the long
term. We'll classify them as 'normal' static methods and 'explicit' static
methods. For now, we're going to support 'normal' static methods as they are
needed for 'from' conversions. Still, there is an opportunity to allow more
explicit statics later on for some uses involving meta-programming.

```
type Vector a
  type MkVec (unsafe_to_array: Array a)
  static from (that : List x): Vector x = MkVec (Array.from that)
```

this internally elaborates to

```
type Vector a
  type MkVec.type a
    type MkVec (unsafe_to_array: Array a)
  ...
```

with the obvious subtyping relationship

```
MkVec.type a <: Vector a
```

Similarly to the pun story above, when you have a single `type` statement
contained in a parent `type` statement, and the shape aligns, we should be
willing to allow punning between them, eliminating the distinct `MkVec.type`
here. This results in a nice "obvious" usage pattern, where we aren't
distinguishing between several vector types. The type of the module lives in the
type constructor, and the type of its individual constructor that is limited to
only holding arrays.

Using `Vector` itself as the place where the statics live allows for a
convenient usage pattern:

```
Vector.from [1,2,3] : Vector Int
```

where 'Vector' is playing double duty as a type constructor and as the bag of
static methods.

```
Vec ([1,2,3].to Array) : Vector Int
```

Since static methods live in the topmost atom _constructor_, this implies a
constraint on type statements that contain statics. Namely that it has to pun
with its own type. This translates to having no distinct type variables for each
of its arguments. We may choose to relax this in the future, but by being
overstrict right now, we leave room for evolving the language spec rather than
risk users depending on bad behaviors we'd have to change later.

e.g.

```
type Book (author: String) (title: String) (year: Int)
  static whatever = ...
```

elaborates out to

```
type Book.type
  type Book (author: String) (title: String) (year: Int)
  static whatever = ...
```

where 'whatever' is a method on `Book.type`, not `Book`, which is not a pun.
Like java, static methods don't appear as static methods inside subtypes, and,
in our case, terms that inhabit our times. This is a deliberate design choice,
by choosing _not_ to have static methods push down into atom constructors that
build terms in a type for now we retain the option to revisit this later.
There's a tension here between the 'ease of use' of getting list methods off of
a proxy list you happen to have in hand as a 'prototype', and the ability to
allow subtypes to have their own statics later on, which would be useful if we
want to meaningfully subtype non-empty lists and lists, but allow different
conversions 'to' non-empty lists. We don't have to choose now, so we prefer to
leave the option open, until more sophisticated usage scenarios and users force
a choice.

# case objects

If we carefully read the above rules, these rules allow us to construct
module-like objects via types:

```
type Bucket
  static wat : Foo -> Bar
```

which at first seems to not give me any way to construct a term of type Bucket,
but recall it elaborates to

```
type Bucket.type
  type Bucket
  static wat : Foo -> Bar
```

and then puns `Bucket.type = Bucket` so we can talk about Bucket.wat exactly as
if Bucket was a qualified module import. This supplies us with an implementation
semantics for what 'modules' mean. They are just types with no parameters, where
all their top level methods are lifted into static, and where we cut the
subtyping relationship with any types you define inside the module and the
module itself.

# Future growth directions

## Explicit static methods

The static methods get attached to the outermost atom _constructor_ from a type
definition. This means they do not have access to the type arguments themselves.

We could allow for "explicit static methods" that attach to the outermost Atom.
(That is to say the atom constructor + type arguments).

This would allow you to say something like

```
(Vector Int).from Json
```

Where that from conversion would be able to extract the type argument 'a' and
then explicitly invoke _its_ Json conversion method.

A syntax for this would something like

```
type Vector a
  type MkVec (unsafe_to_array : Array a)
  explicit static from (that : Json) = map (x -> x.to this.a) Json.expectArray
```

Notice how this "explicit" static method is accessing the type arguments of
'Vector' through `this`.

Note: This isn't something we should do in general as it requires explicitly
passing type arguments everywhere, and would encumber `from` conversions unduly
if it was the only implementation technique available to us. This would be
analogous to building a type system without any "erasure" if we were forced to
pass explicit types everywhere, and would come at an unacceptable performance
tax.

Returning to the view of case objects and modules above, ML-style functors would
correspond to having 'explicit' statics rather than static methods in the case
object, which carries with it the concerns of whether modules have applicative
or generative semantics. By punting on this for now we don't have to decide such
things, and don't have to pay a universal performance tax, while still leaving
the door to that becoming a feature we build in the future into the convenient
semantic 'dead space' we leave undefined.

## Future Direction: Normalization-by-evaluation for partial evaluation, behavioral reflection, and an improved editor experience

Pedantically, normalization-by-evaluation is an implementation technique
commonly used in dependent type checkers or evaluators where you have a starting
surface syntax (terms) that you compile down into internal semantic domain
objects (values), but where you want the ability to reify _all_ values back into
terms that are in some sense canonical, (e.g. have been reduced to a so-called
"normal" form).

This is made complicated by the fact that normal forms walk underneath
functions, so you need to be able to evaluate functions with arguments they
'don't know yet' to reach this normal form. We call such arguments we don't know
'stuck' or 'neutral' terms. Any attempt to eliminate them (extract information,
apply them, case on them, etc.) builds a bigger neutral term that captures what
you attempted to do to it, and then we convert between these stuck terms back
into full expressions in the surface language.

The magic is in slightly extending the rules of the interpreter we are using to
allow these stuck terms to be passed, and to build up these larger stuck terms
when elimination forms are applied to them.

### Scenarios

#### IDE Usage

One such usage scenario is when dealing with how to show the user the 'result'
of a computation that has not yet been wired up to all of its inputs. Consider a
world where we have a stuck or out of scope 'a', 'b' and 'c'. In this setting
when we go to evaluate

```
foldr (+) 0 [a,b,c]
```

we get the stuck term

```
a + (b + (c + 0))
```

and can show this to users even in the absence of a known a b and c, and then as
they wire those up to concrete inputs, it'll gradually pop in complexity level
by level until they have it fully wired up and they can see the final answer.

#### LINQ

Another direct analogue can be found in LINQ in the dot-net ecosystem. There
Erik Meijer and Anders Hejlsberg allowed Visual Basic and C# lambdas to be
inspected to see their contents, so that LINQ providers could transcode them
into things like SQL FROM or HAVING clauses for SELECT statements where possible
and the like.

This opens the door towards, say, an efficient library for 'Apache spark'
calculations in Enso, where we can marshal lambdas out closer to the data,
rather than have to bring the data to the lambda.

### "Template Enso"

Morally this looks a lot like BER MetaOcaml, where we're deliberately being
"evil" and allowing you to use naked terms in our language through NBE (like
LINQ does above) instead of using an explicit syntax or typed template haskell
'Code' type. This places a stronger burden on users of the advanced reflection
behaviors to get things right, but allows them to express the full array of
LINQ-like options in the language.

The LINQ scenarios are predicated on being able to feed stuck values into
programs and reify the result as a syntax tree for the partially evaluated
result. The meta-programming or "template Enso" scenario is predicated on being
able to reflect that syntax tree back into an Enso term or function.

### Open design questions

This isn't a corner of the design space we need to fully realize at this point,
and there is more room for expansion. There are some open questions we'd like to
answer.

How should we represent the creation and feeding of stuck terms?

One could envision supplying a syntax for

foo :: A -> B neutral "x" foo

which feeds foo a stuck term of type A, and lifts the resulting term of type B
to `Code B`, where it is explicitly known you can manipulate it as a syntax tree
of the partially evaluated result. But this becomes a bit awkward to invoke.

More pragmatically, this is probably built ontop of a richer programming
interface like

```
type Code a
  # has a side-effect of producing a fresh variable
  # morally this lives in a variable supply monad like TH's 'Q'
  static fresh : String -> Name

  type Var (name: Name)
  static map : (a -> b) -> Code a -> Code b

  # other constructors for Code
  type ...

  lower : a
```

or

```
type Code a
  static fresh : String -> Name
  type Var (name: Name)
  static pure : a -> Code a -- this is where the magic happens
  static apply : Code (a -> b) -> Code a -> Code b
  static map (f : a -> b) (self : Code a): Code b = apply (pure f) self
  static join (x : Code (Code a)) : Code a = x.lower
  # other constructors for Code
  type ...

  lower : a
```

where we can start to observe that the behavior of this 'linq' like Code
construct looks like a monad/comonad in Enso due to all the things NBE let us
identify here.

There remain other design options to explore here. Do we limit ourselves to
terms like BER MetaOcaml? Do we allow elaboration to statements and other
complex expressions like template-haskell? None of that needs to be resolved
today.
