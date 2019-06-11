# Enso's Philosophy
Enso is a programming language unlike any that have come before it, a seamless
blend of code and visual communication that can span organisations.

> "The compiler is your assistant, and you interact with it to arrive at a
> working program via a conversation, with information going both ways."
>
> – [Edwin Brady](https://twitter.com/edwinbrady/status/1115361451005423617)

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Tenets of Enso](#tenets-of-enso)
  - [Explicit Overrides Implicit](#explicit-overrides-implicit)
- [Designing Enso](#designing-enso)

<!-- /MarkdownTOC -->

## Tenets of Enso
As we design Enso, we rely on a small set of principles to guide the design and
evolution of the language. They are elucidated below.

- **Visual and Textual:** The visual and textual syntaxes are both first-class.
  One is not translated to the other, but they are instead equivalent. New
  features must work across both.
- **Unified Syntax:** The language syntax should be simple, concise, and be
  usable at both the type and term levels.
- **Visual Communication:** A pure and functional language that lends itself
  easily to visualisation of data-flows.
- **One Right Way:** There should, overwhelmingly, be only one way to perform a
  given task. Limited choice breeds simplicity.
- **Help the User:** Enso should do its utmost to make things easier for the
  user, even if that involves accepting additional tooling complexity. This does
  not come at the exclusion of letting users access that power.
- **Simple Complexity:** Though the language is backed by powerful functionality
  and compiler smarts, this should be invisible to the users until they care to
  engage with it.
- **Powerful When Necessary:** Designed to employ powerful techniques that
  confer safety and speed, allowing the users to write correct programs.
- **Performance and Predictability:** Predictable performance, with integrated
  debugging and profiling features to help users diagnose their problems.
- **Explicit Overrides Implicit:** The design of Enso and its libraries should
  _always_ account for making all behaviour implicit.

### Explicit Overrides Implicit
When designing Enso and its libraries, we don't want to have any behaviour of a
function that is not recorded in its type, or its defaults. This gives rise to
two main principles for designing Enso's APIs:

1. Use the correct types to inform both users and the tools about the behaviour
   of the function.
2. Use the inbuilt capabilities for named and default arguments to provide
   _sensible defaults_, for an API, without hiding behaviour.

An example of this trade-off is reading a opening a file handle with
`openHandle`.

Unlike more specialised functions such as `readFile` and `writeFile`,
`openHandle` is much more flexible about how it opens the file. In such cases,
users making use of this file function can generally be assumed to want to open
the file for both reading _and_ writing.

This is the sensible, default, but it is made properly explicit by inclusion as
a defaulted keyword argument to the function:

```
type File.Mode :
    Read
    Write
    RW
    Append
    RWA

openHandle : File.Path -> File.Mode -> File.Handle
openHandle path -> fileMode = RW -> ...
```

In doing so, the design of the function tells the user the following things:

- It takes a file path, which represents the location of a file on the user's
  local system (as opposed to a `Resource.Path`, which is a location for a
  generic resource).
- It has an _explicit_ default behaviour that it opens the file for both reading
  and writing, that is defaulted as part of the definition, but can be
  overridden if necessary.

As a result, this design allows for _explicit_ communication of the behaviour of
the function, both under default, and non-default circumstances. Hence, the user
who would like to open a file for appending (via `appendHandle`), as well as
reading and writing, can call it as follows: `openHandle path (fileMode = RWA)`,
or just `openHandle path RWA`.

## Designing Enso
As Enso's design and functionality evolves, we have to take the utmost care to
ensure that it doesn't balloon beyond control. As a result, every new feature
that we contemplate adding to the language should advance these core tenets, and
thereby ensure that it matches with the overall vision for Enso.

We stick to the above principles when we're _building_ the compiler as well, 
with code being liberally commented with need-to-know information, as well as 
for clean and clear design documents to accompany it. 
