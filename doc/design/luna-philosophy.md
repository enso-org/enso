# Luna's Philosophy
Luna is a programming language unlike any that have come before it, a seamless
blend of code and visual communication that can span organisations. 

> "The compiler is your assistant, and you interact with it to arrive at a 
> working program via a conversation, with information going both ways."
> 
> – [Edwin Brady](https://twitter.com/edwinbrady/status/1115361451005423617)

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Tenets of Luna](#tenets-of-luna)
  - [Explicit Overrides Implicit](#explicit-overrides-implicit)
- [Designing Luna](#designing-luna)

<!-- /MarkdownTOC -->

## Tenets of Luna
As we design Luna, we rely on a small set of principles to guide the design and
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
- **Simple Complexity:** Though the language is backed by powerful functionality
  and compiler smarts, this should be invisible to the users until they care to
  engage with it.
- **Powerful When Necessary:** Designed to employ powerful techniques that
  confer safety and speed, allowing the users to write correct programs.
- **Performance and Predictability:** Predictable performance, with integrated
  debugging and profiling features to help users diagnose their problems.
- **Explicit Overrides Implicit:** The design of Luna and its libraries should
  _always_ account for making all behaviour implicit.

### Explicit Overrides Implicit
When designing Luna and its libraries, we don't want to have any behaviour of a
function that is not recorded in its type, or its defaults. This gives rise to
two main principles for designing Luna's APIs:

1. Use the correct types to inform both users and the tools about the behaviour
   of the function.
2. Use the inbuilt capabilities for named and default arguments to provide 
   _sensible defaults_, for an API, without hiding behaviour.

<!-- 

Change this example. No longer relevant in the case of `convert`   

A canonical example of this trade-off, based on a real discussion within the 
team, is the behaviour of the `readFile` function when passed a `File.Path` that
contains an environment variable: "$FOO/data/". 

In a case like this, the sensible default for data-analysis tools is to have it
resolve the environment variable, but this default is made explicit by inclusion
as a defaulted keyword argument to the function:

```
readFile : File.Path -> Bool -> File.Handle
readFile = filePath -> expandEnvironmentVariables = True -> ...
```

In doing so, the design of the function tells the user the following things:

- It takes a file path, which represents a location of a file (whether that be
  on the user's local system, or elsewhere).
- It has an _explicit_ default behaviour that expands environment variables that
  is defaulted as part of the definition, but can be overridden if necessary. 

As a result, this design allows for _explicit_ communication of the behaviour of
the function, both under default, and non-default circumstances. Hence, the 
user would be able to do `openFile myPath False`, or even 
`openFile myPath (expandEnvironmentVariables = False)`.

-->

## Designing Luna
As Luna's design and functionality evolves, we have to take the utmost care to
ensure that it doesn't balloon beyond control. As a result, every new feature 
that we contemplate adding to the language should advance these core tenets, and
thereby ensure that it matches with the overall vision for Luna. 
