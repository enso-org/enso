---
layout: developer-doc
title: Enso's Philosophy
category: summary
tags: [summary, design]
order: 1
---

# Enso: Simplicity and Correctness

Enso is an award-winning, general-purpose, purely functional programming
language with equivalent, first-class visual and textual representations and a
highly expressive and novel type system.

Enso has been designed from the ground up to be easy to use for people of all
experience levels, providing power to those that want it while keeping it hidden
from those that do not. Its type rich type system helps users to catch their
mistakes before they happen, which combines with its performant runtime to
drastically improve the quality of the end result. Enso is very much intended to
be a production language (rather than a research project), and the user-focused
development philosophy reflects that.

From a technical point of view, Enso incorporates many of the recent innovations
in the design and development of programming languages to improve the user
experience. It provides higher-order functions, strict semantics with opt-in
laziness, and opt-in algebraic data types, all under the auspices of a novel
type system that merges gradual typing with a great user experience. Enso is the
culmination of many years of research into functional programming languages,
consolidating the work trail-blazed by Haskell, Idris and Agda, but also
improvements in user-experience and ergonomics.

All in all, Enso is a programming language unlike any other before it, a
seamless blend of code and visual communication that can span organisations.

> "The compiler is your assistant, and you interact with it to arrive at a
> working program via a conversation, with information going both ways."
>
> – [Edwin Brady](https://twitter.com/edwinbrady/status/1115361451005423617)

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Why a New Programming Language?](#why-a-new-programming-language)
- [Software Correctness Matters](#software-correctness-matters)
- [Connections to your Data](#connections-to-your-data)
- [Tenets of Enso](#tenets-of-enso)
  - [Visual and Textual](#visual-and-textual)
  - [Unified Syntax](#unified-syntax)
  - [Visual Communication](#visual-communication)
  - [One Right Way](#one-right-way)
  - [Help the User](#help-the-user)
  - [Simple Complexity](#simple-complexity)
  - [Powerful When Necessary](#powerful-when-necessary)
  - [Performance and Predictability](#performance-and-predictability)
  - [Explicit Over Implicit](#explicit-over-implicit)
  - [Guidance Over On-Boarding](#guidance-over-on-boarding)
- [Designing Enso](#designing-enso)

<!-- /MarkdownTOC -->

## Why a New Programming Language?

Since the 1980s, the way that programmers work and the tools that they use have
changed remarkably little. However, there is a small but growing chorus that
worries that the status quo is unsustainable: programmers are having problems
keeping up with their own creations. "Even good programmers are struggling to
make sense of the systems that they are working with," says Chris Granger, a
software developer who worked as a lead at Microsoft on Visual Studio.

We believe, without a drastic change to how software is created, that humanity's
progress is going to be stymied. However, in contrast to many approaches to
finding the next-generation interfaces for human-computer interaction, we
believe that textual code should not be replaced, but instead _enhanced_. In the
same way that writing and speaking coexist, there are times where code is more
convenient than any other approach.

## Software Correctness Matters

"In September 2007, Jean Bookout was driving on the highway with her best friend
in a Toyota Camry when the accelerator seemed to get stuck. When she took her
foot off the pedal, the car didn't slow down. She tried the brakes but they
seemed to have lost their power. As she swerved toward an off-ramp going 50
miles per hour, she pulled the emergency brake. The car left a skid mark 150
feet long before running into an embankment by the side of the road. The
passenger was killed. Bookout woke up in a hospital a month later.

"The incident was one of many in a nearly decade-long investigation into claims
of so-called unintended acceleration in Toyota cars. Toyota blamed the incidents
on poorly designed floor mats, “sticky” pedals, and driver error, but outsiders
suspected that faulty software might be responsible. The National Highway
Traffic Safety Administration enlisted software experts from NASA to perform an
intensive review of Toyota’s code. After nearly 10 months, the NASA team hadn't
found evidence that software was the cause—but said they couldn't prove it
wasn't.

"It was during litigation of the Bookout accident that someone finally found a
convincing connection. Michael Barr, an expert witness for the plaintiff, had a
team of software experts spend 18 months with the Toyota code, picking up where
NASA left off. Using the same model as the Camry involved in the accident,
Barr’s team demonstrated that there were more than 10 million ways for key tasks
on the on-board computer to fail, potentially leading to unintended
acceleration. They showed that as little as a single bit flip could make a car
run out of control."

The above text is part of an amazing article
[The Coming Software Apocalypse](https://www.theatlantic.com/technology/archive/2017/09/saving-the-world-from-code/540393/)
by James Somers, we strongly encourage you to read it all in order to understand
the reasoning behind much of Enso's design principles.

We believe that everyone should be able to process data and create software.
Thus, we strongly disagree with the assumption that developers should learn how
to formally prove properties about their programs, as it requires a very rare
theoretical background. We believe that it's the responsibility of the language
and its tooling to prove the correctness of the users' creation. _“Human
intuition is poor at estimating the true probability of supposedly ‘extremely
rare’ combinations of events in systems operating at a scale of millions of
requests per second. That human fallibility means that some of the more subtle,
dangerous bugs turn out to be errors in design; the code faithfully implements
the intended design, but the design fails to correctly handle a particular
‘rare’ scenario.”_, wrote Chris Newcombe, who was a leader on Amazon Web
Services team and one of Steam's creators. Enso was designed to prove the
correctness and provide as much valuable information to the user as possible in
as automated and intuitive a fashion as possible.

## Connections to your Data

Software creation is a very creative process. However, while using conventional
languages, programmers are like chess players trying to play with a blindfold on

- so much of their mental energy is spent just trying to picture where the
  pieces are that there’s hardly any left over to think about the game itself.

Enso was designed around the idea that _people need an immediate connection to
what they are making_, as described by by Brett Victor in his amazing talk
[Inventing on Principle](https://vimeo.com/36579366). Any violation of this
principle alienates users from the actual problems they are trying to solve,
which consequently decreases their understanding and increases the number of
mistakes they make.

Enso's visual representation targets domains where data processing is the
primary focus, including data science, machine learning, IoT, bioinformatics,
predictive maintenance, computer graphics, sound processing and so on. Each
domain requires a highly tailored toolbox for working with such data, and Enso
provides a coherent and unified foundation for building such toolboxes, on top
of a growing library of existing ones. At its core, Enso delivers a powerful
environment for the modelling of data flows, with extensive inbuilt capabilities
for data visualisation and manipulation.

## Tenets of Enso

The design of a tool can have a _drastic_ effect on the
[cognitive load](https://en.wikipedia.org/wiki/Cognitive_load) experienced by
users. As we design Enso, we rely on a small set of principles to guide the
design and evolution of the language, with the aim of making it easier to both
express thoughts and understand existing logic.

Enso's design is based on a small set of principles that all focus on minimising
the cognitive load for users. They are elucidated below

- **Visual and Textual:** The visual and textual syntaxes are both first-class.
  One is just as a canonical representation of the program as the other. New
  features must work across both.
- **Unified Syntax:** The language syntax should be simple, concise, and be
  usable on both the type and term levels.
- **Visual Communication:** A pure and functional language that lends itself
  easily to visualisation of data-flows.
- **One Right Way:** There should, overwhelmingly, be only one way to perform a
  given task. Limited choice breeds simplicity.
- **Help the User:** Enso should do its utmost to make things easier for the
  user, even if that involves accepting additional complexity in the
  implementation. This does not come at the exclusion of letting users access
  that power.
- **Simple Complexity:** Though the language is backed by powerful functionality
  and compiler smarts, but this should be invisible to the users until they care
  to engage with it.
- **Powerful When Necessary:** Designed to employ powerful techniques that
  confer safety and speed, allowing the users to write correct programs.
- **Performance and Predictability:** Predictable performance, with integrated
  debugging and profiling features to help users diagnose their problems.
- **Explicit Over Implicit:** The design of Enso and its libraries should
  _always_ account for making all behaviour explicit.
- **Guidance Over On-Boarding:** While an on-boarding process can help new users
  get up to speed, the language itself should endeavour to be as helpful as
  possible.

### Visual and Textual

While Enso _is_ a programming language, the visual environment with which it
ships is just as important. This means that when designing the language, and any
new feature for the language, we need to explicitly account for how it works in
both the visual and textual syntaxes.

- **Necessity:** Is this functionality necessary in both environments, or is it
  compensating for a deficiency in one environment.
- **Simplicity:** Features should seem uniform between the two environments as
  much as possible.
- **Portability:** One environment should not expose any features not exposed by
  the other.

Both syntaxes are of _equal_ importance when thinking about designing Enso, and
so any functionality from one syntax that doesn't fit in the other should be
rejected.

### Unified Syntax

With the syntax being the primary mode of interaction between users and Enso,
whether it be visual or textual, it is important that it is consistent within
itself. This is quite a nebulous concept, but it boils down to a few main
factors:

- The syntax should have a minimal number of constructs in it.
- The syntax should use a coherent visual language in both its forms.
- The type-level syntax should equal the value-level syntax, allowing a natural
  blend of programming with both types and terms.
- The behaviour of the program should be immediately evident from the syntax
  (and so the syntax should not embody any context-sensitive behaviour).
- The language syntax should be amenable to providing clear and descriptive
  diagnostics to users.
- Special cases in the syntax should be avoided

A key point of design for Enso's syntax is the notion that syntactic rules need
to be remembered, so it is incredibly important to find a happy middle ground
between _too many_ and _not enough_ syntactic constructs.

### Visual Communication

Humans are inherently visual creatures, and Enso is designed to enable a
_visual_ style of communication. When designing new language features, it is
overwhelmingly important to consider their impact on Enso's visualisation
functionality.

- _How_ they impact the existing visualisation capabilities.
- _How_ they can be visualised themselves.

### One Right Way

Choice in a language inherently creates myriad ways to do things. This is
singularly unhelpful in any programming language, let alone one intended to span
all experience levels, as it increases the cognitive burden of using the
language.

To that end, it is significantly important to not introduce elements to the
design that can already be achieved another way, or that can be achieved simply
through a combination of existing features.

However, there are sometimes cases where the duplication of functionality may be
introduced while providing a significant ergonomic benefit to users. Under such
circumstances, the trade-offs of the design decision must be very carefully
weighted, and a decision made based on the balance between complexity and
ergonomics.

### Help the User

Features with significant usability benefits for users should be weighted very
highly in Enso's design. This is true _even_ when adding such a feature may
bring significant complexity to the implementation of the language or tooling,
as the user experience is paramount.

That is not to say, however, that _any and all_ complexity should be accepted in
the aid of user experience. If the maintenance burden is too great, then it is
likely that introducing such a photo will impact future UX improvements. To this
end, it is always a careful balancing act, even if we tilt the scales heavily in
favour of UX over simplicity of the implementation.

### Simple Complexity

A big part of Enso's design is the notion of employing sophisticated and
cutting-edge techniques in programming language design. However, many languages
that employ these techniques do so to the _detriment_ of the user experience.

While this increased power may allow the system to guarantee more things, or
perform better, it often means that the user has to contend with additional
complexity. A huge part of the design of Enso is hiding this complexity from the
user as much as is possible, ensuring that the focus of the language design is
on a good user experience.

Nevertheless, we do not intend to _bar_ users from this complexity. As much as
Enso should provide a good user experience, it should also not stand in the way
of power users.

### Powerful When Necessary

While we want to provide a simple tool to our users, this should not induce the
language to shy away from necessary power.

Sometimes, the expression of an idea in its most succinct requires powerful
language constructs. Given that Enso doesn't want to limit the ability of its
users to express things, it needs to provide this power. However, this power
should be opt-in, with users not wanting the enhancements to expressiveness not
having to know or care about it.

### Performance and Predictability

There is little that is more frustrating than the code you write running slowly
for no explicable reason.

To that end, Enso aims not only to be _fast_, meaning that users will not have
to rely on other languages or tools for performance, but it also aims to be
_predictable_. With Enso, 'predictable' means that it should be easy for
advanced users to reason about the performance of their code. As a result, we
aim for a sensible mapping from source code to machine execution, and aim to
ensure that seemingly innocuous changes do not impact Enso's performance.

### Explicit Over Implicit

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

```ruby
type File.Mode :
    type Read
    type Write
    type RW
    type Append
    type RWA

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

### Guidance Over On-Boarding

While it is important to have an on-boarding process to help bring new users up
to speed, that process cannot be the only guidance that users of Enso get.

To this end, it is _overwhelmingly_ important that the language is helpful and
informative as much as is possible. This means things like the provision of
intelligent suggestions to users based on the kinds of program they are writing.
It means things like helpful and informative error messages that explain
problems to the user in an intuitive fashion whilst not hiding the details.

In essence, it means designing for the _enduring_ user experience of Enso,
rather than purely the initial user experience.

## Designing Enso

As Enso's design and functionality evolves, we have to take the utmost care to
ensure that it doesn't balloon beyond control. As a result, every new feature
that we contemplate adding to the language should advance these core tenets, and
thereby ensure that it matches with the overall vision for Enso.

We stick to the above principles when we're _building_ the compiler as well,
with code being liberally commented with need-to-know information, as well as
for clean and clear design documents to accompany it.

You can find out more about the various components that make up Enso by having a
read of the design documents listed below. These documents are highly technical,
and are not intended to be user-facing documentation.

- **[Semantics](semantics/):** A description of Enso's semantics, with a focus
  on both the main principles and the details.
- **[Syntax](syntax/):** A description of Enso's syntax, with a focus on both
  the main principles behind its design and its nitty gritty details.
- **[The Type System](types/):** A description of Enso's type system, starting
  at a very high level and slowly getting into more detail as the design
  evolves.
- **[The Runtime](runtime/):** A description of the design for the runtime that
  is evolving in tandem with the runtime's development.
