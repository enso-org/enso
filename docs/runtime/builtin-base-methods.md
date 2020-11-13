---
layout: developer-doc
title: Builtin Base Methods
category: runtime
tags: [runtime, base, standard, library]
order: 7
---

# Builtin Base Methods

While we strive to implement most of the Enso standard library in Enso itself,
it is necessary for certain methods, particularly ones involving operations out
of reach for the standard language semantics, or primitive system calls, to be
implemented in Java.

To facilitate this process, we have created an annotation-based DSL for easier
generation of such methods' boilerplate.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [DSL Overview](#dsl-overview)
- [`@BuiltinMethod` Annotation](#builtinmethod-annotation)
- [Structural Requirements](#structural-requirements)
- [`Execute` Method Semantics](#execute-method-semantics)
- [Generated Node](#generated-node)

<!-- /MarkdownTOC -->

## DSL Overview

The Enso Base DSL exposes certain annotations that allow to generate boilerplate
transforming standard Truffle nodes into a shape that can easily be exported to
the Base library. This DSL facilitates automatic parsing and typechecking of
method arguments and wrapping the node in an actual `Function` object.

## `@BuiltinMethod` Annotation

The `@BuiltinMethod` annotation is applied to a Truffle `Node` subclass that
should be treated as a builtin method. It takes the following arguments:

1. `type` **String** the name of the type the method will be registered on.
2. `name` **String** the method name.
3. `description` **String** a summary of the method's behavior, for
   documentation purposes.

The annotation will generate a `RootNode` subclass in the same package as the
declaring node, with a name basing on the declaring node's name. The name of the
generated node is the name of the original node, with the `Node` suffix stripped
and the `MethodGen` suffix appended. E.g. `AndOperatorNode` will generate a
`AndOperatorMethodGen` root node.

## Structural Requirements

The DSL places certain structural requirements on the declaring node.

1. **Construction:** The node must be constructible using either a
   paremeterless, static `build` method or a parameterless constructor. If a
   suitable `build` method is defined, it will be preferred over the
   constructor.
2. **Execution:** The node must declare a single, accessible (i.e. at least
   package-private), non-void method named `execute`.

## `Execute` Method Semantics

The `execute` method defined by the declaring node has the following semantics.

1. Return type. If the return type is `Stateful`, the method is assumed to
   modify the monadic state. For any other return type, the method cannot modify
   the monadic state.
2. Arguments. The method may take any number of arguments, though at least an
   argument named `self` must be present. The arguments have following
   semantics:
   1. An `Object` argument may be annotated with `@MonadicState`. This tells the
      DSL to substitute it for the current value of monadic state. Note that
      this value is read-only. In order to modify state, the method must return
      a `Stateful`.
   2. Any `VirtualFrame` argument will be passed the current execution frame.
   3. Any `CallerInfo` argument will be passed the current `CallerInfo` and mark
      the method as requiring the caller info at runtime.
   4. All other arguments are treated as positional arguments to the method.
      Note that according to the language specification, any method must take an
      argument named `this`. Because of a naming conflict with Java, this
      argument is called `_this` in the DSL. Please note that it is mandatory to
      take an argument called `_this`, even if it is unused.
   5. The positional arguments can be of any typed specified by the runtime type
      system (see `org.enso.interpreter.runtime.Types`) or `Object`. Arguments
      typed as `Thunk` mark the argument as suspended in the generated function
      header.

## Generated Node

The DSL generates a single root node per annotated class. This node handles
reading the arguments from the execution frame and typechecking them, before
delegating to the declaring node.

The generated node also defines a static `makeFunction(Language)` method,
wrapping the node in a runtime function object.
