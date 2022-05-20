---
layout: developer-doc
title: Managed Resources
category: semantics
tags: [resources, finalization, cleanup]
order: 9
---

# Managed Resources

Enso is a language targeting an audience with possibly low programming skills
and aims to be as user-friendly as possible. Therefore, it is crucial to provide
some mechanisms to automatically clean up unclosed resources (such as file
handles, sockets, machine pointers, etc.). The Managed Resources system solves
this problem, by allowing library authors to attach garbage collection hooks to
certain objects, such that a clean up action can be performed as soon as the
runtime discovers the resource will never be used again. This document outlines
the behavior of this system, as well as important notes regarding its use.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Basic Usage](#basic-usage)
- [Semantics and Guarantees](#semantics-and-guarantees)
  - [Execution Guarantess](#execution-guarantees)
  - [Multiple Managed Resources Wrapping The Same Underlying Resource](#multiple-managed-resources-wrapping-the-same-underlying-resource)
  - [Thread Safety](#thread-safety)

<!-- /MarkdownTOC -->

## Basic Usage

A new managed resource is created by calling the
`Managed_Resource.register resource finalizer` method. The `resource` is the
object being finalized and `finalizer` is a one-argument function that will be
passed the `resource` upon finalization. this call returns an object wrapping
the original resource. The underlying resource will be finalized as soon as the
_returned wrapper_ is garbage collected. It is therefore crucial to stop using
`resource` right after the call, as it may be finalized at any point after this
call.

> #### Important
>
> Due to the limitations of current implementation of Enso, the `finalizer`
> passed to `Managed_Resource.register` must not be a lambda. This is because
> lambdas implicitly capture the whole lexical scope they are defined in, so in
> `res = Managed_Resource.register object (o -> o.close)`, the `finalizer`
> closes over the value of `res`, preventing it from being garbage collected.
> The same limitation concerns the underscore-lambda syntax, as `_.close` is
> equivalent to `o -> o.close`. The finalizer should be a (possibly curried)
> call to a function defined outside of the lexical scope of the
> `Managed_Resource.register` call.

To perform operations on the underlying resource, use the
`<managed-resource>.with action` method, where `<managed-resource>` is the
object returned from the call to `Managed_Resource.register`, and `action` is a
function taking the underlying object as its only argument. It is important that
the object passed to `action` is not stored and is not used past the return of
`action`. This means in particular that it is unsafe to give another thread a
reference to that object, if the thread remains alive past the return of
`action`. If such an operation is necessary, the other thread should call `with`
itself, using a reference to the original manged resource.

A managed resource can be closed manually, using `<managed-resource>.close`. The
underlying object is then finalized immediately.

The finalization of a resource can be aborted using
`Managed_Resource.take resource`. This call will abort any automatic
finalization actions and return the original underlying object. The return value
is no longer managed by the runtime and must either be finalized manually or
wrapped in a new managed resource using a call to `Managed_Resource.register`.

## Semantics and Guarantees

This section outlines the runtime semantics and guarantees provided by the
managed resources system.

### Execution Guarantees

The finalizer attached to a managed resource is guaranteed to be executed
at-most-once.

There are no guarantees that the finalizer will ever _be_ executed. It is
executed as soon as the runtime garbage-collects the managed resource, but this
is not to say "as soon as the managed resource becomes unreachable". The runtime
is free to run garbage collection at any point, including to not run it at all
over the course of program execution. A call to `Runtime.gc` serves as hint to
the runtime system to perform garbage collection, but does not guarantee that
garbage collection will actually run.

The finalizer may be run from any application thread, with no guarantees as to
which thread will perform the finalization.

### Multiple Managed Resources Wrapping The Same Underlying Resource

In case the same underlying resource is used in multiple managed resources, it
will be finalized as soon as the first managed resource is garbage collected.
Moreover, the finalizer will be called for each garbage collected managed
resource, possibly leading to multiple-finalization of the underlying object.
Therefore, using the same underlying resource with multiple managed resource
instances should be considered an error.

### Thread Safety

Operations on managed resources are thread safe. Therefore, the safety
guarantees of the underlying resources are the limitation – if the underlying
resource is not thread-safe, calls to `Managed_Resource.with` will also not be
thread-safe.
