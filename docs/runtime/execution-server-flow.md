---
layout: developer-doc
title: Execution Server Flow
category: runtime
tags: [runtime, execution, language-server]
order: 6
---

# Execution Server Flow
This document describes the API and workflow of the internal execution server.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Opening a Connection](#opening-a-connection)
- [API](#api)
- [Internal Architecture](#internal-architecture)
   - [Job Queue](#job-queue)
   - [Job Types](#job-types)
   - [Scheduling Rules](#scheduling-rules)
   - [API Methods to Jobs Translation](#api-methods-to-jobs-translation)

<!-- /MarkdownTOC -->

## Opening a Connection
> The actionables for this section are:
> 
> 1. describe the `org.graalvm.polyglot.Context.Builder.serverTransport`
>    workflow of connecting to the server.

## API
> The actionables for this section are:
> 
> 1. Document the server's API.

## Internal Architecture
This section describes certain implementation details of the execution server,
allowing it to perform its operations safely and interactively.

### Job Queue
The execution server uses a job queue containing requests to be performed.
An API method may queue multiple jobs.

### Job Types
There are a number of job types used internally for handling different
scenarios:

#### `EnsureCompiled`
Takes a set of module names that must be compiled and ensures that the
corresponding modules are compiled in the newest version.
It also performs cache invalidations on changes since the last batch.
Caches should be invalidated for all contexts affected by this recompilation.
This operation is idempotent and should be run before any `Execute` action.
This operation is not currently interruptible, but may become so in the future.

#### `Execute`
Takes a context ID and an optional set of expression IDs that should be
executed. and executes the Enso code corresponding to the context's stack.
Updates caches and sends updates to the users.

This operation is interruptible through `Thread.interrupt()`.

### Scheduling Rules

1. `EnsureCompiled` jobs must be run sequentially (i.e. no other state
   modifications or executions are allowed during this job's run).
2. `Execute` jobs may be run in parallel with each other (but not with
   `EnsureCompiled` jobs).
3. `EnsureCompiled` jobs may be collapsed into one, by unifying their module
   sets.
4. `Execute` jobs with the same `contextId` may be collapsed into one, by
   merging their `expression IDs` sets.
5. All enqueued `EnsureCompiled` jobs should run before any `Execute` jobs.
6. The order of `EnsureCompiled` jobs can be freely changed by the compiler.

### API Methods to Jobs Translation
The following describes handling of API messages through job queue
modifications.

1. EditFile:
   1. Abort and/or dequeue all pending and running messages (if possible).
   2. Synchronously perform all code updates.
   3. Enqueue an `EnsureCompiled` with all affected files.
   4. Enqueue an `Execute` for each existing context.
2. Stack Modifications and Recomputes:
   1. Abort and/or dequeue all pending and running requests relevant to the
      affected context.
   2. Synchronously perform all state updates.
   3. Respond to the user.
   4. Enqueue EnsureCompiled for the file at the bottom of the affected
      context's stack.
   5. Enqueue Execute for the affected context.
3. Visualization modifications:
   1. Synchronously perform all state updates.
   2. Respond to the user.
   3. Enqueue `EnsureCompiled` and `Execute` for the affected context.
      Set the minimal set of expressions required to compute to
      `Set(visualizedExpression)` in the `Execute` command.
4. Create/Destroy context:
   1. Abort any jobs concerning the affected context.
   2. Perform state updates / cleanups. No jobs to schedule.
