---
layout: developer-doc
title: Enso Debugger Protocol Message Specification
category: debugger
tags: [repl, debugger, protocol, specification]
order: 1
---

# Enso Debugger Protocol Message Specification
Binary Protocol for the Debugger is used in communication between the runtime
and tools exploiting the REPL/debugger functionalities. It can be used to
implement a simple REPL or add debugging capabilities to the editor.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Types](#types)
  - [`ObjectRepr`](#objectrepr)
  - [`StackTraceElement`](#stacktraceelement)
  - [`Exception`](#exception)
  - [`Binding`](#binding)
- [Messages](#messages)
  - [Session start](#session-start)
  - [Evaluation](#evaluation)
  - [List bindings](#list-bindings)
  - [Session exit](#session-exit)

<!-- /MarkdownTOC -->

## Types

There are some helper types used within the debugger's protocol. These are
specified here.

### `ObjectRepr`
External representation of arbitrary values returned by the REPL (internally
these are of type `Object`).

As these values are only used for presentation, they are represented by String.

```typescript
interface ObjectRepr {
  representation: String;
}
```

### `StackTraceElement`
Represents a line of the stack trace. Corresponds to
`java.lang.StackTraceElement`.

```typescript
interface StackTraceElement {
  declaringClass: String;
  methodName: String;
  fileName: String;
  lineNumber: Int;
}
```

### `Exception`
Represents an exception that may have been raised during requested execution.

```typescript
interface Exception {
  message: String;
  stackTrace: [StackTraceElement];
  cause: Exception;
}
```

### `Binding`
Represents a single binding in the current scope.

```typescript
interface Binding {
    name: String;
    value: ObjectRepr;
}
```

## Messages

All endpoints accept messages of type `Request` and return a `Response`. 
These messages contain unions that contain the actual payload specified for each
type of message.

```idl
namespace org.enso.polyglot.debugger.protocol;

union RequestPayload {
    EVALUATE: EvaluationRequest,
    LIST_BINDINGS: ListBindingsRequest,
    SESSION_EXIT: SessionExitRequest
}

table Request {
    payload: RequestPayload (required);
}

union ResponsePayload {
    EVALUATION_SUCCESS: EvaluationSuccess,
    EVALUATION_FAILURE: EvaluationFailure,
    LIST_BINDINGS: ListBindingsResult,
    SESSION_EXIT: SessionExitSuccess,
    SESSION_START: SessionStartNotification
}

table Response {
    payload: ResponsePayload (required);
}
```

### Session start

When a breakpoint is reached, the debugger sends a notification to the client
indicating that a REPL session should be started.

#### Notification
```idl
namespace org.enso.polyglot.protocol.debugger;

table SessionStartNotification {}
```

### Evaluation
Evaluates an arbitrary expression in the current execution context.

Responds with either a message with the value of successfully evaluated 
expression or a message with an exception that has been raised during
evaluation.

#### Request
```idl
namespace org.enso.polyglot.protocol.debugger;

table ReplEvaluationRequest {
  expression: String (required);
}
```

#### Response
```idl
namespace org.enso.polyglot.protocol.debugger;

table ReplEvaluationSuccess {
  result: ObjectRepr (required);
}

table ReplEvaluationFailure {
  exception: Exception (required);
}
```

### List bindings
Lists all the bindings available in the current execution scope and sends them 
back.

#### Request
```idl
namespace org.enso.polyglot.protocol.debugger;

table ReplListBindingsRequest {}
```

#### Response
```idl
namespace org.enso.polyglot.protocol.debugger;

table ReplListBindingsResult {
  bindings: [Binding];
}
```

### Session exit
Terminates this REPL session (and resumes normal program execution).

The last result of Evaluation will be returned from the instrumented node or if
no expressions have been evaluated, unit is returned.

This function must always be called at the end of REPL session, as otherwise the
program will never resume.

#### Request
```idl
namespace org.enso.polyglot.protocol.debugger;

table ReplExitRequest {}
```

#### Response
```idl
namespace org.enso.polyglot.protocol.debugger;

table ReplExitSuccess {}
```