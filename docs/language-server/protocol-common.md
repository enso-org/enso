---
layout: developer-doc
title: Enso Protocol Common Message Specification
category: language-server
tags: [language-server, protocol, specification]
order: 2
---

# Enso Protocol Common Message Specification

This document contains the specification of the Enso protocol messages that are
common to multiple sub-components of the protocol.

For information on the design and architecture of the protocol, as well as its
transport formats, please look [here](./protocol-architecture.md).

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Protocol Message Specification](#protocol-message-specification)
- [Common Types](#common-types)
  - [`Path`](#path)
  - [`IPWithSocket`](#ipwithsocket)
  - [`EnsoUUID`](#ensouuid)
- [Common Errors](#common-errors)
  - [`ParseError`](#parseerror)
  - [`InvalidRequest`](#invalidrequest)
  - [`MethodNotFound`](#methodnotfound)
  - [`InvalidParams`](#invalidparams)
  - [`ServiceError`](#serviceerror)
  - [`NotImplementedError`](#notimplementederror)

<!-- /MarkdownTOC -->

## Protocol Message Specification

The message specification for protocol messages must include the following
fields:

- **Type:** The type of the message (e.g. Request or Notification).
- **Direction:** The direction in which the _originating_ message is sent
  (either `Client -> Server` or `Server -> Client`).
- **Connection:** Which connection the message should be sent on. Write
  'Protocol' for the textual connection and 'Data' for the binary connection.
- **Visibility:** Whether the method should be used by the public or is an
  internal / implementation detail ('Public' or 'Private').

They must also contain separate sections specifying their parameters, result (if
it has one), and any errors that may occur. These specifications should be
either in typescript or flatbuffers syntax, depending on the connection on which
the message occurs.

The capability specifications must include the following fields, as well as a
section 'Enables' stating which protocol messages are gated by the capability.

- **method:** The name of the capability.
- **registerOptions:** The options that must be provided to register the
  capability, described using typescript type syntax.

## Common Types

There are a number of types that are shared between many of the protocol
messages. They are specified below.

### `Path`

A path is a representation of a path relative to a specified content root.

#### Format

Please note that segments can only be ordinary file names, `..` and `.` may not
be supported.

```typescript
interface Path {
  rootId: UUID;
  segments: [String];
}
```

```idl
namespace org.enso.languageserver.protocol.binary;

//A representation of a path relative to a specified content root.
table Path {

  //a content root id that the path is relative to
  rootId: EnsoUUID;

  //path segments
  segments: [string];

}
```

### `IPWithSocket`

A IPWithSocket is an endpoint for communication between machines.

#### Format

```typescript
interface IPWithSocket {
  host: String;
  port: Int;
}
```

### `EnsoUUID`

An EnsoUUID is a value object containing 128-bit universally unique identifier.

#### Format

```idl
namespace org.enso.languageserver.protocol.binary;

//A binary representation of universally unique identifiers.
struct EnsoUUID {

  //The most significant bits of the UUID.
  leastSigBits:uint64;

  //The most significant bits of the UUID.
  mostSigBits:uint64;

}
```

## Common Errors

### `ParseError`

Signals that the message failed to parse.

```typescript
"error" : {
  "code" : -32700,
  "message" : "Parse error"
}
```

### `InvalidRequest`

Signals that the request is invalid.

```typescript
"error" : {
  "code" : -32600,
  "message" : "Invalid Request"
}
```

### `MethodNotFound`

Signals that the requested method is not supported.

```typescript
"error" : {
  "code" : -32601,
  "message" : "Method not found"
}
```

### `InvalidParams`

Signals that the parameters provided for the requested method were invalid.

This may indicate that the type or format of one of the parameters is different
than expected.

```typescript
"error" : {
  "code" : -32602,
  "message" : "Invalid params"
}
```

### `ServiceError`

Signals a generic service error.

```typescript
"error" : {
  "code" : 1,
  "message" : "Service error"
}
```

### `NotImplementedError`

Signals that the requested method is supported, but it is has not been
implemented yet.

```typescript
"error" : {
  "code" : 10,
  "message" : "The requested method is not implemented"
}
```
