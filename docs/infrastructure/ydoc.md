---
layout: developer-doc
title: Ydoc Server
category: infrastructure
tags: [infrastructure, ydoc, collaborative-editing, yjs]
order: 8
---

# Ydoc Server

The Ydoc server enables real-time collaborative editing in the Enso IDE using
[Yjs](https://yjs.dev/) CRDTs (Conflict-free Replicated Data Types). It runs as
a polyglot JavaScript application within GraalJS and communicates with the
Language Server through dedicated message channels.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Architecture Overview](#architecture-overview)
- [Communication Channels](#communication-channels)
  - [YjsChannel Interface](#yjschannel-interface)
  - [YjsChannel.Server Interface](#yjschannelcallbacks-interface)
  - [JSON Channel](#json-channel)
  - [Binary Channel](#binary-channel)
- [Thread Safety](#thread-safety)
- [Startup Flow](#startup-flow)
- [Source Code Layout](#source-code-layout)

<!-- /MarkdownTOC -->

## Architecture Overview

The Ydoc server acts as a bridge between WebSocket clients (IDE instances) and
the Language Server. It maintains shared document state using Yjs CRDTs and
synchronizes changes across all connected clients.

```
+------------------+                +--------------------------------------------------------------+
|                  |                |                                                              |
| +--------------+ |    WebSocket   | +-----------------+   YjsChannel.Server  +-----------------+ |
| |  IDE Client  | |<-------------->| |  Ydoc Server    |<-------------------->| Language Server | |
| +--------------+ |                | +-----------------+                      +-----------------+ |
|     Electron     |                |                         GraalVM                              |
+------------------+                +--------------------------------------------------------------+
```

The IDE client creates a `YjsChannel` backed by a Yjs `Y.Array` and connects to
the Ydoc server using the
[Yjs WebSocket provider](https://docs.yjs.dev/ecosystem/connection-provider/y-websocket).
This provider handles automatic synchronization of CRDT state between the client
and server over WebSocket.

When the IDE client establishes a WebSocket connection, the Ydoc server creates
a corresponding `YjsChannel` (also backed by `Y.Array`) that is synchronized
with the client's channel. The Yjs sync protocol ensures both sides maintain
consistent state.

The Ydoc server then passes the newly created `YjsChannel` to the Language
Server by invoking `onConnect()` on the `YjsChannel.Server` interface. This
allows the Language Server to subscribe to messages from the IDE client and send
responses back through the same channel.

The Ydoc server runs in a GraalJS polyglot context, allowing JavaScript code to
interact with Java objects. This enables efficient message passing between the
TypeScript-based Ydoc implementation and the Java-based Language Server.

## Communication Channels

Communication between the Ydoc server and Language Server uses the `YjsChannel`
abstraction. Messages are stored in a Yjs Y.Array CRDT, which provides reliable
ordering and delivery even across runtime boundaries.

### YjsChannel Interface

The `YjsChannel` interface defines bidirectional message passing:

```java
public interface YjsChannel {
  void send(Object message);
  void subscribe(Consumer<Object> messageHandler);
}
```

- `send()` - Sends a message to the remote endpoint
- `subscribe()` - Registers a handler for incoming messages

Messages sent by an endpoint are automatically filtered out on that endpoint,
preventing echo. If messages arrive before a handler is subscribed, they are
queued and delivered upon subscription.

### YjsChannel.Server Interface

The `YjsChannel.Server` interface notifies the Language Server when new channels
are established:

```java
public interface YjsChannel.Server {
  void onConnect(YjsChannel channel);
}
```

Two callback instances are passed to the Ydoc server at startup:

- `YDOC_JSON_CHANNEL_CALLBACKS` - For JSON-RPC text messages
- `YDOC_BINARY_CHANNEL_CALLBACKS` - For binary protocol messages

### JSON Channel

The JSON channel handles JSON-RPC communication for IDE operations. On the
Language Server side, `YdocJsonRpcServer.ServerCallbacks` implements
`YjsChannel.Server` and creates Akka actors to process messages:

- `MessageHandlerSupervisor` - Processes incoming JSON-RPC requests
- `OutgoingMessageHandler` - Sends JSON-RPC responses through the channel

### Binary Channel

The binary channel handles efficient binary data transfer. On the TypeScript
side, `YjsBinaryChannel` extends `YjsChannel` to convert between JavaScript
`Uint8Array` and Java direct `ByteBuffer`.

On the Language Server side, `BinaryYdocServer.BinaryServerCallbacks` decodes
incoming binary messages and forwards them to connection controllers.

## Thread Safety

GraalJS polyglot context requires all JavaScript interactions to occur on a
single thread. To enable safe cross-thread communication, the Ydoc server wraps
channels and callbacks with synchronized versions:

- `YjsChannelSynchronized` - Queues channel operations to the Ydoc executor
- `YjsCallbacksSynchronized` - Wraps channels before passing to delegates

The `YdocScheduledExecutorService` maintains an event queue and executes tasks
on the owner thread.

## Startup Flow

1. `MainModule` of the Language Server creates callback instances for JSON and
   binary channels
2. `YdocServerApi.launchYdocServer()` starts the Ydoc server
3. The `Ydoc` class initializes GraalJS context and loads `main.ts` ydoc
   entrypoint passing callback objects for JSON and binary channels
4. When a WebSocket client connects, Ydoc creates channels and invokes
   `onConnect()` on the appropriate callbacks
5. The Language Server subscribes to channels and begins message exchange

## Source Code Layout

**Java/Scala (Language Server side):**

- [`lib/java/ydoc-api/`](../../lib/java/ydoc-api/) - Core interfaces
  (`YjsChannel`, `YjsChannel.Server`)
- [`lib/java/ydoc-server/`](../../lib/java/ydoc-server/) - Server implementation
  and thread synchronization
- [`lib/scala/json-rpc-server/.../YdocJsonRpcServer.scala`](../../lib/scala/json-rpc-server/src/main/scala/org/enso/jsonrpc/YdocJsonRpcServer.scala) -
  JSON-RPC channel handling
- [`engine/language-server/.../BinaryYdocServer.scala`](../../engine/language-server/src/main/scala/org/enso/languageserver/http/server/BinaryYdocServer.scala) -
  Binary channel handling
- [`lib/java/ydoc-api/.../YdocServerApi.java`](../../lib/java/ydoc-api/src/main/java/org/enso/ydoc/api/YdocServerApi.java) -
  Server bootstrap API

**TypeScript (Ydoc server side):**

- [`app/ydoc-channel/`](../../app/ydoc-channel/) - `YjsChannel` TypeScript
  implementation
- [`app/ydoc-server/`](../../app/ydoc-server/) - Ydoc server logic and
  `YjsBinaryChannel`
- [`app/ydoc-server-polyglot/`](../../app/ydoc-server-polyglot/) - GraalJS entry
  point
