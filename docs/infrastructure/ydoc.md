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
  - [Visualization Channels](#visualization-channels)
- [Thread Safety](#thread-safety)
- [Startup Flow](#startup-flow)
- [Source Code Layout](#source-code-layout)
- [Debugging](#debugging)
  - [Inspecting Channel Traffic with ydoc-inspect](#inspecting-channel-traffic-with-ydoc-inspect)

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

Four callback instances are passed to the Ydoc server at startup:

- `YDOC_JSON_CHANNEL_CALLBACKS` - For JSON-RPC text messages
- `YDOC_BINARY_CHANNEL_CALLBACKS` - For binary protocol messages
- `YDOC_VIS_CONTROL_CHANNEL_CALLBACKS` - For visualization attach/detach/fail
  JSON messages (see [Visualization Channels](#visualization-channels))
- `YDOC_VIS_DATA_CHANNEL_CALLBACKS` - For visualization response bytes

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

### Visualization Channels

Visualization requests and responses are carried over a dedicated pair of
channels and an accompanying subdoc. The legacy
`executionContext/{attach,detach,modify}Visualization` and
`executionContext/executeExpression` JSON-RPC methods and the FlatBuffers
`VisualizationUpdate` binary notification have been removed.

- `vis:control` (JSON strings) carries the four message kinds exchanged between
  the ydoc-server bridge and the Language Server. The bridge sends `attach` and
  `detach` to the LS. The LS sends `ready` and `failed` back.
- `vis:data` (binary, raw `Uint8Array` on the JS side, Java `ByteBuffer` on the
  LS side) carries response payloads framed as `[16-byte requestId][bytes]`.

The **visualization subdoc** is a Yjs subdoc held under
`DistributedProject.visualizations` (a Y.Map keyed by a single reserved slot).
Clients write slots into the subdoc's top-level `slots: Y.Map<requestId, ...>`
to request visualizations. Each slot is a Y.Map with fixed field-name keys
(`visualizationId`, `contextId`, `nodeExternalId`, `request`, `status`,
`response`, `failure`, `createdAt`). `status` is one of
`pending | ready | failed`. The request's `expression` field is a tagged union
(`string | LSMethodPointer | { inFrame: string }`); slots whose expression is an
`{ inFrame }` are one-shot evaluations (the old `executeExpression` path).

The ydoc-server
[`visualizationBridge`](../../app/ydoc-server/src/visualizationBridge.ts)
observes those mutations and emits `attach` messages on `vis:control` when it
sees a newly-`pending` slot, and `detach` messages when a previously-attached
slot is removed from the map (except for `inFrame` one-shots, the runtime
auto-detaches those internally, so the client removes the slot after reading the
response and the bridge does not emit a detach). Responses flowing back from the
LS as binary frames on `vis:data` are written into the originating slot's
`response` field and the slot's `status` flips to `ready`. Failures arrive as a
`failed` JSON control message and flip `status` to `failed`.

Persistent attach slots have no terminal status: they stay `ready` (or `failed`)
until the client removes them. Removing a slot is how clients "detach" as there
is no `detached` status. One-shot `inFrame` slots are terminal on `ready`. The
client consumes the payload and removes the slot, no detach message is emitted
because the runtime has already auto-detached the underlying oneshot.

Requests are immutable: a client modify is expressed as
`removeSlot(oldRequestId); createSlot(newRequestId)` with the same
`visualizationId`. The bridge emits both `attach(new)` and `detach(old)`. The
LS-side `VisualizationBridgeActor` keys its correlation on the bridge
`requestId` (not `visualizationId`) and suppresses the runtime detach when
another in-flight request still shares the same `visualizationId`, since the
runtime's attach is an upsert.

## Thread Safety

GraalJS polyglot context requires all JavaScript interactions to occur on a
single thread. To enable safe cross-thread communication, the Ydoc server wraps
channels and callbacks with synchronized versions:

- `YjsChannelSynchronized` - Queues channel operations to the Ydoc executor
- `YjsCallbacksSynchronized` - Wraps channels before passing to delegates

The `YdocScheduledExecutorService` maintains an event queue and executes tasks
on the owner thread.

## Startup Flow

1. `MainModule` of the Language Server creates callback instances for the JSON,
   binary, `vis:control`, and `vis:data` channels (the last two are supplied by
   `VisualizationBridgeActor`)
2. `YdocServerApi.launchYdocServer()` starts the Ydoc server, passing all four
   callbacks
3. The `Ydoc` class initializes GraalJS context and loads the `main.ts` ydoc
   entrypoint, binding all four callback objects
4. When a WebSocket client connects, Ydoc creates channels for that session
   (shared across clients of the same project URL) and invokes `onConnect()` on
   the appropriate callbacks
5. The Language Server subscribes to channels and begins message exchange. For
   the visualization channels, `VisualizationBridgeActor` records the channel
   references and decodes `attach` / `detach` JSON into
   `Api.AttachVisualization` / `Api.DetachVisualization` on the Runtime API

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

## Debugging

The more and more TypeScript code we execute on the language server side, the
more important it is to be able to _debug it properly_. Classical
[mixed debugging](../debugger/mixed-debugging.md) works, but the standard way to
debug JavaScript is to use Chrome Dev Tools. There is a dedicated page for
generic [Debugging of Enso in Chrome](../debugger/chrome-devtools.md). There are
general instructions how to [run the Enso IDE](../CONTRIBUTING.md#running-ide)
during development. Additional alternations to those instructions are given
here.

Make sure you build the JavaScript bundle (a single JavaScript file without
imports) that you want to execute and debug. Typically use:

```bash
enso$ corepack pnpm compile
enso$ ls -1 app/ydoc-server-polyglot/dist/
main.cjs
main.cjs.map
```

With the `main.cjs` file generated continue to launch the `dev:gui` with
additional environment variables:

```bash
enso$ ENSO_ENGINE_ARGS=--jvm \
      JAVA_TOOL_OPTIONS="-ea" \
      YDOC_SERVER_JS=`pwd`/app/ydoc-server-polyglot/dist/main.cjs \
      corepack pnpm dev:gui
```

The first environment makes sure engine will be running in `--jvm` mode and that
we can _enable assertions_ in that mode by the second environment variable. When
assertions are on and the `YDOC_SERVER_JS` is specified and appropriate file
exists, it will be loaded in instead of the builtin version embedded in `enso`
binary itself.

<img width="1990" height="1060" alt="chrome dev tools in JVM mode" src="https://github.com/user-attachments/assets/43d1c9af-6b36-422c-b5bb-c2c662fb717d" />

The Chrome Dev Tools URL gets printed on the console and can be used to attach
to the `YDOC_SERVER_JS` script. Should the be a need to _"attach early"_ one can
rename the file to include word _"suspend"_ in its name - like
`main-suspend.cjs` for example. Then the execution stops before the debugger is
attached.

### Debugging `enso` _Native Image_ Binary

Sometimes it may be beneficial to debug _native image_ version of `enso` binary.
Then one has to get a binary with _enabled assertions_ - according to the
[native image configuration](./native-image.md#engine-configuration) page one
can use:

```bash
enso$ ENSO_LAUNCHER=native,test sbt buildEngineDistribution
```

with such an `enso` binary one can skip `--jvm` argument and just use:

```
enso$ corepack pnpm compile
enso$ YDOC_SERVER_JS=`pwd`/app/ydoc-server-polyglot/dist/main.cjs \
  corepack pnpm dev:gui
```

Compiling _native image_ version takes more time, however launching the _native
image_ version is usually way faster than the `--jvm` version. Moreover it more
closely mimics the _production mode_ used by majority of Enso users.

### Inspecting Channel Traffic with ydoc-inspect

The `ydoc-inspect` tool connects to a running Ydoc server and provides an
interactive console for observing and injecting messages on YjsChannels. It
syncs the server's internal inspect Y.Doc via WebSocket and exposes helper
functions through Chrome DevTools `chrome://inspect` page.

#### Prerequisites

The inspect endpoint is only available when the Ydoc server runs in debug mode.
This is controlled by the `ENSO_IDE_YDOC_LS_DEBUG` environment variable, which
is set to `true` automatically when the application is started in dev mode with
`pnpm run dev:gui`. When debug mode is active, the `InspectManager` wraps both
JSON and binary channel servers to intercept all message traffic and expose it
through a `/project/inspect` WebSocket endpoint.

#### Running ydoc-inspect

Start the application and open a project:

```bash
enso$ corepack pnpm run dev:gui
```

Launch the inspect tool:

```bash
enso$ corepack pnpm run dev:inspect
```

Available CLI options:

| Option       | Default     | Description                              |
| ------------ | ----------- | ---------------------------------------- |
| `--host`     | `localhost` | Ydoc server hostname                     |
| `--port`     | `30617`     | Ydoc server port                         |
| `--truncate` | `240`       | Max characters for message data display  |
| `--no-watch` | _(off)_     | Disable automatic live message streaming |

#### DevTools Console Commands

Once connected, open `chrome://inspect` and attach to the Node.js process. The
following global functions are available in the DevTools console:

**Channel inspection:**

```js
channels()                    // List all registered channels
messages(channelId?, n?)      // Get messages (optionally for a channel, last n)
filter(channelId?, pattern?)  // Filter messages by regex (string or RegExp)
send(channelId, msg)          // Send a message to the client as Language Server
receive(channelId, msg)       // Send a message to Language Server as client
watch(channelId?)             // Watch live messages (returns stop function)
unwatch()                     // Stop watching live messages
```

**AST inspection:**

```js
modules()                     // List all module names in the project
ast(moduleName?)              // Get root AST node (defaults to Main)
tree(moduleName?, depth?)     // Print AST tree structure to console
node(id)                      // Look up an AST node by id
meta(id)                      // Show metadata for a node (position, visualization, etc.)
code(moduleName?)             // Print full module source code
```

The AST commands work by syncing the project's Y.Doc (the `index` document)
alongside the inspect Y.Doc.
