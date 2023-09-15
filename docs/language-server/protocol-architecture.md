---
layout: developer-doc
title: The Enso Protocol
category: language-server
tags: [language-server, protocol, architecture]
order: 1
---

# The Enso Protocol

Enso is a sophisticated language, but in order to provide a great user
experience to our users we also need the ability to provide great tooling. This
tooling means a language server, but it also means a set of extra peripheral
components that ensure we can run Enso in a way that the product requires.

These services are responsible for providing the whole-host of language- and
project-level tooling to the IDE components, whether they're hosted in the cloud
or locally on a user's machine.

This document contains the architectural and functional specification of the
Enso protocol.

For a detailed specification of all of the messages that make up the protocol,
please see [the protocol message specifications](./README.md).

<!-- MarkdownTOC levels="2,3,4" autolink="true" -->

- [Architecture](#architecture)
  - [The Project Manager](#the-project-manager)
  - [Language Server](#language-server)
- [Textual Protocol](#textual-protocol)
  - [Textual Protocol Communication Patterns](#textual-protocol-communication-patterns)
  - [Textual Protocol Transport](#textual-protocol-transport)
  - [The Protocol Format](#the-protocol-format)
- [Textual Protocol Functionality](#textual-protocol-functionality)
  - [Textual Diff Management](#textual-diff-management)
  - [Handling Multiple Clients](#handling-multiple-clients)
  - [Project State Management](#project-state-management)
  - [File Management and Storage](#file-management-and-storage)
  - [Execution Management](#execution-management)
    - [Caching](#caching)
    - [Progress Reporting](#progress-reporting)
  - [Completion](#completion)
  - [Analysis Operations](#analysis-operations)
  - [Functionality Post 2.0](#functionality-post-20)
- [Binary Protocol](#binary-protocol)
  - [Binary Protocol Communication Patterns](#binary-protocol-communication-patterns)
  - [Binary Protocol Transport](#binary-protocol-transport)
- [Binary Protocol Functionality](#binary-protocol-functionality)
  - [Displaying Visualizations](#displaying-visualizations)
- [Service Connection Setup](#service-connection-setup)
- [Service Connection Teardown](#service-connection-teardown)

<!-- /MarkdownTOC -->

## Architecture

The divisions of responsibility between the backend engine services are dictated
purely by necessity. As multi-client editing necessitates careful
synchronisation and conflict resolution, between the actions of multiple
clients. This section deals with the intended architecture for the Engine
Services.

The engine services are divided into two main components:

1. **The Project Manager:** This component is responsible for listing and
   managing user projects, as well as spawning the language server for a given
   project when it is opened.
2. **The Language Server:** This component is responsible for dealing with
   incoming connections and resolving conflicts between multiple clients. It is
   also responsible for servicing all of the requests from the clients.

Both components will be implemented as akka actors such that we can defer the
decision as to run them in different processes until the requirements become
more clear.

### The Project Manager

The project manager service is responsible for both allowing users to work with
their projects but also the setup and teardown of the language server itself.
Its responsibilities can be summarised as follows:

- Allowing users to manage their projects.
- Starting up the language server for a given project upon user selection.
- Notifying the language server of a pending shutdown on project exit to allow
  it to persist any state that it needs to disk.

### Language Server

The language server is responsible for managing incoming connections and
communicating with the clients, as well as resolving any potential conflicts
between the clients. It is responsible for the following:

- Negotiating and accepting connections from multiple clients.
- Resolving conflicts between messages from multiple clients.
- Optimising incoming requests wherever possible.

It is also responsible for actually servicing all of the incoming requests,
which includes but isn't limited to:

- **Completion Information:** It should be able to provide a set of candidate
  completions for a given location in the code.
- **Introspection Information:** It should be able to provide introspection
  information from the running interpreter, which consists primarily of types
  and values.
- **Textual Diff Management:** It needs to be able to accept and publish diffs
  of the program source code. As part of this, it needs to keep the node
  metadata up to date.
- **Analysis Operations:** It should be able to service various IDE-style
  analysis requests (e.g. jump-to-definition or find usages)
- **Arbitrary Code Execution:** It should be able to execute arbitrary Enso code
  on values in scope.
- **Refactoring:** Common refactoring operations for Enso programs, including
  renaming, code formatting, and so on.
- **IO Management:** Though this is arguably a feature of the runtime rather
  than the language server itself, this refers to the ability to watch files and
  monitor IO in order to recompute minimal subsets of the program.

It should be noted that the language server _explicitly does not_ talk using
LSP. The component is solely responsible for _servicing_ requests, instead of
dealing with the minutiae of connection negotiation and request handling.

Additionally, it is _very important_ to note that the language server _must not_
depend directly on the runtime (as a compile-time dependency). This would
introduce significant coupling between the runtime implementation and the
language server. Instead, the LS should only depend on `org.graalvm.polyglot` to
interface with the runtime.

## Textual Protocol

> This section is partially out of date.

The protocol refers to the communication format that all of the above services
speak between each other and to the GUI. This protocol is not specialised only
to language server operations, as instead it needs to work for all of the
various services in this set.

The protocol we are using intends to be fully compatible with the Microsoft LSP
[specification](https://microsoft.github.io/language-server-protocol/specifications/specification-3-145)
(version 3.15). In essence, we will operate as follows:

- Where our use case matches with a function provided by LSP, we will use the
  specified LSP message (e.g. completions).
- Where our use-case does not match a message provided by LSP, we will use the
  following process:
  1. If we can implement this on top of one of LSP's extensible mechanisms (e.g.
     commands) we will do so.
  2. If this is not possible, we will specify an _extension_ to the protocol.
     This extension will be well-specified within this document, and should be
     in the spirit of the existing protocol. If relevant, we may propose it as a
     future extension to the specification.

Aside from the language server protocol-based operations, we will definitely
need a protocol extension to support Enso's custom language functionality.

### Textual Protocol Communication Patterns

Whatever protocol we decide on will need to have support for a couple of main
communication patterns:

- **Pub/Sub:** A standard publisher/subscriber model, the server will need to be
  able to support this kind of connection to deal with events that do not occur
  strictly in response to client actions (e.g. updates to observed values).
- **Req/Res:** A standard request/response model, the server will need to be
  able to support this kind of connection to deal with one-off requests from the
  client, and potentially to make requests to the client (e.g. list modules in
  the current project, please refresh your file state).

There are also certain messages that follow the request/response model but where
the responses are trivial acknowledgements. For simplicity's sake these are
currently subsumed by the generic request-response model.

As we have decided to remain compatible with LSP, we can use any communication
pattern that we desire, either by employing existing LSP messages, or writing
our own protocol extensions. Both of the above-listed patterns are supported by
LSP.

We can support additional patterns through LSP's mechanisms:

- Asynchronous responses can be sent as notifications.
- Protocol-level acknowledgements is supported directly in LSP.

### Textual Protocol Transport

The transport of the protocol refers to the underlying layer over which its
messages (discussed in [the protocol format](#the-protocol-format) below) are
sent. As we are maintaining compatibility with LSP, the protocol transport
format is already defined for us.

- Textual messages are sent using
  [JSON-RPC](https://en.wikipedia.org/wiki/JSON-RPC) over a WebSocket connection
  (as defined in the LSP spec).
- As a protocol extension we also negotiate a secondary binary WebSocket
  connection for sending visualization data. This transport is independent of
  the LSP spec, and hence is defined entirely by us.

> The actionables for this section are:
>
> - Determine the details for the binary WebSocket, including how we want to
>   encode messages and pointers into the stream, as well as how we set it up
>   and tear it down.

### The Protocol Format

Protocol messages are defined by LSP. Any extensions to the messages defined in
the standard should use similar patterns such that they are not incongruous with
LSP messages. The following notes apply:

- Textual messages should be sent as LSP messages or extensions to them.
- We have a hybrid extension to the protocol to allow us to send binary data
  (for visualizations) over a second WebSocket connection.

This means that we have two pipes: one is the textual WebSocket defined by LSP,
and the other is a binary WebSocket.

## Textual Protocol Functionality

This entire section deals with the _functional_ requirements placed upon the
protocol used by the engine services. These requirements are overwhelmingly
imposed by the IDE, but also include additional functionality for the future
evolution of the language.

All of the following pieces of functionality that are explained in detail are
those _expected_ for the 2.0 release. Any additional functionality beyond this
milestone is described in a [dedicated section](#functionality-post-20).

### Textual Diff Management

The engine services need to support robust handling of textual diffs. This is
simply because it is the primary form of communication for synchronising source
code between the IDE and the engine. It will need to support the following
operations:

- Synchronisation requests to ensure that the engine and IDE have the same view
  of the files in the project.
- Diff update requests, that send a textual diff between client and server (or
  vice versa).

Both of these are supported natively within the LSP, and we will be using those
messages to implement this.

It should be noted that we _explicitly_ do not intend to handle updates to node
metadata within the language server.

- These updates should be sent as part of each diff the client provides (as a
  separate segment in the `didChange` message).
- We may support this _in the future_, but we do not for now.

We place the following requirements upon the implementation of this:

- We must be able to handle diffs of _any size_, even though we prefer that the
  client sends us minimal diffs. We do not know what _all_ clients will do, and
  hence to remain compatible we must handle all diffs.
- Diffs may require some AST-based or semantic minimisation in order to assist
  in the compiler's incremental pipeline.
- The gateway must handle diffs from multiple clients properly.

The implementation is as follows:

- Support for the LSP messages `didOpen`, `didChange`, `willSaveWaitUntil`,
  `didSave`, `didClose`, and support for informing the runtime on each of these.
- It must track which files are currently open in the editor.
- The language server and runtime should watch the project folder in order to
  track updates as necessary.

### Handling Multiple Clients

Multiple-client support will be implemented while remaining compatible with the
LSP specification.

- In the initial implementation, we will work on the principle of 'write lock',
  where only one of the multiple connected clients has the ability to write to
  the file.
- In the future we will work on true conflict resolution of edits, mediated by
  the language server.

It will work as follows:

- We will use as much of the LSP initialisation and connection flow as possible
  to connect additional clients. The extensions should be minimal and should
  _not_ break compatibility with LSP.
- We will extend the protocol (or use extension mechanisms) to implement
  write-lock negotiation. The first client to connect is granted write-lock, but
  this may be changed later via negotiation.
- If any client sends a `didChange` message without holding the write lock, it
  should receive an `applyEdit` message that reverts the change, as well as a
  notification of the error.
- The language server / gateway is responsible for synchronising state with new
  clients as they connect. As part of initialisation it should receive client
  state, and then `applyEdit` to synchronise views of the code.

### Project State Management

One of the most important functionalities for this service set is the ability to
manage the state of a project in general. The project state refers to the whole
set of the project files and metadata and needs to support the following
functionalities:

- Get project metadata (name, maintainer, version, dependencies, and so on)
- Change requests for the above

All file-based operations in the project can be handled by the editor directly,
or the language server (when doing refactoring operations), and as such need no
support in this section of the protocol.

At the current time, the language server has a 1:1 correspondence with a
project. In the future, however, we may want to add LSP support for multiple
projects in a single engine, as this would allow users to work with multiple
related projects in a single instance of the IDE.

### File Management and Storage

The nature of LSP means that file management and storage is _not_ handled by the
language server, and is instead handled by the editor. The protocol makes a
distinction between:

- **Open Files:** These are the ones currently open in the editor, and are
  'owned' by the editor. The language server has to work with an in-memory
  representation for these files.
- **Closed Files:** These are the ones not open in the editor, and can be
  accessed and modified _directly_ by the language server.

The language server must have _direct_ access to the project directory on the
machine where it is running (either the local machine or in the Enso cloud), and
file operations between the IDE and that machine are handled _indepdendently_ of
the language server.

### Execution Management

The language server process will need to be able to respond to requests for
various kinds of execution of Enso code. Furthermore, it needs to be able to
respond to requests to 'listen' to the execution of various portions of code.
This implies that the following functionalities are needed:

- Execution of a function with provided arguments.
- Execution of a function from a given call site (stack position and code
  position).
- Attach an execution listener to an arbitrary code span.
- Detach an execution listener by ID.
- Implement heartbeat messages for execution listeners. If a heartbeat response
  isn't received before some time-out, the language server should detach the
  listener.
- Force cache invalidation for arbitrary code spans.
- Attach an automatic execution request.
- Detach an automatic execution request.
- Redirect `stdout`/`stdin`/`stderr` to and from the IDE.

All of these functionalities will need to take the form of custom extensions to
the LSP, as they do not fit well into any of the available extension points. To
that end, these extensions should fit well with the LSP.

A subscription (execution listener) is applied to an arbitrary span of code at a
given position in the call stack.

- A subscription may encompass multiple nodes or a single node. Information is
  received for _all_ nodes covered by the provided span.
- A subscription will ensure that the client receives information on changes in:
  - Execution state (whether the node is being computed or is cached)
  - Profiling information
  - Values
  - Types
  - Where we are in the call stack (useful for recursive execution)
- Such subscriptions _must_ be accompanied by heartbeat messages in order to
  allow the language server to cull unused subscriptions.
- Additionally, it will be important for each subscription to be able to
  configure a _rate limit_, such that the update messages do not overwhelm the
  client. If unspecified this should be set to a sensible default.

#### Caching

One of the most important elements of execution management for the language
server is the ability to control and interact with the execution cache state in
the runtime.

- This cache stores intermediate values, and every value can be in one of three
  states: invalid, valid but evicted, and valid but present.
- The cache works based on dependencies between data, such that if `foo` is used
  by `bar`, then changing `foo` must recompute `bar`.

The cache eviction strategy is one that will need to evolve. This comes down to
the simple fact that we do not yet have the tools to implement sophisticated
strategies, but we need to be correct.

- In the initial version we will invalidate _all_ call sites for a given method
  name when a name is changed. Internally this is implemented as the
  invalidation of all occurrences of a dynamic symbol by name, while ignoring
  the type it was defined on.
- We also need to account for dependencies between data such that if there is a
  dependency `b => a`, then a change to `a` must invalidate the cache result of
  `b`.
- In future, the typechecker will be able to help constrain the set of evicted
  methods by exploiting dependencies between values and types with more
  information.
- There may also be non-obvious data dependencies that can be exploited to make
  better cache-eviction decisions.

#### Progress Reporting

In the future it will be desirable for long running computations to provide
real-time progress information (e.g. for training a neural network it would be
great to know which epoch is running).

- This could be achieved by a special kind of Monadic context (similar to
  writer, but mutable buffer based).
- This would allow the function to log values without needing to return.
- These would be sent as visualizations for use in the IDE.

LSP provides an inbuilt mechanism for reporting progress, but that will not work
with visualizations. As a result that should be reserved for reporting progress
of long-running operations within the _language server_ rather than in user
code.

### Completion

The IDE needs the ability to request completions for some target point (cursor
position) in the source code. In essence, this boils down to _some_ kind of
smart completion. The completion should provide the following:

- Sensible suggestions at the cursor position, ranked by relevance.
- Local variables, where relevant.
- Suggestions for symbols that would make sense (e.g. by type) but are not
  imported. To support this, selection of such a symbol will trigger the
  automatic addition of the relevant import on the language server.
- Searching in tags and documentation. This metadata-based search functionality
  should be used to refine suggestions and help suggest functionality relevant
  to user tasks.
- Browsing the symbol hierarchy. The user should be able to click through
  modules to browse the various symbols contained within.
- Import Completion for when a user has typed `import` and hits `<tab>`. This
  feature should suggest libraries that are available, along with provide their
  top-level documentation to give users an idea of what they can be used for.

Hints should be gathered by the runtime in an un-ranked fashion based upon the
above criteria. This will involve combining knowledge from both the compiler and
the interpreter to deliver a sensible set of hints.

- Hints should be scored on a type match. For example, if we have a type `5`,
  `foo : 5 -> String` scores higher than `bar : Nat -> Dynamic`, scores higher
  than `baz : Any -> Any`. This should be done by heuristics initially, and
  later by querying the typechecker for subsumption relationships (the notion of
  specificity discussed in the types design document).
- Information contained in the `tags` section of the documentation should also
  be used to rank candidates.
- Please note that this ranking algorithm will be required to get more complex
  in the future, so please design it for extensibility _and_ high performance.
- Local variables should rank higher than global symbols.

From an implementation perspective, the following notes apply:

- This will be implemented on top of the `completion` and `completionResolve`
  messages provided by the LSP spec.
- We will extend these messages with an _optional_ field that specifies the type
  being queried upon. This is a stop-gap solution until inference can determine
  the type.
- The request does _not_ contain the query string, as text matching is handled
  by the IDE. The language server only handles candidate completions.
- We should determine if there are any sensible ways in which this process can
  be optimised, as we have the potential to return very large completion sets.
  It is probably worth waiting to see if this is necessary before implementing
  any optimisations here.

### Analysis Operations

We also want to be able to support a useful set of semantic analysis operations
to help users navigate their code. As these rely on knowledge of the language
semantics, they must be explicitly supported by the language server:

- **List Symbols in Scope:** The scope should be specified by a code span.
- **Insert Import for Symbol:** This should use an `applyEdit` message to ask
  the IDE to insert an import for the symbol specified in the request. If the
  file is closed, then the edit should be made directly, as the LSP specifies.

### Functionality Post 2.0

In addition to the functionality discussed in detail above, there are further
augmentations that could sensibly be made to the Engine services to support a
much better editing and user-experience for Enso. These are listed briefly below
and will be expanded upon as necessary in the future.

- **Refactoring Operations:** As all of these operations rely on a semantic
  analysis of the source program, they must be performed by the language server.
  These should include (but may not be limited to) the renaming, moving,
  extraction and inlining of entities. In future this could be expanded to
  include refactoring hints a la IntelliJ.
- **Arbitrary Visualization Code:** Visualizations should be able to be defined
  using Enso code and will require additional support.
- **IO Manager:** The ability to do sophisticated IO monitoring, such as
  watching for file changes, in order to support minimal re-execution of
  analysis pipelines.
- **Enhanced Type-Manipulation:** Get fits for holes, case splitting, insert
  type, refine type, solve type, and so on. Inspiration for these operations can
  be taken from programs that provide for interactive type-driven development.
- **REPL:** Protocol messages to support a REPL-style of interactive
  development. This should include, at a minimum, the ability to execute
  arbitrary code statements in a REPL, but could be enhanced by the ability to
  execute code from the editor in the REPL, and send changes back from the REPL
  to the file.
- **Debugging:**: The user should be able to place break-points, and easily
  inspect values during execution of a program. This debugging functionality
  should allow for hot-reloading of code, changing of values within a live
  program, and various other debugger functionality (step over, step in, step
  out, continue, etc). Future debugger functionality should be based on the
  standard
  [debug adapter protocol](https://microsoft.github.io/debug-adapter-protocol/specification).
- **Profiling Information:** Profiling information for the executing code, able
  to be displayed visually in Enso Studio.
- **Code Formatting:** Automatic formatting of Enso code using the One True
  Style ™.
- **Server-Side Metadata Management:** The lack of node metadata management in
  the language server currently means that any language client other than Enso
  Studio is guaranteed to corrupt the node metadata when editing Enso code. This
  will reset the node layout and can be quite annoying.
- **True Multi-Client Support:** The initial release will only support multiple
  connected clients through the use of a write lock. This is not a great user
  experience, and in future we should instead use proper conflict resolution for
  true collaborative editing. This will use a combination of `didChange` and
  `applyEdit` messages to reconcile all clients' views of the files. This is
  also why `willSaveWaitUntil` is important, as it can ensure that no client
  editor saves until it has the authority to do so (all changes are reconciled).
- **Enhanced Semantic Analysis:** Enhanced semantic analysis operations that
  rely on compiler analysis and typechecking. This includes things like "find
  usages", "jump to definition" and "find symbol", as these can greatly enhance
  a user's development experience.
- **LSP Spec Completeness:** We should also support all LSP messages that are
  relevant to our language. Currently we only support a small subset thereof.

## Binary Protocol

The binary protocol refers to the auxiliary protocol used to transport raw
binary data between the engine and the client. This functionality is _entirely_
extraneous to the operation of the [textual protocol](#textual-protocol), and is
used for transferring large amounts of data between Enso components.

As the protocol is a binary transport, it is _mediated and controlled_ by
messages that exist as part of the textual protocol.

In order to deserialize a family of messages and correlate responses with
requests, each request/response/notification is wrapped in an envelope
structure. There is a separate envelope for incoming and outgoing messages:

```idl
namespace org.enso.languageserver.protocol.binary;

//A mapping between payload enum and inbound payload types.
union InboundPayload {
  INIT_SESSION_CMD: InitSessionCommand,
  WRITE_FILE_CMD: WriteFileCommand,
  READ_FILE_CMD: ReadFileCommand
}

//An envelope for inbound requests and commands.
table InboundMessage {

  //A unique id of the message sent to the server.
  messageId: EnsoUUID (required);

  //An optional correlation id used to correlate a response with a request.
  correlationId: EnsoUUID;

  //A message payload that carries requests sent by a client.
  payload: InboundPayload (required);

}
```

```idl
namespace org.enso.languageserver.protocol.binary;

//A mapping between payload enum and outbound payload types.
union OutboundPayload {
  ERROR: Error,
  SUCCESS: Success,
  VISUALIZATION_UPDATE: VisualizationUpdate,
  FILE_CONTENTS_REPLY: FileContentsReply
}

//An envelope for outbound responses.
table OutboundMessage {

  //A unique id of the message sent from the server.
  messageId: EnsoUUID (required);

  //An optional correlation id used to correlate a response with a request.
  correlationId: EnsoUUID;

  //A message payload that carries responses and notifications sent by a server
  payload: OutboundPayload (required);

}
```

```idl
namespace org.enso.languageserver.protocol.binary;

//This message type is used to indicate failure of some operation performed.
table Error {

  //A unique error code identifying error type.
  code: int;

  //An error message.
  message: string;

}

//Indicates an operation has succeeded.
table Success {}
```

### Binary Protocol Communication Patterns

The binary protocol currently only supports a single type of communication
pattern:

- **Push:** Messages containing data are pushed in response to operations
  performed using the textual protocol.

### Binary Protocol Transport

The binary protocol uses [flatbuffers](https://github.com/google/flatbuffers)
for the protocol transport format. This choice has been made for a few reasons:

- Robust multi-language support, including Rust and Java on the JVM.
- High performance, including support for zero-copy data handling and streaming
  data.
- Robust, schema-based messages.

## Binary Protocol Functionality

The binary protocol exists in order to serve the high-bandwidth data transfer
requirements of the engine and the GUI.

### Displaying Visualizations

A major part of Enso Studio's functionality is the rich embedded visualizations
that it supports. This means that the following functionality is necessary:

- Execution of an arbitrary Enso expression on a cached value designated by a
  source location.
- The ability to create and destroy visualization subscriptions with an
  arbitrary piece of Enso code as the preprocessing function.
- The ability to update _existing_ subscriptions with a new preprocessing
  function.

Visualizations in Enso are able to output arbitrary data for display in the GUI,
which requires a mechanism for transferring arbitrary data between the engine
and the GUI. These visualizations can output data in common formats, which will
be serialised by the transport (e.g. text), but they can also write arbitrary
binary data that can then be interpreted by the visualization component itself
in any language that can be used from within the IDE.

From the implementation perspective:

- This will need to be an entirely separate set of protocol messages that should
  be specified in detail in this document.
- Visualizations should work on a pub/sub model, where an update is sent every
  time the underlying data is recomputed.
- Protocol responses must contain a pointer into the binary pipe carrying the
  visualization data to identify an update.

## Service Connection Setup

As these services need to support multiple clients in future, there is some
rigmarole around setting up the various connections needed by each client. The
process for spawning and connecting to an engine instance is as follows:

1.  **Spawn the Server:** The project manager spawns the language server,
    passing the socket information as part of the initialisation flow.
2.  **Client ID Generation:** The client generates and stores a UUID that will
    be used to identify the client while it is connected.
3.  **Protocol Connection Initialisation:** The client performs the init for the
    textual protocol connection, passing its client identifier as it does so.
    See
    [`session/initProtocolConnection`](./protocol-language-server.md#sessioninitprotocolconnection)
    for more information.
4.  **Data Connection Initialisation:** The client performs the init for the
    data connection, passing its client identifier as it does so. See
    [`session/initDataConnection`](./protocol-language-server.md#sessioninitdataconnection)
    below more information.

## Service Connection Teardown

As the engine performs sophisticated caching and persisting of data where
possible, it is very important that the client informs the engine of the end of
its session. In contrast to the initialisation flow above, this is not an
involved process.

1.  **Notify the Engine:** _Prior_ to disconnecting from the sockets, the client
    must send `session/end` to the server.
2.  **Disconnect:** Once that message has been sent, the client may disconnect
    at any time.
