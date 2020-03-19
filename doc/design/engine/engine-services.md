# The Enso Engine Services
Enso is a sophisticated language, but in order to provide a great user
experience to our users we also need the ability to provide great tooling. This
tooling means a language server, but it also means a set of extra peripheral
components that ensure we can run Enso in a way that the product requires.

These services are responsible for providing the whole-host of language- and
project-level tooling to the IDE components, whether they're hosted in the cloud
or locally on a user's machine.

To that end, we need to have a well-specified idea of what the various services
do, and how they interact. This document contains a design for the engine
services components, as well as any open questions that may remain.

<!-- MarkdownTOC levels="2,3,4" autolink="true" -->

- [Architecture](#architecture)
  - [The Project Picker](#the-project-picker)
  - [Language Server](#language-server)
- [The Protocol Itself](#the-protocol-itself)
  - [Protocol Communication Patterns](#protocol-communication-patterns)
  - [The Protocol Transport](#the-protocol-transport)
  - [The Protocol Format](#the-protocol-format)
- [Protocol Functionality](#protocol-functionality)
  - [Textual Diff Management](#textual-diff-management)
  - [Handling Multiple Clients](#handling-multiple-clients)
  - [Project State Management](#project-state-management)
  - [File Management and Storage](#file-management-and-storage)
  - [Execution Management](#execution-management)
    - [Caching](#caching)
    - [Progress Reporting](#progress-reporting)
  - [Visualisation Support](#visualisation-support)
  - [Completion](#completion)
  - [Analysis Operations](#analysis-operations)
  - [Functionality Post 2.0](#functionality-post-20)
- [Protocol Message Specification - Common Types](#protocol-message-specification---common-types)
    - [`Path`](#path)
    - [`AbsolutePath`](#absolutepath)
- [Protocol Message Specification - Project Picker](#protocol-message-specification---project-picker)
  - [Types](#types)
    - [`ProjectMetadata`](#projectmetadata)
  - [Project Management Operations](#project-management-operations)
    - [`project/open`](#projectopen)
    - [`project/close`](#projectclose)
    - [`project/listRecent`](#projectlistrecent)
    - [`project/create`](#projectcreate)
    - [`project/delete`](#projectdelete)
    - [`project/listSample`](#projectlistsample)
  - [Language Server Management](#language-server-management)
  - [Errors - Project Manager](#errors---project-manager)
- [Protocol Message Specification - Language Server](#protocol-message-specification---language-server)
  - [Types](#types-1)
    - [`File`](#file)
    - [`DirectoryTree`](#directorytree)
    - [`FileAttributes`](#fileattributes)
    - [`FileEventKind`](#fileeventkind)
    - [`Position`](#position)
    - [`Range`](#range)
    - [`TextEdit`](#textedit)
    - [`FileEdit`](#fileedit)
    - [`FileContents`](#filecontents)
    - [`FileSystemObject`](#filesystemobject)
    - [`WorkspaceEdit`](#workspaceedit)
  - [Capability Management](#capability-management)
    - [`capability/acquire`](#capabilityacquire)
    - [`capability/release`](#capabilityrelease)
    - [`capability/granted`](#capabilitygranted)
    - [`capability/forceReleased`](#capabilityforcereleased)
  - [Capabilities](#capabilities)
    - [`text/canEdit`](#textcanedit)
    - [`file/receivesTreeUpdates`](#filereceivestreeupdates)
    - [`executionContext/canModify`](#executioncontextcanmodify)
    - [`executionContext/receiveUpdates`](#executioncontextreceiveupdates)
  - [File Management Operations](#file-management-operations)
    - [`file/write`](#filewrite)
    - [`file/read`](#fileread)
    - [`file/create`](#filecreate)
    - [`file/delete`](#filedelete)
    - [`file/copy`](#filecopy)
    - [`file/move`](#filemove)
    - [`file/exists`](#fileexists)
    - [`file/tree`](#filetree)
    - [`file/list`](#filelist)
    - [`file/info`](#fileinfo)
    - [`file/event`](#fileevent)
    - [`file/addRoot`](#fileaddroot)
    - [`file/removeRoot`](#fileremoveroot)
    - [`file/rootAdded`](#filerootadded)
    - [`file/rootRemoved`](#filerootremoved)
  - [Text Editing Operations](#text-editing-operations)
    - [`text/openFile`](#textopenfile)
    - [`text/closeFile`](#textclosefile)
    - [`text/save`](#textsave)
    - [`text/applyEdit`](#textapplyedit)
    - [`text/didChange`](#textdidchange)
  - [Workspace Operations](#workspace-operations)
    - [`workspace/connect`](#workspaceconnect)
    - [`workspace/undo`](#workspaceundo)
    - [`workspace/redo`](#workspaceredo)
  - [Execution Management](#execution-management-1)
    - [Types](#types-2)
    - [`executionContext/create`](#executioncontextcreate)
    - [`executionContext/destroy`](#executioncontextdestroy)
    - [`executionContext/fork`](#executioncontextfork)
    - [`executionContext/push`](#executioncontextpush)
    - [`executionContext/pop`](#executioncontextpop)
    - [`executionContext/recompute`](#executioncontextrecompute)
    - [`executionContext/expressionValuesComputed`](#executioncontextexpressionvaluescomputed)
  - [Errors - Language Server](#errors---language-server)

<!-- /MarkdownTOC -->

## Architecture
The divisions of responsibility between the backend engine services are dictated
purely by necessity. As multi-client editing necessitates careful
synchronisation and conflict resolution, between the actions of multiple
clients. This section deals with the intended architecture for the Engine
Services.

The engine services are divided into two main components:

1. **The Project Picker:** This component is responsible for listing and
   managing user projects, as well as spawning the language server for a given
   project when it is opened.
2. **The Language Server:** This component is responsible for dealing with
   incoming connections and resolving conflicts between multiple clients. It is
   also responsible for servicing all of the requests from the clients.

Both components will be implemented as akka actors such that we can defer the
decision as to run them in different processes until the requirements become
more clear.

### The Project Picker
The project picker service is responsible for both allowing users to work with
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

## The Protocol Itself
The protocol refers to the communication format that all of the above services
speak between each other and to the GUI. This protocol is not specialised only
to language server operations, as instead it needs to work for all of the
various services in this set.

The protocol we are using intends to be fully compatible with the Microsoft LSP
[specification](https://microsoft.github.io/language-server-protocol/specifications/specification-3-145) (version 3.15). In essence, we will operate
as follows:

- Where our use case matches with a function provided by LSP, we will use the
  specified LSP message (e.g. completions).
- Where our use-case does not match a message provided by LSP, we will use the
  following process:
  1. If we can implement this on top of one of LSP's extensible mechanisms (e.g.
     commands) we will do so.
  2. If this is not possible, we will specify an _extension_ to the protocol.
     This extension will be well-specified within this document, and should be
     in the spirit of the existing protocol. If relevant, we may propose it as
     a future extension to the specification.

Aside from the language server protocol-based operations, we will definitely
need a protocol extension

### Protocol Communication Patterns
Whatever protocol we decide on will need to have support for a couple of main
communication patterns:

- **Pub/Sub:** A standard publisher/subscriber model, the server will need to be
  able to support this kind of connection to deal with events that do not occur
  strictly in response to client actions (e.g. updates to observed values).
- **Req/Res:** A standard request/response model, the server will need to be
  able to support this kind of connection to deal with one-off requests from
  the client, and potentially to make requests to the client (e.g. list modules
  in the current project, please refresh your file state).

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

### The Protocol Transport
The transport of the protocol refers to the underlying layer over which its
messages (discussed in [the protocol format](#the-protocol-format) below) are
sent. As we are maintaining compatibility with LSP, the protocol transport
format is already defined for us.

- Textual messages are sent using [JSON-RPC](https://en.wikipedia.org/wiki/JSON-RPC) over a WebSocket connection (as defined in the LSP spec).
- As a protocol extension we also negotiate a secondary binary WebSocket
  connection for sending visualisation data. This transport is independent of
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
  (for visualisations) over a second WebSocket connection.

This means that we have two pipes: one is the textual WebSocket defined by LSP,
and the other is a binary WebSocket.

## Protocol Functionality
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
  + Execution state (whether the node is being computed or is cached)
  + Profiling information
  + Values
  + Types
  + Where we are in the call stack (useful for recursive execution)
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
- These would be sent as visualisations for use in the IDE.

LSP provides an inbuilt mechanism for reporting progress, but that will not work
with visualisations. As a result that should be reserved for reporting progress
of long-running operations within the _language server_ rather than in user
code.

### Visualisation Support
A major part of Enso Studio's functionality is the rich embedded visualisations
that it supports. This means that the following functionality is necessary:

- Execution of an arbitrary Enso expression on a cached value designated by
  a source location.
- The ability to create and destroy visualisation subscriptions with an
  arbitrary piece of Enso code as the preprocessing function.
- The ability to update _existing_ subscriptions with a new preprocessing
  function.

From the implementation perspective:

- This will need to be an entirely separate set of protocol messages that should
  be specified in detail in this document.
- Visualisations should work on a pub/sub model, where an update is sent every
  time the underlying data is recomputed.
- Protocol responses must contain a pointer into the binary pipe carrying the
  visualisation data to identify an update.

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
- **Arbitrary Visualisation Code:** Visualisations should be able to be defined
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
  standard [debug adapter protocol](https://microsoft.github.io/debug-adapter-protocol/specification).
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

## Protocol Message Specification - Common Types
There are a number of types that are shared between many of the protocol
messages. They are specified below.

#### `Path`
A path is a representation of a path relative to a specified content root.

##### Format
Please note that segments can only be ordinary file names, `..` and `.` may not be supported.

```typescript
interface Path {
  rootId: UUID;
  segments: [String];
}
```

## Protocol Message Specification - Project Picker
This section exists to contain a specification of each of the messages that the
project picker supports. This is in order to aid in the proper creation of
clients, and to serve as an agreed-upon definition for the protocol between the
IDE and Engine teams.

> The actionables for this section are:
>
> - As we establish the _exact_ format for each of the messages supported by the
>   services, record the details of each message here.

### Types
There are a number of types that are used only within the project server's
protocol messages. These are specified here.

#### `ProjectMetadata`
This type represents information about a project.

##### Format

```typescript
interface ProjectMetadata {
  name: String;
  id: UUID;
  size: Size;
  lastOpened: UTCDateTime;
  path: Path | URI;
}
```

### Project Management Operations
The primary responsibility of the project pickers is to allow users to manage
their projects.

#### `project/open`
This message requests that the project picker open a specified project. This
operation also includes spawning an instance of the language server open on the
specified project.

- **Type:** Request
- **Direction:** Client -> Server

##### Parameters

```typescript
interface ProjectOpenRequest {
  projectId: UUID;
}
```

##### Result

```typescript
interface ProjectOpenResult {
  lsAddress: IPWithSocket;
}
```

##### Errors
TBC

#### `project/close`
This message requests that the project picker close a specified project. This
operation includes shutting down the language server gracefully so that it can
persist state to disk as needed.

- **Type:** Request
- **Direction:** Client -> Server

##### Parameters

```typescript
interface ProjectCloseRequest {
  projectId: UUID;
}
```

##### Result

```typescript
{}
```

##### Errors
TBC

#### `project/listRecent`
This message requests that the project picker lists the user's most recently
opened projects.

- **Type:** Request
- **Direction:** Client -> Server

##### Parameters

```typescript
interface ProjectListRecentRequest {
  numProjects: Int;
}
```

##### Result

```typescript
interface ProjectListRecentResponse {
  projects: [ProjectMetadata];
}
```

##### Errors
TBC

#### `project/create`
This message requests the creation of a new project.

- **Type:** Request
- **Direction:** Client -> Server

##### Parameters

```typescript
interface ProjectCreateRequest {
  name: String;
  location: SystemPath | URI;
}
```

##### Result

```typescript
{}
```

##### Errors
TBC

#### `project/delete`
This message requests the deletion of a project.

- **Type:** Request
- **Direction:** Client -> Server

##### Parameters

```typescript
interface ProjectDeleteRequest {
  projectId: UUID;
}
```

##### Result

```typescript
{}
```

##### Errors
TBC

#### `project/listSample`
This request lists the sample projects that are available to the user.

- **Type:** Request
- **Direction:** Client -> Server

##### Parameters

```typescript
interface ProjectListSampleRequest {
  numProjects: Int;
}
```

##### Result

```typescript
interface ProjectListSampleResponse {
  projects: [ProjectMetadata];
}
```

##### Errors
TBC

### Language Server Management
The project picker is also responsible for managing the language server. This
means that it needs to be able to spawn the process, but also tell the process
when to shut down.

> The actionables for this section are:
>
> - Fill it in when we have more of an idea about exactly how this spawning
>   relationship is going to work.

### Errors - Project Manager
The project picker component also has its own set of errors. This section is not
a complete specification and will be updated as new errors are added.

## Protocol Message Specification - Language Server
This section exists to contain a specification of each of the messages that the
language server supports. This is in order to aid in the proper creation of
clients, and to serve as an agreed-upon definition for the protocol between the
IDE and Engine teams.

> The actionables for this section are:
>
> - As we establish the _exact_ format for each of the messages supported by the
>   services, record the details of each message here.
> - This should always be done, but may reference LSP.

### Types
There are a number of types that are used only within the language server's
protocol messages. These are specified here.

#### `File`
A representation of a file on disk.

##### Format

```typescript
interface File {
  name: String; // Includes the file extension
  type: String;
}
```

#### `DirectoryTree`
A directory tree is a recursive type used to represent tree structures of files
and directories.

##### Format

```typescript
interface DirectoryTree {
  path: Path;
  name: String;
  files: [FileSystemObject];
  directories: [DirectoryTree];
}
```

#### `FileAttributes`
A description of the attributes of a file required by the IDE. These attributes
may be expanded in future.

##### Format

```typescript
interface FileAttributes {
  creationTime: UTCDateTime;
  lastAccessTime: UTCDateTime;
  lastModifiedTime: UTCDateTime;
  kind: FileSystemObject;
  byteSize: Size;
}
```

#### `FileEventKind`
The kind of event being described for a watched file.

##### Format

```typescript
type FileEventKind = Added | Removed | Modified;
```

#### `Position`
A representation of a position in a text file.

##### Format

```typescript
interface Position {
  /**
   * Line position in a document (zero-based).
   */
  line: number;

  /**
   * Character offset on a line in a document (zero-based). Assuming that the line is
   * represented as a string, the `character` value represents the gap between the
   * `character` and `character + 1`.
   *
   * If the character value is greater than the line length it defaults back to the
   * line length.
   */
  character: number;
}
```

#### `Range`
A representation of a range of text in a text file.

##### Format

```typescript
interface Range {
  /**
   * The range's start position.
   */
  start: Position;

  /**
   * The range's end position.
   */
  end: Position;
}
```

#### `TextEdit`
A representation of a change to a text file at a given position.

##### Format

```typescript
interface TextEdit {
  range: Range;
  text: String;
}
```

#### `FileEdit`
A representation of a batch of edits to a file, versioned.

##### Format

```typescript
interface FileEdit {
  path: Path;
  edits: [TextEdit];
  oldVersion: SHA3-224;
  newVersion: SHA3-224;
}
```

#### `FileContents`
A representation of the contents of a file.

##### Format

```typescript
interface FileContents[T] {
  contents: T;
}

class TextFileContents extends FileContents[String];
```

#### `FileSystemObject`
A representation of what kind of type a filesystem object can be.

##### Format

```typescript
type FileSystemObject
  = Directory
  | DirectoryTruncated
  | SymlinkLoop
  | File
  | Other;

/**
 * Represents a directory.
 *
 * @param name a name of the directory
 * @param path a path to the directory
 */
interface Directory {
  name: String;
  path: Path;
}

/**
 * Represents a directory which contents have been truncated.
 *
 * @param name a name of the directory
 * @param path a path to the directory
 */
interface DirectoryTruncated {
  name: String;
  path: Path;
}

/**
 * Represents a symbolic link that creates a loop.
 *
 * @param name a name of the symlink
 * @param path a path to the symlink
 * @param target a target of the symlink. Since it is a loop,
 * target is a subpath of the symlink
 */
interface SymlinkLoop {
  name: String;
  path: Path;
  target: Path;
}

/**
 * Represents a file.
 *
 * @param name a name of the file
 * @param path a path to the file
 */
interface File {
  name: String;
  path: Path;
}

/**
 * Represents unrecognized object.
 * Example is a broken symbolic link.
 */
interface Other {
  name: String;
  path: Path;
;
```

#### `WorkspaceEdit`
This is a message to be specified once we better understand the intricacies of
undo/redo.

### Capability Management
In order to mediate between multiple clients properly, the language server has
a robust notion of capability management to grant and remove permissions from
clients.

#### `capability/acquire`
This requests that the server grant the specified capability to the requesting
client.

- **Type:** Request
- **Direction:** Client -> Server

##### Parameters

```typescript
{
  registration: CapabilityRegistration;
}

interface CapabilityRegistration {
  method: String;
  registerOptions?: any;
}
```

The `registerOptions` are determined by the `method`. The method must be listed
in the section on [capabilities](#capabilities) below.

##### Result

```typescript
null
```

##### Errors
TBC

#### `capability/release`
This requests that the server acknowledge that the client is releasing a given
capability.

- **Type:** Request
- **Direction:** Client -> Server

##### Parameters

```typescript
{
  registration: CapabilityRegistration;
}
```

##### Result

```typescript
null
```

##### Errors
TBC

#### `capability/granted`
This notifies the client that it has been granted a capability without any
action on its part.

- **Type:** Notification
- **Direction:** Server -> Client

##### Parameters

```typescript
{
  registration: CapabilityRegistration;
}
```

##### Errors
TBC

#### `capability/forceReleased`
This notifies the client that a capability has been forcibly removed from its
capability set.

- **Type:** Notification
- **Direction:** Server -> Client

##### Parameters

```typescript
{
  registration: CapabilityRegistration;
}
```

##### Errors
TBC

### Capabilities
The capability management features work with the following capabilities.

#### `text/canEdit`
This capability states that the capability has the ability to perform both
`text/applyEdit` and `text/save` for the specified file.

- **method:** `text/canEdit`
- **registerOptions:** `{path: Path;}`

##### Enables
- `text/applyEdit`
- `text/save`

##### Disables
None

#### `file/receivesTreeUpdates`
This capability states that the client will receive updates for any watched
content roots in the current project.

- **method:** `file/receivesTreeUpdates`
- **registerOptions:** `{}`

##### Enables
- `file/event`

##### Disables
None

#### `executionContext/canModify`
This capability states that the client has the ability to modify an execution
context, including modifying the execution stack, invalidating caches, or
destroying the context.

- **method:** `executionContext/canModify`
- **registerOptions:** `{  contextId: ContextId; }`

##### Enables
- `executionContext/destroy`
- `executionContext/recompute`
- `executionContext/push`
- `executionContext/pop`

##### Disables
None

#### `executionContext/receiveUpdates`
This capability states that the client receives expression value updates from
a given execution context.

- **method:** `executionContext/receiveUpdates`
- **registerOptions:** `{  contextId: ContextId; }`

##### Enables
- `executionContext/expressionValuesComputed`

##### Disables
None

### File Management Operations
The language server also provides file operations to the IDE.

#### `file/write`
This requests that the file manager component write to a specified file with
the specified contents.

- **Type:** Request
- **Direction:** Client -> Server

This request is _explicitly_ allowed to write to files that do not exist, and
will create them under such circumstances. If a file is recorded as 'open' by
one of the clients, and another client attempts to write to that file, the
write must fail.

##### Parameters

```typescript
{
  path: Path;
  contents: FileContents[T];
}
```

##### Result

```typescript
null
```

##### Errors

- [`FileSystemError`](#filesystemerror) to signal a generic, unrecoverable file-system error.
- [`ContentRootNotFoundError`](#contentrootnotfounderror) to signal that the requested content root cannot be found.
- [`AccessDeniedError`](#accessdeniederror) to signal that a user doesn't have access to a resource.

#### `file/read`
This requests that the file manager component reads the contents of a specified
file.

- **Type:** Request
- **Direction:** Client -> Server

If the file is recorded as open by the language server, then the result will
return the contents from the in-memory buffer rather than the file on disk.

##### Parameters

```typescript
{
  path: Path;
}
```

##### Result

```typescript
{
  contents: FileContents[T]
}
```

##### Errors

- [`FileSystemError`](#filesystemerror) to signal a generic, unrecoverable file-system error.
- [`ContentRootNotFoundError`](#contentrootnotfounderror) to signal that the requested content root cannot be found.
- [`AccessDeniedError`](#accessdeniederror) to signal that a user doesn't have access to a resource.
- [`FileNotFound`](#filenotfound) informs that file cannot be found.

#### `file/create`
This request asks the file manager to create the specified file system object.

- **Type:** Request
- **Direction:** Client -> Server

This will fail if the specified object already exists.

##### Parameters

```typescript
{
  object: FileSystemObject;
}
```

##### Response

```typescript
null
```

##### Errors

- [`FileSystemError`](#filesystemerror) to signal a generic, unrecoverable file-system error.
- [`ContentRootNotFoundError`](#contentrootnotfounderror) to signal that the requested content root cannot be found.
- [`AccessDeniedError`](#accessdeniederror) to signal that a user doesn't have access to a resource.

#### `file/delete`
This request asks the file manager to delete the specified file system object.

- **Type:** Request
- **Direction:** Client -> Server

##### Parameters

```typescript
{
  path: Path;
}
```

##### Result

```
null
```

##### Errors
- [`FileSystemError`](#filesystemerror) to signal a generic, unrecoverable file-system error.
- [`ContentRootNotFoundError`](#contentrootnotfounderror) to signal that the requested content root cannot be found.
- [`FileNotFound`](#filenotfound) informs that file cannot be found.
- [`FileExists`](#fileexists) informs that file already exists

#### `file/copy`
This request asks the file manager to copy a specified filesystem object to
another location.

- **Type:** Request
- **Direction:** Client -> Server

##### Parameters

```typescript
{
  from: Path;
  to: Path;
}
```

##### Result

```typescript
null
```

##### Errors
- [`FileSystemError`](#filesystemerror) to signal a generic, unrecoverable file-system error.
- [`ContentRootNotFoundError`](#contentrootnotfounderror) to signal that the requested content root cannot be found.
- [`FileNotFound`](#filenotfound) informs that file cannot be found.

#### `file/move`
This request asks the file manager to move a specified filesystem object to
another location.

- **Type:** Request
- **Direction:** Client -> Server

The move should be specified by filesystem events, and such notifications should
inform the client that the currently edited file has been moved.

##### Parameters

```typescript
{
  from: Path;
  to: Path;
}
```

##### Result

```typescript
null
```

##### Errors
- [`FileSystemError`](#filesystemerror) to signal a generic, unrecoverable file-system error.
- [`ContentRootNotFoundError`](#contentrootnotfounderror) to signal that the requested content root cannot be found.
- [`FileNotFound`](#filenotfound) informs that file cannot be found.
- [`FileExists`](#fileexists) informs that target file already exists.

#### `file/exists`
This request asks the file manager to check whether a filesystem object exists
at the specified path.

- **Type:** Request
- **Direction:** Client -> Server

##### Parameters

```typescript
{
  path: Path;
}
```

##### Result

```typescript
{
  exists: Boolean;
}
```

##### Errors
- [`ContentRootNotFoundError`](#contentrootnotfounderror) to signal that the
  requested content root cannot be found.

#### `file/tree`
This request asks the file manager component to generate and provide the
directory tree starting at a given path.

- **Type:** Request
- **Direction:** Client -> Server

For trees that exceed the provided `depth`, the result should be truncated, and
the corresponding flag should be set.

##### Parameters

```typescript
{
  path: Path;
  depth?: Int;
}
```

##### Result

```typescript
{
  tree: DirectoryTree;
}
```

##### Errors
- [`ContentRootNotFoundError`](#contentrootnotfounderror) to signal that the
  requested content root cannot be found.
- [`FileNotFound`](#filenotfound) informs that requested path does not exist or
  provided depth argument is <= 0.
- [`NotDirectory`](#notdirectory) informs that requested path is not a
  directory.

#### `file/list`
This request lists the contents of a given filesystem object. For a file it will
just return the file, while for a directory it will list the contents of the
directory.

- **Type:** Request
- **Direction:** Client -> Server

##### Parameters

```typescript
{
  path: Path;
}
```

##### Result

```typescript
{
  paths: [FileSystemObject];
}
```

##### Errors
- [`ContentRootNotFoundError`](#contentrootnotfounderror) to signal that the
  requested content root cannot be found.
- [`FileNotFound`](#filenotfound) informs that requested path does not exist.
- [`NotDirectory`](#notdirectory) informs that requested path is not a
  directory.

#### `file/info`
This request gets information about a specified filesystem object.

- **Type:** Request
- **Direction:** Client -> Server

This request should work for all kinds of filesystem object.

##### Parameters

```typescript
{
  path: Path;
}
```

##### Result

```typescript
{
  attributes: Attributes;
}
```

##### Errors
TBC

#### `file/event`
This is a notification that is sent every time something under a watched content
root changes. It is used to ensure that the client's filesystem representation
stays in synchronisation with reality.

- **Type:** Notification
- **Direction:** Server -> Client

Events should be sent from server to client for every event observed under one
of the (possibly multiple) content roots.

##### Parameters

```typescript
{
  object: FileSystemObject;
  kind: FileEventKind;
}
```

##### Errors
TBC

#### `file/addRoot`
This request adds a content root to the active project.

- **Type:** Request
- **Direction:** Client -> Server

When a content root is added, the language server must notify clients other than
the one that added the root by sending a `file/rootAdded`. Additionally, all
clients must be notified with a `file/event` about the addition of the new root.
The IDE is responsible for calling `file/tree` on that root to discover its
structure.

##### Parameters

```typescript
{
  absolutePath: [String];
  id: UUID; // The ID of the content root
}
```

##### Result

```typescript
null
```

##### Errors
TBC

#### `file/removeRoot`
This request removes a content root from the active project.

- **Type:** Request
- **Direction:** Client -> Server

When a content root is removed, the language server must notify clients other
than the one that added the root by sending a `file/rootRemoved`. Additionally,
the server must send a `file/event` making the root of the new tree visible. The
IDE is responsible for any additional discovery.

##### Parameters

```typescript
{
  id: UUID; // The content root ID
}
```

##### Result

```typescript
null
```

##### Errors
TBC

#### `file/rootAdded`
This is a notification sent to all clients other than the one performing the
addition of the root in order to inform them of the content root's ID.

- **Type:** Notification
- **Direction:** Server -> Client

##### Parameters

```typescript
{
  id: UUID; // The content root ID
  absolutePath: [String]
}
```

##### Errors
TBC

#### `file/rootRemoved`
This is a notification sent to all clients other than the one performing the
removal of the content root in order to inform them of the removal of the root.

- **Type:** Notification
- **Direction:** Server -> Client

##### Parameters

```typescript
{
  id: UUID; // The content root ID
}
```

##### Errors
TBC

### Text Editing Operations
The language server also has a set of text editing operations to ensure that it
stays in sync with the clients.

#### `text/openFile`
This request informs the language server that a client has opened the specified
file.

- **Type:** Request
- **Direction:** Client -> Server

If no client has write lock on the opened file, the capability is granted to
the client that sent the `text/openFile` message.

##### Parameters

```typescript
{
  path: Path;
}
```

##### Result

```typescript
{
  writeCapability?: CapabilityRegistration;
  content: String;
  currentVersion: SHA3-224;
}
```

##### Errors
- [`FileSystemError`](#filesystemerror) to signal a generic, unrecoverable file-system error.
- [`ContentRootNotFoundError`](#contentrootnotfounderror) to signal that the requested content root cannot be found.
- [`AccessDeniedError`](#accessdeniederror) to signal that a user doesn't have access to a resource.
- [`FileNotFound`](#filenotfound) informs that file cannot be found.


#### `text/closeFile`
This request informs the language server that a client has closed the specified
file.

- **Type:** Request
- **Direction:** Client -> Server

##### Parameters

```typescript
{
  path: Path;
}
```

##### Result

```typescript
null
```

##### Errors
- [`FileNotOpenedError`](#filenotopenederror) to signal that a file wasn't
opened.

#### `text/save`
This requests for the language server to save the specified file.

- **Type:** Request
- **Direction:** Client -> Server

The request may fail if the requesting client does not have permission to edit
that file, or if the client is requesting a save of an outdated version.

##### Parameters

```typescript
{
  path: Path;
  currentVersion: SHA3-224;
}
```

##### Result

```typescript
null
```

##### Errors
- [`FileNotOpenedError`](#filenotopenederror) to signal that the file isn't
open.
- [`InvalidVersionError`](#invalidversionerror) to signal that the version provided by the client doesn't match the version
computed by the server.
- [`WriteDeniedError`](#writedeniederror) to signal that the client doesn't hold write lock for the buffer.
- [`FileSystemError`](#filesystemerror) to signal a generic, unrecoverable file-system error.
- [`ContentRootNotFoundError`](#contentrootnotfounderror) to signal that the requested content root cannot be found.
- [`AccessDeniedError`](#accessdeniederror) to signal that the user doesn't have access to a resource.

#### `text/applyEdit`
This requests that the server apply a series of edits to the project. These
edits solely concern text files.

- **Type:** Request
- **Direction:** Client -> Server

This operation may fail if the requesting client does not have permission to
edit the resources for which edits are sent. This failure _may_ be partial, in
that some edits are applied and others are not.

##### Parameters

```typescript
{
  edit: FileEdit;
}
```

##### Result

```typescript
null
```

##### Errors
- [`FileNotOpenedError`](#filenotopenederror) to signal that the file isn't
open.
- [`TextEditValidationError`](#texteditvalidationerror) to signal that validation has failed for a series of edits.
- [`InvalidVersionError`](#invalidversionerror) to signal that the version provided by the client doesn't match the version
computed by the server.
- [`WriteDeniedError`](#writedeniederror) to signal that the client doesn't hold write lock for the buffer.

#### `text/didChange`
This is a notification sent from the server to the clients to inform them of any
changes made to files that they have open.

- **Type:** Notification
- **Direction:** Server -> Client

This notification must _only_ be sent for files that the client has open.

##### Parameters

```typescript
{
  edits: [FileEdit];
}
```

##### Errors
```typescript
null
```

### Workspace Operations
The language server also has a set of operations useful for managing the client
workspace.

#### `workspace/connect`
This is a request sent from the client to the server when it first connects to
the server process, allowing it to obtain some initial information.

- **Type:** Request
- **Direction:** Client -> Server

##### Parameters

```typescript
null
```

##### Result

```typescript
{
  contentRoots: [{id: UUID; absPath: [String]}]
}
```

##### Errors
TBC

#### `workspace/undo`
This request is sent from the client to the server to request that an operation
be undone.

- **Type:** Request
- **Direction:** Client -> Server

The exact behaviour of this message is to be determined, but it must involve the
server undoing that same action for all clients in the workspace.

##### Parameters

```typescript
{
  requestID?: UUID; // If not specified, it undoes the latest request
}
```

##### Result

```typescript
null
```

##### Errors
TBC

#### `workspace/redo`
This request is sent from the client to the server to request that an operation
be redone.

- **Type:** Request
- **Direction:** Client -> Server

The exact behaviour of this message is to be determined, but it must involve the
server redoing that same action for all clients in the workspace.

##### Parameters

```typescript
{
  requestID?: UUID; // If not specified, it redoes the latest request
}
```

##### Result

```typescript
null
```

##### Errors
TBC

### Execution Management
The execution management portion of the language server API deals with exposing
fine-grained control over program and expression execution to the clients of
the language server. This is incredibly important for enabling the high levels
of interactivity required by Enso Studio.

#### Types
The execution management API exposes a set of common types used by many of its
messages.

##### `ExpressionId`
An identifier used for Enso expressions.

```typescript
type ExpressionId = UUID;
```

##### `ContextId`
An identifier used for execution contexts.

```typescript
type ContextId = UUID;
```

##### `StackItem`
A representation of an executable position in code, used by the execution APIs.

`ExplicitCall` is a call performed at the top of the stack, to initialize the
context with first execution.
The `thisArgumentsPosition` field can be omitted, in which case the context
will try to infer the argument on a best-effort basis. E.g. for a module-level
method, or a method defined on a parameter-less atom type, `this` will be
substituted for the unambiguous singleton instance.

`LocalCall` is a call corresponding to "entering a function call".

```typescript
type StackItem = ExplicitCall | LocalCall

interface ExplicitCall {
  methodPointer: MethodPointer;
  thisArgumentExpression?: String;
  positionalArgumentsExpressions: String[];
}

interface LocalCall {
  expressionId: ExpressionId;
}
```

##### `MethodPointer`
Points to a method definition.

```typescript
{
  file: Path;
  definedOnType: String;
  name: String;
}
```

##### `ExpressionValueUpdate`

```typescript
{
  id: ExpressionId;
  type?: String;
  shortValue?: String;
  methodCall?: MethodPointer;
}
```

#### `executionContext/create`
Sent from the client to the server to create a new execution context.

##### Parameters
```typescript
{
  contextId: ContextId;
}
```

##### Result
```typescript
{
  canModify: CapabilityRegistration;
  receivesEvents: CapabilityRegistration;
}
```

##### Errors
No known errors.

#### `executionContext/destroy`
Sent from the client to the server destroy an execution context and free its
resources.

##### Parameters
```typescript
{
  contextId: ContextId;
}
```

##### Result
```typescript
null
```

##### Errors
- [`AccessDeniedError`](#accessdeniederror) when the user does not hold the
  `executionContext/canModify` capability for this context.

#### `executionContext/fork`
Sent from the client to the server to duplicate an execution context, creating
an independent copy, containing all the data precomputed in the first one.

##### Parameters
```typescript
{
  contextId: ContextId;
  newContextId: ContextId;
}
```

##### Result
```typescript
{
  canModify: CapabilityRegistration;
  receivesEvents: CapabilityRegistration;
}
```

##### Errors
No known errors.

#### `executionContext/push`
Sent from the client to the server move the execution context to a new location
deeper down the stack.

##### Parameters
```typescript
{
  contextId: ContextId;
  stackItem: StackItem;
}
```

##### Result
```typescript
null
```

##### Errors
- [`StackItemNotFoundError`](#stackitemnotfounderror) when the request stack
  item could not be found.
- [`AccessDeniedError`](#accessdeniederror) when the user does not hold the
  `executionContext/canModify` capability for this context.


#### `executionContext/pop`
Sent from the client to the server move the execution context up the stack,
corresponding to the client clicking out of the current breadcrumb.

##### Parameters
```typescript
{
  contextId: ContextId;
}
```

##### Result
```typescript
null
```

##### Errors
- [`AccessDeniedError`](#accessdeniederror) when the user does not hold the
  `executionContext/canModify` capability for this context.

#### `executionContext/recompute`
Sent from the client to the server to force recomputation of current position.
May include a list of expressions for which caches should be invalidated.

##### Parameters
```typescript
{
  contextId: ContextId;
  invalidatedExpressions?: "all" | ExpressionId[]
}
```

##### Result
```typescript
null
```

##### Errors
No known errors.

#### `executionContext/expressionValuesComputed`
Sent from the server to the client to inform about new information for certain
expressions becoming available.

##### Parameters
```typescript
{
  contextId: ContextId;
  updates: ExpressionValueUpdate[]
}
```


### Errors - Language Server
The language server component also has its own set of errors. This section is
not a complete specification and will be updated as new errors are added.

##### `FileSystemError`
This error signals generic file system errors.

```typescript
"error" : {
  "code" : 1000,
  "message" : String
}
```

##### `ContentRootNotFoundError`
The error informs that the requested content root cannot be found.

```typescript
"error" : {
  "code" : 1001,
  "message" : "Content root not found"
}
```

##### `AccessDeniedError`
It signals that a user doesn't have access to a resource.

```typescript
"error" : {
  "code" : 1002,
  "message" : "Access denied"
}
```

##### `FileNotFound`
It signals that requested file doesn't exist.

```typescript
"error" : {
  "code" : 1003,
  "message" : "File not found"
}
```

##### `FileExists`
It signals that file already exists.

```typescript
"error" : {
  "code" : 1004,
  "message" : "File already exists"
}
```

##### `OperationTimeoutError`
It signals that IO operation timed out.

```typescript
"error" : {
  "code" : 1005,
  "message" : "IO operation timeout"
}
```

##### `NotDirectory`
It signals that provided path is not a directory.

```typescript
"error" : {
  "code" : 1006,
  "message" : "Path is not a directory"
}
```

##### `StackItemNotFoundError`
```typescript
"error" : {
  "code" : 2001,
  "message" : "Stack item not found."
}
```
##### `FileNotOpenedError`
Signals that a file wasn't opened.

```typescript
"error" : {
  "code" : 3001,
  "message" : "File not opened"
}
```

##### `TextEditValidationError`
Signals that validation has failed for a series of edits.

```typescript
"error" : {
  "code" : 3002,
  "message" : "The start position is after the end position"
}
```

##### `InvalidVersionError`
Signals that version provided by a client doesn't match to the version
computed by the server.

```typescript
"error" : {
  "code" : 3003,
  "message" : "Invalid version [client version: ade2967cab172183d1a67ea40cb8e92e23218764bc9934c3795fcea5, server version: 7602967cab172183d1a67ea40cb8e92e23218764bc9934c3795fcea5]"
}
```

##### `WriteDeniedError`
Signals that the client doesn't hold write lock to the buffer.

```typescript
"error" : {
  "code" : 3004,
  "message" : "Write denied"
}
```
