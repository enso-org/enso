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

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Architecture](#architecture)
  - [Language Server](#language-server)
  - [File Manager](#file-manager)
  - [Multi-Client Coordinator](#multi-client-coordinator)
  - [Supervisor](#supervisor)
- [The Protocol Itself](#the-protocol-itself)
  - [Protocol Communication Patterns](#protocol-communication-patterns)
  - [The Protocol Transport](#the-protocol-transport)
  - [The Protocol Format](#the-protocol-format)
- [Protocol Functionality](#protocol-functionality)
  - [Project State Management](#project-state-management)
  - [File Management and Storage](#file-management-and-storage)
  - [Textual Diff Management](#textual-diff-management)
  - [Execution Management](#execution-management)
  - [Completion](#completion)
  - [Visualisation Support](#visualisation-support)
  - [Analysis Operations](#analysis-operations)
  - [Functionality Post 2.0](#functionality-post-20)
- [Protocol Message Specification](#protocol-message-specification)

<!-- /MarkdownTOC -->

## Architecture
While it may initially seem like these service components could all be rolled
into one, division of responsibilities combines with plain necessity to mean
that we require a set of services instead. This section deals with an
architecture proposal for how to make these services work together.

It can be summarised with three main ideas:

1. **Defined Services:** These, such as the language server and file manager are
   just doing their isolated jobs. They make the assumption that they only have
   a single client connected to them.
2. **Multi-Client Coordination:** This is an independent service that deals with
   handling connections from multiple clients (IDEs). This service will then
   combine the messages from those clients to create a single authoritative
   message stream that the services in point 1 can deal with.
3. **Supervisor:** The supervisor process is responsible for set-up and
   tear-down of the other processes, as well as for restarting any of the other
   processes correctly if they fail.

> The actionables for this section are:
>
> - Determine any feasible alternatives for this architecture.
> - Make a final decision on how to architect the set of back-end services.

### Language Server
The language server is solely responsible for handling the duties commonly
attributed to a language server, as well as the special functionality needed by
Enso Studio. Its functionality can be summarised as follows, though not all of
this is necessary for 2.0:

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

### File Manager
The file manager service is responsible for actually handling the physical files
on disk. While there are some arguments for including this in the language
server, it makes far more sense as a separate component.

This component is responsible for the following:

- **Code File Management:** Handling the loading and saving of code files on
  disk in response to commands from the GUI.
- **Data File Management:** Handling the upload and download of data files that
  the users want to work with. These files should be accessible by the language
  server, but it doesn't need to know about how they got there or how they get
  edited.
- **Version Control:** In the future, this component will also become
  responsible for interacting with the underlying version control system that
  stores the project data, and creating a coherent file history view for users.

> The actionables for this section are:
>
> - Determine if whose responsibility the file-management component should be.

### Multi-Client Coordinator
This coordinator process is responsible for accepting connections from multiple
users' IDEs in order to enable a multi-client editing experience. It has to take
the messages that come in across these multiple connections and reconcile them
to create a single 'stream of truth' for the file manager and language server,
as neither are multi-client aware.

This component is responsible for the following:

- Accepting connections from multiple clients.
- Distributing updates between clients.
- Reconciling the edits of multiple people at once to create a coherent edit
  stream for the language server.
- De-duplicating requests where relevant (e.g. value subscription pooling) by
  tracking which clients are to receive which responses.

It should be noted that _all_ protocol messages will go via this coordinator
service, and it will hence be the 'entry point' to the set of services.

### Supervisor
The supervisor process is an orchestrator, and is responsible for setting up and
tearing down the other services, as well as restarting them when they fail. Its
responsibilities can be summarised as follows:

- Starting up the set of services for a given project.
- Tearing down the set of services correctly when a project is closed.
- Restarting any of the services properly when they fail. Please note that this
  may require the ability to kill and re-start services that haven't crashed due
  to dependencies between services.

## The Protocol Itself
The protocol refers to the communication format that all of the above services
speak between each other and to the GUI. This protocol is not specialised only
to language server operations, as instead it needs to work for all of the
various services in this set.

> The actionables for this section are:
>
> - Do we want to remain compatible (where possible) with the Microsoft LSP
>   [specification](https://microsoft.github.io/language-server-protocol/specifications/specification-3-14/)?
> - What is the agreed-upon design for the protocol itself?

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

> The actionables for this section are as follows:
>
> - Determine if there are any other communication patterns that we need to be
>   able to support.
> - Do we want to support the request/ack pattern as a separate style (e.g.
>   "run code")?

### The Protocol Transport
The transport of the protocol refers to the underlying layer over which its
messages (discussed in [the protocol format](#the-protocol-format) below) are
sent. It is unclear at this point as to the exact transport we want to use, but
the communication patterns listed above seem to point in the direction of either
WebSocket on its own, or a mixture of WebSockets and HTTP.

> The actionables for this section are:
>
> - Do we want to stay compatible with LSP? If so, we're forced into using pure
>   WS for transport.
> - What does the GUI prefer in this regard? Why?
> - Do we need true serialized request-response? If so, then a full WS solution
>   would need a concept of message IDs.
> - Marcin: Does JSON RPC satisfy our needs?

### The Protocol Format
This section describes the format of a protocol message. This format should be
adhered to by all messages and should obey the following tenets:

- It should permit easy debugging, remaining human readable where possible.
- It should have good support across multiple languages.
- It needs to have good performance for all use-cases, _including_ potentially
  large visualisation data.

The exact format of the protocol is up in the air, but given the above
requirements it makes the most sense for it to be a hybrid protocol that
combines both textual and binary representations. This comes down to the
following set of tradeoffs:

- We want the majority of messages to be human readable to permit easy debugging
  and allow for better multi-language support.
- Textual formats have _low_ overhead for most data.
- _However_, such formats have significant overhead for large chunks of binary
  data, even if they can support them (such as via Base64 encoding).
- We need large chunks of binary data to be _fast_.

> The actionables for this section are as follows:
>
> - Determine if we should settle for a full-binary, or hybrid protocol format.
> - Determine how we can support a hybrid protocol. There is potential for
>   maintaining a separate WS connection for visualisation data only (e.g. audio,
>   video, images, etc).
> - Do we ever want to send raw binary data in circumstances _not_ tied to the
>   visualisations.

## Protocol Functionality
This entire section deals with the _functional_ requirements placed upon the
protocol used by the engine services. These requirements are overwhelmingly
imposed by the IDE, but also include additional functionality for the future
evolution of the language.

All of the following pieces of functionality that are explained in detail are
those _expected_ for the 2.0 release. Any additional functionality beyond this
milestone is described in a [dedicated section](#functionality-post-20).

> The actionables for this section are as follows:
>
> - Determine if the ere is any missing or extraneous functionality for the 2.0
>   release of Enso.
> - Once we have a set of requirements, these should be prioritised in order to
>   assist the IDE team in getting things working as quickly as possible.

### Project State Management
One of the most important functionalities for this service set is the ability to
manage the state of a project in general. The project state refers to the whole
set of the project files and metadata and needs to support the following
functionalities:

- Get project metadata (name, maintainer, version, dependencies, and so on)
- List modules
- Delete module
- Create module

> The actionables for this section are as follows:
>
> - Are there any other functionalities needed from this component?

### File Management and Storage
The file management component needs to deal with both the files storing the
textual program source and any data files uploaded by the user for their program
to use. It should be noted that this does _not_ refer to any sophisticated IO
management that may be done by the runtime in the future.

It needs to support the following functionality:

- CRUD operations on Enso source files.
- CRUD operations on user data files.

> The actionables for this section are:
>
> - Should this component support git and provide a historical view over files?
> - If so, should this happen for 2.0?
> - Are there any other operations needed from the IDE that would fall under
>   this component?
> - Should this component be the engine team's responsibility? It is only needed
    in the cloud setting and has more to do with Cloud than the Language.

### Textual Diff Management
The engine services need to support robust handling of textual diffs. This is
simply because it is the primary form of communication for synchronising source
code between the IDE and the engine. It will need to support the following
operations:

- Synchronisation requests to ensure that the engine and IDE have the same view
  of the files in the project.
- Diff update requests, that send a textual diff between client and server (or
  vice versa).
- Metadata modification requests. The IDE can set a metadata of any location,
  storing payloads of its choice (e.g. node positions).
- It will need to handle ensuring that the node metadata is correctly kept in
  sync.

In order to support these properly, it needs to account for the following things
in the design:

- Diffs should be kept as mimimal as possible. Even so, diffs will likely
  requires some rich minimisation on the server. This will involve AST-based
  diffing and various other compiler-internal functionality to ensure that we
  recompute the minimal possible subset.
- The library for recomputing ID locations needs to be written in scala (for use
  by the server), or those recomputed IDs must be sent on every diff (but this
  would muck with the potential for alternative front-ends).
- The component responsible for handling multiple connections must be able to
  send a file update notification to other clients.

> The actionables for this section are:
>
> - Determine if there are other types of operations needed to be supported on
>   diffs.
> - Determine if there are missing portions of the design.
> - What guarantees can we get about the diffs? What can we realistically
>   expect?
> - Which team is responsible for writing the library for recomputing the ID
>   locations?
> - What kind of multi-client editing do we want to support at first? FCFS, or
>   a 'write-lock' style solution?

### Execution Management
The language server process will need to be able to respond to requests for
various kinds of execution of Enso code. Furthermore, it needs to be able to
respond to requests to 'listen' to the execution of various portions of code.
This implies that the following functionalities are needed:

- Execute a method/function with arguments.
- Execute a method/function with arguments _from_ a selected application (i.e.
  enter a node from its call-site).
- Attach an execution listener to an arbitrary code span. These listeners should
  trigger an update (by ID) every time a node at that position is executed or
  its type changes. It is very important to be able to get type information as
  this is used to colour connections on the graph.
- Detach an execution listener.

> The actionables for this section are:
>
> - What else do we need to support for 2.0?
> - Since listeners are per-GUI (for performance), what requirements do we need
>   to place on GUI communication (e.g. does the multi-user coordinator need to
>   have heartbeat messages for keepalive)?
> - What, exactly, should the value listener updates contain? Should they have
>   the whole update, just a short rep and type with a pointer to where a
>   request can be made to get the full value (better for performance), or
    just the full value pointer (best performance, clunky usage)?

### Completion
The IDE needs the ability to request completions for some target point (cursor
position) in the source code. In essence, this boils down to _some_ kind of
smart completion. The completion should provide the following:

- Sensible suggestions at the cursor position, ranked by relevance.
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

It should be noted that the exact set of criteria for determining the
'relevance' of a suggestion have not yet been determined.

Another possible solution is for the engine to send _all_ globally defined
symbols to the IDE upfront and for the suggestions algorithm to be implement
client side. This has the potential upsides of:

  - Better performance & more fluid experience (no need to ping the server
    every time a user is about to type).
  - Lower cloud resources usage (the often-triggered bit of computing
    hints is offloaded to the client's browser).
  - In phase 1 of the project (before TC), it is the only way to take types
    into account – IDE is the only component that reliably stores the type 
    information.


> The actionables for this section are:
> - Determine whether the algorithmic details are a part of the language server
    design.
> - Determine what should form the candidate set in a given completion location,
>   and how these candidates should be ranked (type information, scope
>   information, documentation, tags, other scoring metadata).
> - Determine how best to transport these candidates back to the GUI in order to
>   provide the best performance and responsiveness possible.

### Visualisation Support
A major part of Enso Studio's functionality is the rich embedded visualisations
that it supports. This means that the following functionality is necessary:

- Execution of an arbitrary Enso expression on a cached value designated by
  a source location.
- This code should be executed in its own isolated scope (having certains
  modules imported, per the visualization's request), with only the required
  input values being available.

> The actionables for this section are:
>
> - What else do we need to support visualisations for 2.0?

### Analysis Operations
We also want to be able to support a useful set of semantic analysis operations
to help users navigate their code. As these rely on knowledge of the language
semantics, they must be explicitly supported by the language server:

- List functions/methods in scope
- Find usages of symbol (impossible without a typechecker)
- Jump to definition of symbol (only possible without a typechecker by runtime
  profiling)
- Search for symbol
- Import file for symbol

> The actionables for this section are:
>
> - Are there any other analyses we need to support for 2.0?

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
  using Enso code, and hence there needs to be the ability to execute arbitrary
  code on values visible in the current scope.
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
  out, continue, etc).
- **Profiling Information:** Profiling information for the executing code, able
  to be displayed visually in Enso Studio.
- **Code Formatting:** Automatic formatting of Enso code using the One True
  Style ™.

> The actionables for this section are:
>
> - Are there any other things we know now that we want to support in the
>   future?

## Protocol Message Specification
This section exists to contain a specification of each of the messages the
protocol supports. This is in order to aid the proper creation of clients, and
to serve as an agreed-upon definition for the protocol between the IDE team and
the Engine team.

> The actionables for this section are:
>
> - As we establish the _exact_ format for each of the messages supported by the
>   services, record the details of each message here.
