---
layout: developer-doc
title: Enso Protocol Language Server Message Specification
category: language-server
tags: [language-server, protocol, specification]
order: 4
---

# Enso Protocol Language Server Message Specification

This document contains the specification of the Enso protocol messages that
pertain to the language server component. Please familiarise yourself with the
[common](./protocol-common.md) features of the protocol before reading this
document.

For information on the design and architecture of the protocol, as well as its
transport formats, please look [here](./protocol-architecture).

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Types](#types)
  - [`ExpressionId`](#expressionid)
  - [`ContextId`](#contextid)
  - [`StackItem`](#stackitem)
  - [`MethodPointer`](#methodpointer)
  - [`ExpressionValueUpdate`](#expressionvalueupdate)
  - [`VisualisationConfiguration`](#visualisationconfiguration)
  - [`SuggestionEntryArgument`](#suggestionentryargument)
  - [`SuggestionEntry`](#suggestionentry)
  - [`SuggestionEntryType`](#suggestionentrytype)
  - [`SuggestionId`](#suggestionid)
  - [`SuggestionsDatabaseEntry`](#suggestionsdatabaseentry)
  - [`SuggestionsDatabaseUpdate`](#suggestionsdatabaseupdate)
  - [`File`](#file)
  - [`DirectoryTree`](#directorytree)
  - [`FileAttributes`](#fileattributes)
  - [`UTCDateTime`](#utcdatetime)
  - [`FileEventKind`](#fileeventkind)
  - [`Position`](#position)
  - [`Range`](#range)
  - [`TextEdit`](#textedit)
  - [`DiagnosticType`](#diagnostictype)
  - [`StackTraceElement`](#stacktraceelement)
  - [`Diagnostic`](#diagnostic)
  - [`SHA3-224`](#sha3-224)
  - [`FileEdit`](#fileedit)
  - [`FileContents`](#filecontents)
  - [`FileSystemObject`](#filesystemobject)
  - [`WorkspaceEdit`](#workspaceedit)
- [Connection Management](#connection-management)
  - [`session/initProtocolConnection`](#sessioninitprotocolconnection)
  - [`session/initBinaryConnection`](#sessioninitbinaryconnection)
- [Capability Management](#capability-management)
  - [`capability/acquire`](#capabilityacquire)
  - [`capability/release`](#capabilityrelease)
  - [`capability/granted`](#capabilitygranted)
  - [`capability/forceReleased`](#capabilityforcereleased)
- [Capabilities](#capabilities)
  - [`text/canEdit`](#textcanedit)
  - [`file/receivesTreeUpdates`](#filereceivestreeupdates)
  - [`executionContext/canModify`](#executioncontextcanmodify)
  - [`executionContext/receivesUpdates`](#executioncontextreceivesupdates)
  - [`search/receivesSuggestionsDatabaseUpdates`](#searchreceivessuggestionsdatabaseupdates)
- [File Management Operations](#file-management-operations)
  - [`file/write`](#filewrite)
  - [`file/read`](#fileread)
  - [`file/writeBinary`](#filewritebinary)
  - [`file/readBinary`](#filereadbinary)
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
  - [`workspace/undo`](#workspaceundo)
  - [`workspace/redo`](#workspaceredo)
- [Monitoring](#monitoring)
  - [`heartbeat/ping`](#heartbeatping)
- [Execution Management Operations](#execution-management-operations)
  - [Execution Management Example](#execution-management-example)
  - [Create Execution Context](#create-execution-context)
  - [Push Item](#push-item)
  - [Pop Item](#pop-item)
  - [`executionContext/create`](#executioncontextcreate)
  - [`executionContext/destroy`](#executioncontextdestroy)
  - [`executionContext/fork`](#executioncontextfork)
  - [`executionContext/push`](#executioncontextpush)
  - [`executionContext/pop`](#executioncontextpop)
  - [`executionContext/recompute`](#executioncontextrecompute)
  - [`executionContext/expressionValuesComputed`](#executioncontextexpressionvaluescomputed)
  - [`executionContext/executionFailed`](#executioncontextexecutionfailed)
  - [`executionContext/executionStatus`](#executioncontextexecutionstatus)
  - [`executionContext/attachVisualisation`](#executioncontextattachvisualisation)
  - [`executionContext/detachVisualisation`](#executioncontextdetachvisualisation)
  - [`executionContext/modifyVisualisation`](#executioncontextmodifyvisualisation)
  - [`executionContext/visualisationUpdate`](#executioncontextvisualisationupdate)
- [Search Operations](#search-operations)
  - [Suggestions Database Example](#suggestionsdatabaseexample)
  - [`search/getSuggestionsDatabase`](#searchgetsuggestionsdatabase)
  - [`search/invalidateSuggestionsDatabase`](#invalidatesuggestionsdatabase)
  - [`search/getSuggestionsDatabaseVersion`](#searchgetsuggestionsdatabaseversion)
  - [`search/suggestionsDatabaseUpdate`](#searchsuggestionsdatabaseupdate)
  - [`search/completion`](#searchcompletion)
- [Input/Output Operations](#input-output-operations)
  - [`io/redirectStandardOutput`](#ioredirectstdardoutput)
  - [`io/suppressStandardOutput`](#iosuppressstdardoutput)
  - [`io/standardOutputAppended`](#iostandardoutputappended)
  - [`io/redirectStandardError`](#ioredirectstdarderror)
  - [`io/suppressStandardError`](#iosuppressstdarderror)
  - [`io/standardErrorAppended`](#iostandarderrorappended)
  - [`io/feedStandardInput`](#iofeedstandardinput)
  - [`io/waitingForStandardInput`](#iowaitingforstandardinput)
- [Errors](#errors)
  - [`AccessDeniedError`](#accessdeniederror)
  - [`FileSystemError`](#filesystemerror)
  - [`ContentRootNotFoundError`](#contentrootnotfounderror)
  - [`FileNotFound`](#filenotfound)
  - [`FileExists`](#fileexists-1)
  - [`OperationTimeoutError`](#operationtimeouterror)
  - [`NotDirectory`](#notdirectory)
  - [`StackItemNotFoundError`](#stackitemnotfounderror)
  - [`ContextNotFoundError`](#contextnotfounderror)
  - [`EmptyStackError`](#emptystackerror)
  - [`InvalidStackItemError`](#invalidstackitemerror)
  - [`ModuleNotFoundError`](#modulenotfounderror)
  - [`VisualisationNotFoundError`](#visualisationnotfounderror)
  - [`VisualisationExpressionError`](#visualisationexpressionerror)
  - [`VisualisationEvaluationError`](#visualisationevaluationerror)
  - [`ExecutionFailedError`](#executionfailederror)
  - [`FileNotOpenedError`](#filenotopenederror)
  - [`TextEditValidationError`](#texteditvalidationerror)
  - [`InvalidVersionError`](#invalidversionerror)
  - [`WriteDeniedError`](#writedeniederror)
  - [`CapabilityNotAcquired`](#capabilitynotacquired)
  - [`SessionNotInitialisedError`](#sessionnotinitialisederror)
  - [`SessionAlreadyInitialisedError`](#sessionalreadyinitialisederror)
  - [`SuggestionsDatabaseError`](#suggestionsdatabaseerror)

<!-- /MarkdownTOC -->

## Types

There are a number of types that are used only within the language server's
protocol messages. These are specified here.

### `ExpressionId`

An identifier used for Enso expressions.

```typescript
type ExpressionId = UUID;
```

### `ContextId`

An identifier used for execution contexts.

```typescript
type ContextId = UUID;
```

### `StackItem`

A representation of an executable position in code, used by the execution APIs.

`ExplicitCall` is a call performed at the top of the stack, to initialize the
context with first execution. The `thisArgumentsPosition` field can be omitted,
in which case the context will try to infer the argument on a best-effort basis.
E.g. for a module-level method, or a method defined on a parameter-less atom
type, `this` will be substituted for the unambiguous singleton instance.

`LocalCall` is a call corresponding to "entering a function call".

```typescript
type StackItem = ExplicitCall | LocalCall;

interface ExplicitCall {
  methodPointer: MethodPointer;
  thisArgumentExpression?: String;
  positionalArgumentsExpressions: String[];
}

interface LocalCall {
  expressionId: ExpressionId;
}
```

### `MethodPointer`

Points to a method definition.

```typescript
interface MethodPointer {
  /** The fully qualified module name. */
  module: String;

  /** The type on which the method is defined. */
  definedOnType: String;

  /** The method name. */
  name: String;
}
```

### `ExpressionValueUpdate`

```typescript
interface ExpressionValueUpdate {
  /** The id of updated expression */
  expressionId: ExpressionId;

  /** The updated type of the expression */
  type?: String;

  /** The updated pointer to the method call */
  methodPointer?: SuggestionId;
}
```

### `VisualisationConfiguration`

A configuration object for properties of the visualisation.

```typescript
interface VisualisationConfiguration {
  /**
   * An execution context of the visualisation.
   */
  executionContextId: UUID;
  /**
   * A qualified name of the module containing the expression which creates
   * visualisation.
   */
  visualisationModule: String;
  /**
   * The expression that creates a visualisation.
   */
  expression: String;
}
```

### `SuggestionEntryArgument`

The argument of a [`SuggestionEntry`](#suggestionentry).

#### Format

```typescript
// The argument of an atom, method or function suggestion
interface SuggestionEntryArgument {
  // The argument name
  name: string;
  // The arguement type. String 'Any' is used to specify genric types
  type: string;
  // Indicates whether the argument is lazy
  isSuspended: bool;
  // Indicates whether the argument has default value
  hasDefault: bool;
  // Optional default value
  defaultValue?: string;
}
```

### `SuggestionEntry`

The language construct that can be returned as a suggestion.

#### Format

```typescript
// The definition scope
interface SuggestionEntryScope {
  // The start position of the definition scope
  start: Position;
  // The end position of the definition scope
  end: Position;
}

// A type of suggestion entries.
type SuggestionEntry =
  // A value constructor
  | SuggestionEntryAtom
  // A method defined on a type
  | SuggestionEntryMethod
  // A function
  | SuggestionEntryFunction
  // A local value
  | SuggestionEntryLocal;

interface SuggestionEntryAtom {
  name: string;
  module: string;
  arguments: [SuggestionEntryArgument];
  returnType: string;
  documentation?: string;
}

interface SuggestionEntryMethod {
  name: string;
  module: string;
  arguments: [SuggestionEntryArgument];
  selfType: string;
  returnType: string;
  documentation?: string;
}

interface SuggestionEntryFunction {
  name: string;
  module: string;
  arguments: [SuggestionEntryArgument];
  returnType: string;
  scope: SuggestionEntryScope;
}

interface SuggestionEntryLocal {
  name: string;
  module: string;
  returnType: string;
  scope: SuggestionEntryScope;
}
```

### `SuggestionEntryType`

The suggestion entry type that is used as a filter in search requests.

#### Format

```typescript
// The kind of a suggestion.
type SuggestionEntryType = Atom | Method | Function | Local;
```

### `SuggestionId`

The suggestion entry id of the suggestions database.

#### Format

```typescript
type SuggestionId = number;
```

### `SuggestionsDatabaseEntry`

#### Format

The entry in the suggestions database.

```typescript
interface SuggestionsDatabaseEntry {
  // suggestion entry id
  id: SuggestionId;
  // suggestion entry
  suggestion: SuggestionEntry;
}
```

### `SuggestionsDatabaseUpdate`

The update of the suggestions database.

#### Format

```typescript
// The kind of the suggestions database update.
type SuggestionsDatabaseUpdate = Add | Remove | Modify;

interface Add {
  // suggestion entry id
  id: SuggestionId;
  // suggestion entry
  suggestion: SuggestionEntry;
}

interface Remove {
  // suggestion entry id
  id: SuggestionId;
}

interface Modify {
  // suggestion entry id
  id: SuggestionId;
  // new return type
  returnType: String;
}
```

### `File`

A representation of a file on disk.

#### Format

```typescript
interface File {
  name: String; // Includes the file extension
  type: String;
}
```

### `DirectoryTree`

A directory tree is a recursive type used to represent tree structures of files
and directories. It contains files and symlinks in the `files` section and
directories in the `directories` section. When the tree was requested with the
parameter limiting the maximum depth, the bottom of the `DirectoryTree` will
contain `Directory` node in the `files` section indicating that there is a
directory, but the contents are unknown because we've reached the maximum depth.

#### Format

```typescript
interface DirectoryTree {
  path: Path;
  name: String;
  files: [FileSystemObject];
  directories: [DirectoryTree];
}
```

### `FileAttributes`

A description of the attributes of a file required by the IDE. These attributes
may be expanded in future.

#### Format

```typescript
/**
 * A representation of the attributes of a file.
 *
 * @param creationTime creation time
 * @param lastAccessTime last access time
 * @param lastModifiedTime last modified time
 * @param kind type of [[FileSystemObject]], can be:
 * `Directory`, `File`, `Other`
 * @param byteSize size in bytes
 */
interface FileAttributes {
  creationTime: UTCDateTime;
  lastAccessTime: UTCDateTime;
  lastModifiedTime: UTCDateTime;
  kind: FileSystemObject;
  byteSize: number;
}
```

### `UTCDateTime`

Time in UTC time zone represented as ISO-8601 string

#### Format

```typescript
type UTCDateTime = String;
```

### `FileEventKind`

The kind of event being described for a watched file.

#### Format

```typescript
type FileEventKind = Added | Removed | Modified;
```

### `Position`

A representation of a position in a text file.

#### Format

```typescript
interface Position {
  /**
   * Line position in a document (zero-based).
   */
  line: number;

  /**
   * Character offset on a line in a document (zero-based). Assuming that the
   * line is represented as a string, the `character` value represents the gap
   * between the `character` and `character + 1`.
   *
   * If the character value is greater than the line length it defaults back to
   * the line length.
   */
  character: number;
}
```

```idl
namespace org.enso.languageserver.protocol.binary;

struct Position {
  // Line position in a document (zero-based)
  line: uint64;
  // Character offset on a line in a document (zero-based)
  character: uint64;
}
```

### `Range`

A representation of a range of text in a text file.

#### Format

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

### `TextEdit`

A representation of a change to a text file at a given position.

#### Format

```typescript
interface TextEdit {
  range: Range;
  text: String;
}
```

### `DiagnosticType`

The type of diagnostic message.

#### Format

```typescript
type DiagnosticType = Error | Warning;
```

### `StackTraceElement`

The frame of the stack trace. If the error refer to a builtin node, the `path`
and `location` fields will be empty.

#### Format

```typescript
interface StackTraceElement {
  /**
   * The function name containing the stack trace element.
   */
  functionName: String;

  /**
   * The location of the file.
   */
  path?: Path;

  /**
   * The location of the element in a file.
   */
  location?: Range;
}
```

### `Diagnostic`

A diagnostic object is produced as a result of an execution attempt, like
pushing the method pointer to a call stack, or editing the file. It can
represent a compiler warning, a compilation error, or a runtime error. The
message has optional `path`, `location` and `stack` fields containing
information about the location in the source code.

In case of the runtime errors, the `path` and `location` fields may be empty if
the error happens in a builtin node. Then, to locate the error in the code, you
can use the `stack` field with a stack trace to find the first element with
non-empty location (as the head of the stack will point to the builtin element).

#### Format

```typescript
interface Diagnostic {
  /**
   * The type of diagnostic message.
   */
  kind: DiagnosticType;

  /**
   * The diagnostic message.
   */
  message: String;

  /**
   * The location of a file containing the diagnostic.
   */
  path?: Path;

  /**
   * The location of the diagnostic object in a file.
   */
  location?: Range;

  /**
   * The stack trace.
   */
  stack: StackTraceElement[];
}
```

### `SHA3-224`

The `SHA3-224` message digest encoded as a base16 string.

#### Format

```typescript
type SHA3-224 = String;
```

### `FileEdit`

A representation of a batch of edits to a file, versioned.

`SHA3-224` represents hash of the file contents. `oldVersion` is the version
you're applying your update on, `newVersion` is what you compute as the hash
after applying the changes. In other words,

```python
hash(origFile) == oldVersion
hash(applyEdits(origFile, edits)) == newVersion
```

it's a sanity check to make sure that the diffs are applied consistently.

Consecutive text edits are applied sequentially, every one acting on the result
of applying previous ones on the original buffer contents. In pseudocode:

```haskell
applyEdits buffer [] = buffer
applyEdits buffer (first : rest) = applyEdits (applyTextEdit buffer first) rest
```

#### Format

```typescript
interface FileEdit {
  path: Path;
  edits: [TextEdit];
  oldVersion: SHA3-224;
  newVersion: SHA3-224;
}
```

### `FileContents`

A representation of the contents of a file.

#### Format

```typescript
interface FileContents[T] {
  contents: T;
}

class TextFileContents extends FileContents[String];
```

### `FileSystemObject`

A representation of what kind of type a filesystem object can be.

#### Format

```typescript
type FileSystemObject = Directory | SymlinkLoop | File | Other;

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
}
```

### `WorkspaceEdit`

This is a message to be specified once we better understand the intricacies of
undo/redo.

> The actionables for this section are:
>
> - Work out the design of this message.
> - Specify this message.

## Connection Management

In order to properly set-up and tear-down the language server connection, we
need a set of messages to control this process.

### `session/initProtocolConnection`

This message initialises the connection used to send the textual protocol
messages. This initialisation is important such that the client identifier can
be correlated between the textual and data connections.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
{
  clientId: UUID;
}
```

#### Result

```typescript
{
  contentRoots: [UUID];
}
```

#### Errors

- [`SessionAlreadyInitialisedError`](#sessionalreadyinitialisederror) to signal
  that session is already initialised.

### `session/initBinaryConnection`

This message initialises the data connection used for transferring binary data
between engine and clients. This initialisation is important such that the
client identifier can be correlated between the data and textual connections.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Data
- **Visibility:** Public

#### Parameters

```idl
namespace org.enso.languageserver.protocol.binary;

//A command initializing a data session.
table InitSessionCommand {

  //A unique identifier of a client initializing the session.
  identifier: EnsoUUID (required);

}

root_type InitSessionCommand;
```

#### Result

```
namespace org.enso.languageserver.protocol.binary;

//Indicates an operation has succeeded.
table Success {}
```

#### Errors

N/A

## Capability Management

In order to mediate between multiple clients properly, the language server has a
robust notion of capability management to grant and remove permissions from
clients.

### `capability/acquire`

This requests that the server grant the specified capability to the requesting
client.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
{
  method: String;
  registerOptions?: any;
}
```

The `registerOptions` are determined by the `method`. The method must be listed
in the section on [capabilities](#capabilities) below.

#### Result

```typescript
null;
```

#### Errors

TBC

### `capability/release`

This requests that the server acknowledge that the client is releasing a given
capability.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
{
  registration: CapabilityRegistration;
}
```

#### Result

```typescript
null;
```

#### Errors

TBC

### `capability/granted`

This notifies the client that it has been granted a capability without any
action on its part.

- **Type:** Notification
- **Direction:** Server -> Client
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
{
  registration: CapabilityRegistration;
}
```

#### Errors

TBC

### `capability/forceReleased`

This notifies the client that a capability has been forcibly removed from its
capability set.

- **Type:** Notification
- **Direction:** Server -> Client
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
{
  registration: CapabilityRegistration;
}
```

#### Errors

TBC

## Capabilities

The capability management features work with the following capabilities.

### `text/canEdit`

This capability states that the capability has the ability to perform both
`text/applyEdit` and `text/save` for the specified file.

- **method:** `text/canEdit`
- **registerOptions:** `{path: Path;}`

#### Enables

- [`text/applyEdit`](#textapplyedit)
- [`text/save`](#textsave)

#### Disables

None

### `file/receivesTreeUpdates`

This capability states that the client will receive updates for any watched
content roots in the current project.

- **method:** `file/receivesTreeUpdates`
- **registerOptions:** `{ path: Path; }`

#### Enables

- [`file/event`](#fileevent)

#### Disables

None

#### Errors

[`capability/acquire`](#capabilityacquire):

- [`FileSystemError`](#filesystemerror) to signal a generic, unrecoverable
  file-system error.
- [`FileNotFound`](#filenotfound) informs that path cannot be found.

[`capability/release`](#capabilityrelease):

- [`CapabilityNotAcquired`](#capabilitynotacquired) informs that requested
  capability is not acquired.

### `executionContext/canModify`

This capability states that the client has the ability to modify an execution
context, including modifying the execution stack, invalidating caches, or
destroying the context.

- **method:** `executionContext/canModify`
- **registerOptions:** `{ contextId: ContextId; }`

#### Enables

- [`executionContext/destroy`](#executioncontextdestroy)
- [`executionContext/recompute`](#executioncontextrecompute)
- [`executionContext/push`](#executioncontextpush)
- [`executionContext/pop`](#executioncontextpop)
- [`executionContext/attachVisualisation`](#executioncontextattachvisualisation)
- [`executionContext/modifyVisualisation`](#executioncontextmodifyvisualisation)
- [`executionContext/detachVisualisation`](#executioncontextdetachvisualisation)
- [`executionContext/visualisationUpdate`](#executioncontextvisualisationupdate)

#### Disables

None

### `executionContext/receivesUpdates`

This capability states that the client receives expression value updates from a
given execution context.

- **method:** `executionContext/receivesUpdates`
- **registerOptions:** `{ contextId: ContextId; }`

#### Enables

- [`executionContext/expressionValuesComputed`](#executioncontextexpressionvaluescomputed)
- [`executionContext/executionFailed`](#executioncontextexecutionfailed)
- [`executionContext/executionStatus`](#executioncontextexecutionstatus)

#### Disables

None

### `search/receivesSuggestionsDatabaseUpdates`

This capability states that the client receives the search database updates for
a given execution context.

- **method:** `search/receivesSuggestionsDatabaseUpdates`
- **registerOptions:** `{}`

### Enables

- [`search/suggestionsDatabaseUpdate`](#suggestionsdatabaseupdate)

### Disables

None

## File Management Operations

The language server also provides file operations to the IDE.

### `file/write`

This requests that the file manager component write to a specified file with the
specified contents.

- **Type:** Request
- **Direction:** Client -> Server

This request is _explicitly_ allowed to write to files that do not exist, and
will create them under such circumstances. If a file is recorded as 'open' by
one of the clients, and another client attempts to write to that file, the write
must fail.

#### Parameters

```typescript
{
  path: Path;
  contents: FileContents[T];
}
```

#### Result

```typescript
null;
```

#### Errors

- [`FileSystemError`](#filesystemerror) to signal a generic, unrecoverable
  file-system error.
- [`ContentRootNotFoundError`](#contentrootnotfounderror) to signal that the
  requested content root cannot be found.
- [`AccessDeniedError`](#accessdeniederror) to signal that a user doesn't have
  access to a resource.

### `file/read`

This requests that the file manager component reads the contents of a specified
file.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

If the file is recorded as open by the language server, then the result will
return the contents from the in-memory buffer rather than the file on disk.

#### Parameters

```typescript
{
  path: Path;
}
```

#### Result

```typescript
{
  contents: FileContents[T];
}
```

#### Errors

- [`FileSystemError`](#filesystemerror) to signal a generic, unrecoverable
  file-system error.
- [`ContentRootNotFoundError`](#contentrootnotfounderror) to signal that the
  requested content root cannot be found.
- [`AccessDeniedError`](#accessdeniederror) to signal that a user doesn't have
  access to a resource.
- [`FileNotFound`](#filenotfound) informs that file cannot be found.

### `file/writeBinary`

This requests that the file manager component write to a specified file with the
binary contents.

- **Type:** Request
- **Connection:** Binary
- **Direction:** Client -> Server

This request is _explicitly_ allowed to write to files that do not exist, and
will create them under such circumstances. If a file is recorded as 'open' by
one of the clients, and another client attempts to write to that file, the write
must fail.

#### Parameters

```idl
namespace org.enso.languageserver.protocol.binary;

//A command writing binary contents to a file.
table WriteFileCommand {

  //A path to a file.
  path: Path;

  //Binary contents.
  contents: [ubyte];

}

```

#### Result

```idl
namespace org.enso.languageserver.protocol.binary;

//Indicates an operation has succeeded.
table Success {}
```

#### Errors

- [`FileSystemError`](#filesystemerror) to signal a generic, unrecoverable
  file-system error.
- [`ContentRootNotFoundError`](#contentrootnotfounderror) to signal that the
  requested content root cannot be found.
- [`AccessDeniedError`](#accessdeniederror) to signal that a user doesn't have
  access to a resource.

### `file/readBinary`

This requests that the file manager component reads the binary contents of a
specified file.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Binary
- **Visibility:** Public

If the file is recorded as open by the language server, then the result will
return the contents from the in-memory buffer rather than the file on disk.

#### Parameters

```idl
namespace org.enso.languageserver.protocol.binary;

//A command reading binary contents from a file.
table ReadFileCommand {

  //A path to a file.
  path: Path;

}
```

#### Result

```idl
namespace org.enso.languageserver.protocol.binary;

//A reply for a ReadFileCommand.
table FileContentsReply {

  //Binary contents.
  contents: [ubyte];

}
```

#### Errors

- [`FileSystemError`](#filesystemerror) to signal a generic, unrecoverable
  file-system error.
- [`ContentRootNotFoundError`](#contentrootnotfounderror) to signal that the
  requested content root cannot be found.
- [`AccessDeniedError`](#accessdeniederror) to signal that a user doesn't have
  access to a resource.
- [`FileNotFound`](#filenotfound) informs that file cannot be found.

### `file/create`

This request asks the file manager to create the specified file system object.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

This will fail if the specified object already exists.

#### Parameters

```typescript
{
  object: FileSystemObject;
}
```

#### Response

```typescript
null;
```

#### Errors

- [`FileSystemError`](#filesystemerror) to signal a generic, unrecoverable
  file-system error.
- [`ContentRootNotFoundError`](#contentrootnotfounderror) to signal that the
  requested content root cannot be found.
- [`AccessDeniedError`](#accessdeniederror) to signal that a user doesn't have
  access to a resource.

### `file/delete`

This request asks the file manager to delete the specified file system object.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
{
  path: Path;
}
```

#### Result

```
null
```

#### Errors

- [`FileSystemError`](#filesystemerror) to signal a generic, unrecoverable
  file-system error.
- [`ContentRootNotFoundError`](#contentrootnotfounderror) to signal that the
  requested content root cannot be found.
- [`FileNotFound`](#filenotfound) informs that file cannot be found.
- [`FileExists`](#fileexists) informs that file already exists

### `file/copy`

This request asks the file manager to copy a specified filesystem object to
another location.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
{
  from: Path;
  to: Path;
}
```

#### Result

```typescript
null;
```

#### Errors

- [`FileSystemError`](#filesystemerror) to signal a generic, unrecoverable
  file-system error.
- [`ContentRootNotFoundError`](#contentrootnotfounderror) to signal that the
  requested content root cannot be found.
- [`FileNotFound`](#filenotfound) informs that file cannot be found.

### `file/move`

This request asks the file manager to move a specified filesystem object to
another location.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

The move should be specified by filesystem events, and such notifications should
inform the client that the currently edited file has been moved.

#### Parameters

```typescript
{
  from: Path;
  to: Path;
}
```

#### Result

```typescript
null;
```

#### Errors

- [`FileSystemError`](#filesystemerror) to signal a generic, unrecoverable
  file-system error.
- [`ContentRootNotFoundError`](#contentrootnotfounderror) to signal that the
  requested content root cannot be found.
- [`FileNotFound`](#filenotfound) informs that file cannot be found.
- [`FileExists`](#fileexists) informs that target file already exists.

### `file/exists`

This request asks the file manager to check whether a filesystem object exists
at the specified path.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
{
  path: Path;
}
```

#### Result

```typescript
{
  exists: Boolean;
}
```

#### Errors

- [`ContentRootNotFoundError`](#contentrootnotfounderror) to signal that the
  requested content root cannot be found.

### `file/tree`

This request asks the file manager component to generate and provide the
directory tree starting at a given path.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
{
  path: Path;
  depth?: Number;
}
```

#### Result

```typescript
{
  tree: DirectoryTree;
}
```

#### Errors

- [`ContentRootNotFoundError`](#contentrootnotfounderror) to signal that the
  requested content root cannot be found.
- [`FileNotFound`](#filenotfound) informs that requested path does not exist or
  provided depth argument is <= 0.
- [`NotDirectory`](#notdirectory) informs that requested path is not a
  directory.

### `file/list`

This request lists the contents of a given filesystem object. For a file it will
just return the file, while for a directory it will list the contents of the
directory.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
{
  path: Path;
}
```

#### Result

```typescript
{
  paths: [FileSystemObject];
}
```

#### Errors

- [`ContentRootNotFoundError`](#contentrootnotfounderror) to signal that the
  requested content root cannot be found.
- [`FileNotFound`](#filenotfound) informs that requested path does not exist.
- [`NotDirectory`](#notdirectory) informs that requested path is not a
  directory.

### `file/info`

This request gets information about a specified filesystem object.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

This request should work for all kinds of filesystem object.

#### Parameters

```typescript
{
  path: Path;
}
```

#### Result

```typescript
{
  attributes: FileAttributes;
}
```

#### Errors

- [`ContentRootNotFoundError`](#contentrootnotfounderror) to signal that the
  requested content root cannot be found.
- [`FileNotFound`](#filenotfound) informs that requested path does not exist.

### `file/event`

This is a notification that is sent every time something under a watched content
root changes. It is used to ensure that the client's filesystem representation
stays in synchronisation with reality.

- **Type:** Notification
- **Direction:** Server -> Client
- **Connection:** Protocol
- **Visibility:** Public

Events should be sent from server to client for every event observed under one
of the (possibly multiple) content roots.

#### Parameters

```typescript
{
  path: Path;
  kind: FileEventKind;
}
```

#### Errors

None

### `file/addRoot`

This request adds a content root to the active project.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

When a content root is added, the language server must notify clients other than
the one that added the root by sending a `file/rootAdded`. Additionally, all
clients must be notified with a `file/event` about the addition of the new root.
The IDE is responsible for calling `file/tree` on that root to discover its
structure.

#### Parameters

```typescript
{
  absolutePath: [String];
  id: UUID; // The ID of the content root
}
```

#### Result

```typescript
null;
```

#### Errors

TBC

### `file/removeRoot`

This request removes a content root from the active project.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

When a content root is removed, the language server must notify clients other
than the one that added the root by sending a `file/rootRemoved`. Additionally,
the server must send a `file/event` making the root of the new tree visible. The
IDE is responsible for any additional discovery.

#### Parameters

```typescript
{
  id: UUID; // The content root ID
}
```

#### Result

```typescript
null;
```

#### Errors

TBC

### `file/rootAdded`

This is a notification sent to all clients other than the one performing the
addition of the root in order to inform them of the content root's ID.

- **Type:** Notification
- **Direction:** Server -> Client
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
{
  id: UUID; // The content root ID
  absolutePath: [String];
}
```

#### Errors

TBC

### `file/rootRemoved`

This is a notification sent to all clients other than the one performing the
removal of the content root in order to inform them of the removal of the root.

- **Type:** Notification
- **Direction:** Server -> Client
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
{
  id: UUID; // The content root ID
}
```

#### Errors

TBC

## Text Editing Operations

The language server also has a set of text editing operations to ensure that it
stays in sync with the clients.

### `text/openFile`

This requests the language server to open the specified file.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

If no client has write lock on the opened file, the capability is granted to the
client that sent the `text/openFile` message.

#### Parameters

```typescript
{
  path: Path;
}
```

#### Result

```typescript
{
  writeCapability?: CapabilityRegistration;
  content: String;
  currentVersion: SHA3-224;
}
```

#### Errors

- [`FileSystemError`](#filesystemerror) to signal a generic, unrecoverable
  file-system error.
- [`ContentRootNotFoundError`](#contentrootnotfounderror) to signal that the
  requested content root cannot be found.
- [`AccessDeniedError`](#accessdeniederror) to signal that a user doesn't have
  access to a resource.
- [`FileNotFound`](#filenotfound) informs that file cannot be found.

### `text/closeFile`

This requests the language server to close the specified file.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
{
  path: Path;
}
```

#### Result

```typescript
null;
```

#### Errors

- [`FileNotOpenedError`](#filenotopenederror) to signal that a file wasn't
  opened.

### `text/save`

This requests for the language server to save the specified file.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

The request may fail if the requesting client does not have permission to edit
that file, or if the client is requesting a save of an outdated version.

#### Parameters

```typescript
{
  path: Path;
  currentVersion: SHA3 - 224;
}
```

#### Result

```typescript
null;
```

#### Errors

- [`FileNotOpenedError`](#filenotopenederror) to signal that the file isn't
  open.
- [`InvalidVersionError`](#invalidversionerror) to signal that the version
  provided by the client doesn't match the version computed by the server.
- [`WriteDeniedError`](#writedeniederror) to signal that the client doesn't hold
  write lock for the buffer.
- [`FileSystemError`](#filesystemerror) to signal a generic, unrecoverable
  file-system error.
- [`ContentRootNotFoundError`](#contentrootnotfounderror) to signal that the
  requested content root cannot be found.
- [`AccessDeniedError`](#accessdeniederror) to signal that the user doesn't have
  access to a resource.

### `text/applyEdit`

This requests that the server apply a series of edits to the project. These
edits solely concern text files.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

This operation may fail if the requesting client does not have permission to
edit the resources for which edits are sent. This failure _may_ be partial, in
that some edits are applied and others are not.

#### Parameters

```typescript
{
  edit: FileEdit;
}
```

#### Result

```typescript
null;
```

#### Errors

- [`FileNotOpenedError`](#filenotopenederror) to signal that the file isn't
  open.
- [`TextEditValidationError`](#texteditvalidationerror) to signal that
  validation has failed for a series of edits.
- [`InvalidVersionError`](#invalidversionerror) to signal that the version
  provided by the client doesn't match the version computed by the server.
- [`WriteDeniedError`](#writedeniederror) to signal that the client doesn't hold
  write lock for the buffer.

### `text/didChange`

This is a notification sent from the server to the clients to inform them of any
changes made to files that they have open.

- **Type:** Notification
- **Direction:** Server -> Client
- **Connection:** Protocol
- **Visibility:** Public

This notification must _only_ be sent for files that the client has open.

#### Parameters

```typescript
{
  edits: [FileEdit];
}
```

#### Errors

```typescript
null;
```

## Workspace Operations

The language server also has a set of operations useful for managing the client
workspace.

### `workspace/undo`

This request is sent from the client to the server to request that an operation
be undone.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

The exact behaviour of this message is to be determined, but it must involve the
server undoing that same action for all clients in the workspace.

#### Parameters

```typescript
{
  requestID?: UUID; // If not specified, it undoes the latest request
}
```

#### Result

```typescript
null;
```

#### Errors

TBC

### `workspace/redo`

This request is sent from the client to the server to request that an operation
be redone.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

The exact behaviour of this message is to be determined, but it must involve the
server redoing that same action for all clients in the workspace.

#### Parameters

```typescript
{
  requestID?: UUID; // If not specified, it redoes the latest request
}
```

#### Result

```typescript
null;
```

#### Errors

TBC

## Monitoring

The language server also has a heartbeat operation to monitor the Language
server. This API is private and should be used only by the Project Manager.

### `heartbeat/ping`

This request is sent from the supervisor process to the server to check the
health of the Language Server.

- **Type:** Request
- **Direction:** Supervisor -> Server
- **Connection:** Protocol
- **Visibility:** Private

#### Parameters

```typescript
null;
```

#### Result

```typescript
null;
```

#### Errors

None

## Refactoring

The language server also provides refactoring operations to restructure an
internal body of code.

### `refactoring/renameProject`

This request is sent from the project manager to the server to refactor project
name in an interpreter runtime.

- **Type:** Request
- **Direction:** Project Manager -> Server
- **Connection:** Protocol
- **Visibility:** Private

#### Parameters

```typescript
{
  oldName: String;
  newName: String;
}
```

#### Result

```typescript
null;
```

#### Errors

None

## Execution Management Operations

The execution management portion of the language server API deals with exposing
fine-grained control over program and expression execution to the clients of the
language server. This is incredibly important for enabling the high levels of
interactivity required by Enso Studio.

### Execution Management Example

Given the default project structure.

```text
├── package.yaml
└── src
    └── Main.enso
```

```bash
$ cat src/Main.enso

main =
    x = 6
    y = x.foo 5
    z = y + 5
    z

Number.foo = x ->
    y = this + 3
    z = y * x
    z



#### METADATA ####
[[{"index": {"value": 98}, "size": {"value": 5}}, "5fc0c11d-bd83-4ca3-b847-b8e362f7658c"],[{"index": {"value": 81}, "size": {"value": 8}}, "1cda3676-bd62-41f8-b6a1-a1e1b7c73d18"],[{"index": {"value": 42}, "size": {"value": 5}}, "899a11e5-4d2b-43dc-a867-2f2ef2d2ba62"],[{"index": {"value": 26}, "size": {"value": 7}}, "37f284d4-c593-4e65-a4be-4948fbd2adfb"],[{"index": {"value": 16}, "size": {"value": 1}}, "c553533e-a2b9-4305-9f12-b8fe7781f933"]]
[]
```

Notice extra newline in the beginning of the `Main.enso` file, it is important
for the precalculated metadata indexes.

### Create Execution Context

```json
{
  "jsonrpc": "2.0",
  "method": "executionContext/create",
  "id": 0,
  "params": null
}
```

Return capabilities together with a newly created `ContextId`.

```json
{
  "jsonrpc": "2.0",
  "id": 0,
  "result": {
    "contextId": "1eb5ad04-4094-4c1f-be54-e9d29ddf19a3",
    "canModify": {
      "method": "executionContext/canModify",
      "registerOptions": {
        "contextId": "1eb5ad04-4094-4c1f-be54-e9d29ddf19a3"
      }
    },
    "receivesUpdates": {
      "method": "executionContext/receivesUpdates",
      "registerOptions": {
        "contextId": "1eb5ad04-4094-4c1f-be54-e9d29ddf19a3"
      }
    }
  }
}
```

### Push Item

Entering the `main` method. First item on the stack should always be an
`ExplicitCall`.

```json
{
  "jsonrpc": "2.0",
  "method": "executionContext/push",
  "id": 0,
  "params": {
    "contextId": "1eb5ad04-4094-4c1f-be54-e9d29ddf19a3",
    "stackItem": {
      "type": "ExplicitCall",
      "methodPointer": {
        "file": {
          "rootId": "18f642a2-5f69-4fc8-add6-13bf199ca326",
          "segments": ["src", "Main.enso"]
        },
        "definedOnType": "Main",
        "name": "main"
      },
      "thisArgumentExpression": null,
      "positionalArgumentsExpressions": []
    }
  }
}
```

Returns successful reponse.

```json
{
  "jsonrpc": "2.0",
  "id": 0,
  "result": null
}
```

And a value update, result of the method `foo` call defined on type `Number`.

```json
{
  "jsonrpc": "2.0",
  "method": "executionContext/expressionValuesComputed",
  "params": {
    "contextId": "1eb5ad04-4094-4c1f-be54-e9d29ddf19a3",
    "updates": [
      {
        "id": "37f284d4-c593-4e65-a4be-4948fbd2adfb",
        "type": "Number",
        "shortValue": "45",
        "methodCall": {
          "file": {
            "rootId": "18f642a2-5f69-4fc8-add6-13bf199ca326",
            "segments": ["src", "Main.enso"]
          },
          "definedOnType": "Number",
          "name": "foo"
        }
      }
    ]
  }
}
```

We can go deeper and evaluate the method `foo` call by pushing the `LocalCall`
on the stack. In general, all consequent stack items should be `LocalCall`s.

```json
{
  "jsonrpc": "2.0",
  "method": "executionContext/push",
  "id": 0,
  "params": {
    "contextId": "1eb5ad04-4094-4c1f-be54-e9d29ddf19a3",
    "stackItem": {
      "type": "LocalCall",
      "expressionId": "37f284d4-c593-4e65-a4be-4948fbd2adfb"
    }
  }
}
```

Returns successful reponse.

```json
{
  "jsonrpc": "2.0",
  "id": 0,
  "result": null
}
```

And update of some value inside the function `foo`.

```json
{
  "jsonrpc": "2.0",
  "method": "executionContext/expressionValuesComputed",
  "params": {
    "contextId": "1eb5ad04-4094-4c1f-be54-e9d29ddf19a3",
    "updates": [
      {
        "id": "1cda3676-bd62-41f8-b6a1-a1e1b7c73d18",
        "type": "Number",
        "shortValue": "9",
        "methodCall": null
      }
    ]
  }
}
```

### Pop Item

```json
{
  "jsonrpc": "2.0",
  "method": "executionContext/pop",
  "id": 0,
  "params": {
    "contextId": "1eb5ad04-4094-4c1f-be54-e9d29ddf19a3"
  }
}
```

Popping one item will return us into the `main` method. Second call will clear
the stack. Subsequent pop calls will result in an error indicating that the
stack is empty.

### `executionContext/create`

Sent from the client to the server to create a new execution context. Return
capabilities [`executionContext/canModify`](#executioncontextcanmodify) and
[`executionContext/receivesUpdates`](#executioncontextreceivesupdates).

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
null;
```

#### Result

```typescript
{
  contextId: ContextId;
  canModify: CapabilityRegistration;
  receivesUpdates: CapabilityRegistration;
}
```

#### Errors

None

### `executionContext/destroy`

Sent from the client to the server destroy an execution context and free its
resources.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
{
  contextId: ContextId;
}
```

#### Result

```typescript
null;
```

#### Errors

- [`AccessDeniedError`](#accessdeniederror) when the user does not hold the
  `executionContext/canModify` capability for this context.
- [`ContextNotFoundError`](#contextnotfounderror) when context can not be found
  by provided id.

### `executionContext/fork`

Sent from the client to the server to duplicate an execution context, creating
an independent copy, containing all the data precomputed in the first one.
Return capabilities [`executionContext/canModify`](#executioncontextcanmodify)
and [`executionContext/receivesUpdates`](#executioncontextreceivesupdates).

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
{
  contextId: ContextId;
}
```

#### Result

```typescript
{
  contextId: ContextId;
  canModify: CapabilityRegistration;
  receivesUpdates: CapabilityRegistration;
}
```

#### Errors

No known errors.

### `executionContext/push`

Sent from the client to the server execute item and move the execution context
to a new location deeper down the stack. If a stack item becomes invalid because
of a text edit (e.g. the root function of the view was removed), it will stop
executing. If the function reappears, execution should resume as normal.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
{
  contextId: ContextId;
  stackItem: StackItem;
}
```

#### Result

```typescript
null;
```

#### Errors

- [`AccessDeniedError`](#accessdeniederror) when the user does not hold the
  `executionContext/canModify` capability for this context.
- [`StackItemNotFoundError`](#stackitemnotfounderror) when the request stack
  item could not be found.
- [`InvalidStackItemError`](#invalidstackitemerror) when pushing `LocalCall` on
  top of the empty stack, or pushing `ExplicitCall` on top of non-empty stack.

### `executionContext/pop`

Sent from the client to the server move the execution context up the stack,
corresponding to the client clicking out of the current breadcrumb.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
{
  contextId: ContextId;
}
```

#### Result

```typescript
null;
```

#### Errors

- [`AccessDeniedError`](#accessdeniederror) when the user does not hold the
  `executionContext/canModify` capability for this context.
- [`EmptyStackError`](#emptystackerror) when the user tries to pop an empty
  stack.

### `executionContext/recompute`

Sent from the client to the server to force recomputation of current position.
May include a list of expressions for which caches should be invalidated.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
{
  contextId: ContextId;
  invalidatedExpressions?: "all" | [ExpressionId]
}
```

#### Result

```typescript
null;
```

#### Errors

- [`AccessDeniedError`](#accessdeniederror) when the user does not hold the
  `executionContext/canModify` capability for this context.
- [`EmptyStackError`](#emptystackerror) when the user tries to recompute an
  empty stack.

### `executionContext/expressionValuesComputed`

Sent from the server to the client to inform about new information for certain
expressions becoming available.

- **Type:** Notification
- **Direction:** Server -> Client
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
{
  contextId: ContextId;
  updates: [ExpressionValueUpdate];
}
```

#### Errors

None

### `executionContext/executionFailed`

Sent from the server to the client to inform about a critical failure when
attempting to execute a context.

When the [`executionContext/executionStatus`](#executioncontextexecutionstatus)
notifies about potential problems in the code found by compiler, or the errors
during runtime, this message signals about the errors in the logic or the
implementation. It can be a compiler crash, an attempt to execute an empty
stack, an error location a method or a module when issuing a
[`executionContext/push`](#executioncontextpush) command.

- **Type:** Notification
- **Direction:** Server -> Client
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
{
  /**
   * The identifier of the execution context.
   */
  contextId: ContextId;

  /**
   * The error message.
   */
  message: String;

  /**
   * The location of a file producing the error.
   */
  path?: Path;
}
```

#### Errors

None

### `executionContext/executionStatus`

Sent from the server to the client to inform about a status of execution.

- **Type:** Notification
- **Direction:** Server -> Client
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
{
  /**
   * The identifier of the execution context.
   */
  contextId: ContextId;

  /**
   * The list of encountered problems.
   */
  diagnostics: Diagnostic[];
}
```

#### Errors

None

### `executionContext/attachVisualisation`

This message allows the client to attach a visualisation, potentially
preprocessed by some arbitrary Enso code, to a given node in the program.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
interface AttachVisualisationRequest {
  visualisationId: UUID;
  expressionId: UUID;
  visualisationConfig: VisualisationConfiguration;
}
```

#### Result

```typescript
null;
```

#### Errors

- [`AccessDeniedError`](#accessdeniederror) when the user does not hold the
  `executionContext/canModify` capability for this context.
- [`ContextNotFoundError`](#contextnotfounderror) when context can not be found
  by provided id.
- [`ModuleNotFoundError`](#modulenotfounderror) to signal that the module with
  the visualisation cannot be found.
- [`VisualisationExpressionError`](#visualisationexpressionerror) to signal that
  the expression specified in the `VisualisationConfiguration` cannot be
  evaluated.

### `executionContext/detachVisualisation`

This message allows a client to detach a visualisation from the executing code.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
interface DetachVisualisationRequest {
  executionContextId: UUID;
  visualisationId: UUID;
  expressionId: UUID;
}
```

#### Result

```typescript
null;
```

#### Errors

- [`AccessDeniedError`](#accessdeniederror) when the user does not hold the
  `executionContext/canModify` capability for this context.
- [`ContextNotFoundError`](#contextnotfounderror) when context can not be found
  by provided id.
- [`VisualisationNotFoundError`](#visualisationnotfounderror) when a
  visualisation can not be found.

### `executionContext/modifyVisualisation`

This message allows a client to modify the configuration for an existing
visualisation.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
interface ModifyVisualisationRequest {
  visualisationId: UUID;
  visualisationConfig: VisualisationConfiguration;
}
```

#### Result

```typescript
null;
```

#### Errors

- [`AccessDeniedError`](#accessdeniederror) when the user does not hold the
  `executionContext/canModify` capability for this context.
- [`ContextNotFoundError`](#contextnotfounderror) when context can not be found
  by provided id.
- [`ModuleNotFoundError`](#modulenotfounderror) to signal that the module with
  the visualisation cannot be found.
- [`VisualisationExpressionError`](#visualisationexpressionerror) to signal that
  the expression specified in the `VisualisationConfiguration` cannot be
  evaluated.
- [`VisualisationNotFoundError`](#visualisationnotfounderror) when a
  visualisation can not be found.

### `executionContext/visualisationUpdate`

This message is responsible for providing a visualisation data update to the
client.

- **Type:** Notification
- **Direction:** Server -> Client
- **Connection:** Data
- **Visibility:** Public

The `visualisationData` component of the table definition _must_ be
pre-serialized before being inserted into this message. As far as this level of
transport is concerned, it is just a binary blob.

#### Parameters

```idl
namespace org.enso.languageserver.protocol.binary;

//A visualisation context identifying a concrete visualisation.
table VisualisationContext {

  //A visualisation identifier.
  visualisationId: EnsoUUID (required);

  //A context identifier.
  contextId: EnsoUUID (required);

  //An expression identifier.
  expressionId: EnsoUUID (required);

}

//An event signaling visualisation update.
table VisualisationUpdate {

  //A visualisation context identifying a concrete visualisation.
  visualisationContext: VisualisationContext (required);

  //A visualisation data.
  data: [ubyte] (required);

}

root_type VisualisationUpdate;
```

#### Errors

N/A

## Search Operations

Search operations allow requesting for the autocomplete suggestions and search
for the documentation. Search operations return links to the items in the
Suggestions Database instead of returning full entries. Suggestions Database is
a key-value storage with [`SuggestionEntry`](#suggestionentry) values.

### Suggestions Database Example

The following code snippet shows examples of the database entries.

```ruby
type MyType a b

type Maybe
    Nothing
    Just a

    is_just = case this of
        Just _  -> true
        Nothing -> false

foo x =
    10 - x

Number.baz x =
    this + x * 10

main =
    x = foo 42
    y = x.baz x
    IO.println y
```

#### MyType

```typescript
<SuggestionEntryAtom>{
  name: "MyType",
  arguments: [],
  returnType: "MyType",
};
```

#### Maybe.Nothing

```typescript
<SuggestionEntryAtom>{
  name: "Nothing",
  arguments: [],
  returnType: "Maybe",
};
```

#### Maybe.Just

```typescript
<SuggestionEntryAtom>{
  name: "Just",
  arguments: [
    {
      name: "a",
      type: "Any",
      isSuspended: false,
      hasDefault: false,
    },
  ],
  returnType: "Maybe",
};
```

#### Maybe.is_just

```typescript
<SuggestionEntryMethod>{
  name: "is_just",
  arguments: [],
  selfType: "Maybe",
  returnType: "Bool",
};
```

#### foo

```typescript
<SuggestionEntryFunction>{
  name: "foo",
  arguments: [
    {
      name: "x",
      type: "Number",
      isSuspended: false,
      hasDefault: false,
    },
  ],
  returnType: "Bool",
};
```

#### Number.baz

```typescript
<SuggestionEntryMethod>{
  name: "baz",
  arguments: [
    {
      name: "x",
      type: "Number",
      isSuspended: false,
      hasDefault: false,
    },
  ],
  selfType: "Number",
  returnType: "Number",
};
```

#### Local x

```typescript
<SuggestionEntryLocal>{
  name: "x",
  returnType: "Number",
};
```

#### Local y

```typescript
<SuggestionEntryLocal>{
  name: "y",
  returnType: "Number",
};
```

### `search/getSuggestionsDatabase`

Sent from client to the server to receive the full suggestions database.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
null;
```

#### Result

```typescript
{
  // The list of suggestions database entries
  entries: [SuggestionsDatabaseEntry];
  // The version of received suggestions database
  currentVersion: number;
}
```

#### Errors

- [`SuggestionsDatabaseError`](#suggestionsdatabaseerror) an error accessing the
  suggestions database
- [`ProjectNotFoundError`](#projectnotfounderror) project is not found in the
  root directory

### `search/invalidateSuggestionsDatabase`

Sent from client to the server to clean the suggestions database resetting the
version.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
null;
```

#### Result

```typescript
null;
```

#### Errors

- [`SuggestionsDatabaseError`](#suggestionsdatabaseerror) an error accessing the
  suggestions database

### `search/getSuggestionsDatabaseVersion`

Sent from client to the server to receive the current version of the suggestions
database.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
null;
```

#### Result

```typescript
{
  // The version of the suggestions database
  currentVersion: number;
}
```

#### Errors

- [`SuggestionsDatabaseError`](#suggestionsdatabaseerror) an error accessing the
  suggestions database
- [`ProjectNotFoundError`](#projectnotfounderror) project is not found in the
  root directory

### `search/suggestionsDatabaseUpdate`

Sent from server to the client to inform abouth the change in the suggestions
database.

- **Type:** Notification
- **Direction:** Server -> Client
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
{
  updates: [SuggestionsDatabaseUpdate];
  currentVersion: number;
}
```

#### Errors

None

### `search/completion`

Sent from client to the server to receive the autocomplete suggestion.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
{
  // The edited file
  file: Path;
  // The cursor position
  position: Position;
  // Filter by methods with the provided self type
  selfType?: string;
  // Filter by the return type
  returnType?: string;
  // Filter by the suggestion types
  tags?: [SuggestionEntryType];
}
```

#### Result

```typescript
{
  results: [SuggestionId];
  currentVersion: number;
}
```

#### Errors

- [`SuggestionsDatabaseError`](#suggestionsdatabaseerror) an error accessing the
  suggestions database
- [`ProjectNotFoundError`](#projectnotfounderror) project is not found in the
  root directory
- [`ModuleNameNotResolvedError`](#modulenamenotresolvederror) the module name
  cannot be extracted from the provided file path parameter

## Input/Output Operations

The input/output portion of the language server API deals with redirecting
stdin/stdout/stderr of Enso programs to the clients of the language server. This
is incredibly important for enabling the high levels of interactivity required
by Enso Studio.

### `io/redirectStandardOutput`

This message allows a client to redirect the standard output of Enso programs.
Once the standard output is redirected, the Language server will notify the
client about new output data by emitting `io/standardOutputAppended` messages.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
null;
```

#### Result

```typescript
null;
```

#### Errors

N/A

### `io/suppressStandardOutput`

This message allows a client to suppress the redirection of the standard output.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
null;
```

#### Result

```typescript
null;
```

#### Errors

N/A

### `io/standardOutputAppended`

Sent from the server to the client to inform that new output data are available
for the standard output.

- **Type:** Notification
- **Direction:** Server -> Client
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
{
  output: String;
}
```

### `io/redirectStandardError`

This message allows a client to redirect the standard error of Enso programs.
Once the standard error is redirected, the Language server will notify the
client about new output data by emitting `io/standardErrorAppended` messages.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
null;
```

#### Result

```typescript
null;
```

#### Errors

N/A

### `io/suppressStandardError`

This message allows a client to suppress the redirection of the standard error.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
null;
```

#### Result

```typescript
null;
```

#### Errors

N/A

### `io/standardErrorAppended`

Sent from the server to the client to inform that new output data are available
for the standard error.

- **Type:** Notification
- **Direction:** Server -> Client
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
{
  output: String;
}
```

### `io/feedStandardInput`

This message allows a client to feed the standard input of Enso programs.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
{
  input: String;
  isLineTerminated: Boolean;
}
```

#### Result

```typescript
null;
```

#### Errors

N/A

### `io/waitingForStandardInput`

Sent from the server to the client to inform that an Enso program is suspended
by `IO.readln`. This message is used to notify a client that she should feed the
standard input.

- **Type:** Notification
- **Direction:** Server -> Client
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
null;
```

## Errors

The language server component also has its own set of errors. This section is
not a complete specification and will be updated as new errors are added.

### `AccessDeniedError`

It signals that a user doesn't have access to a resource.

```typescript
"error" : {
  "code" : 100,
  "message" : "Access denied"
}
```

### `FileSystemError`

This error signals generic file system errors.

```typescript
"error" : {
  "code" : 1000,
  "message" : String
}
```

### `ContentRootNotFoundError`

The error informs that the requested content root cannot be found.

```typescript
"error" : {
  "code" : 1001,
  "message" : "Content root not found"
}
```

### `FileNotFound`

It signals that requested file doesn't exist.

```typescript
"error" : {
  "code" : 1003,
  "message" : "File not found"
}
```

### `FileExists`

It signals that file already exists.

```typescript
"error" : {
  "code" : 1004,
  "message" : "File already exists"
}
```

### `OperationTimeoutError`

It signals that IO operation timed out.

```typescript
"error" : {
  "code" : 1005,
  "message" : "IO operation timeout"
}
```

### `NotDirectory`

It signals that provided path is not a directory.

```typescript
"error" : {
  "code" : 1006,
  "message" : "Path is not a directory"
}
```

### `StackItemNotFoundError`

It signals that provided stack item was not found.

```typescript
"error" : {
  "code" : 2001,
  "message" : "Stack item not found"
}

```

### `ContextNotFoundError`

It signals that provided context was not found.

```typescript
"error" : {
  "code" : 2002,
  "message" : "Context not found"
}
```

### `EmptyStackError`

It signals that stack is empty.

```typescript
"error" : {
  "code" : 2003,
  "message" : "Stack is empty"
}
```

### `InvalidStackItemError`

It signals that stack is invalid in this context.

```typescript
"error" : {
  "code" : 2004,
  "message" : "Invalid stack item"
}
```

### `ModuleNotFoundError`

It signals that the given module cannot be found.

```typescript
"error" : {
  "code" : 2005,
  "message" : "Module not found [Foo.Bar.Baz]"
}
```

### `VisualisationNotFoundError`

It signals that the visualisation cannot be found.

```typescript
"error" : {
  "code" : 2006,
  "message" : "Visualisation not found"
}
```

### `VisualisationExpressionError`

It signals that the expression specified in the `VisualisationConfiguration`
cannot be evaluated.

```typescript
"error" : {
  "code" : 2007,
  "message" : "Evaluation of the visualisation expression failed [i is not defined]"
}
```

### `VisualisationEvaluationError`

It is a push message. It signals that an evaluation of a code responsible for
generating visualisation data failed.

```typescript
"error" : {
  "code" : 2008,
  "message" : "Evaluation of the visualisation failed [cannot execute foo]"
}
```

### `FileNotOpenedError`

Signals that a file wasn't opened.

```typescript
"error" : {
  "code" : 3001,
  "message" : "File not opened"
}
```

### `TextEditValidationError`

Signals that validation has failed for a series of edits.

```typescript
"error" : {
  "code" : 3002,
  "message" : "The start position is after the end position"
}
```

### `InvalidVersionError`

Signals that version provided by a client doesn't match to the version computed
by the server.

```typescript
"error" : {
  "code" : 3003,
  "message" : "Invalid version [client version: ade2967cab172183d1a67ea40cb8e92e23218764bc9934c3795fcea5, server version: 7602967cab172183d1a67ea40cb8e92e23218764bc9934c3795fcea5]"
}
```

### `WriteDeniedError`

Signals that the client doesn't hold write lock to the buffer.

```typescript
"error" : {
  "code" : 3004,
  "message" : "Write denied"
}
```

### `CapabilityNotAcquired`

Signals that requested capability is not acquired.

```typescript
"error" : {
  "code" : 5001,
  "message" : "Capability not acquired"
}
```

### `SessionNotInitialisedError`

Signals that requested cannot be proccessed, beacuse session is not initialised.

```typescript
"error" : {
  "code" : 6001,
  "message" : "Session not initialised"
}
```

### `SessionAlreadyInitialisedError`

Signals that session is already initialised.

```typescript
"error" : {
  "code" : 6002,
  "message" : "Session already initialised"
}
```

### `SuggestionsDatabaseError`

Signals about an error accessing the suggestions database.

```typescript
"error" : {
  "code" : 7001,
  "message" : "Suggestions database error"
}
```

### `ProjectNotFoundError`

Signals that the project not found in the root directory.

```typescript
"error" : {
  "code" : 7002,
  "message" : "Project not found in the root directory"
}
```

### `ModuleNameNotResolvedError`

Signals that the module name can not be resolved for the given file.

```typescript
"error" : {
  "code" : 7003,
  "message" : "Module name can't be resolved for the given file"
}
```
