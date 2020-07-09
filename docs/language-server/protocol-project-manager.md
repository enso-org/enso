---
layout: developer-doc
title: Enso Protocol Project Manager Message Specification
category: language-server
tags: [language-server, protocol, specification]
order: 3
---

# Enso Protocol Project Manager Message Specification
This document contains the specification of the Enso protocol messages that
pertain to the project manager component. Please familiarise yourself with the
[common](./protocol-common.md) features of the protocol before reading this
document.

For information on the design and architecture of the protocol, as well as its
transport formats, please look [here](./protocol-architecture).

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Types](#types)
  - [`ProjectMetadata`](#projectmetadata)
- [Project Management Operations](#project-management-operations)
  - [`project/open`](#projectopen)
  - [`project/close`](#projectclose)
  - [`project/list`](#projectlist)
  - [`project/create`](#projectcreate)
  - [`project/rename`](#projectrename)
  - [`project/delete`](#projectdelete)
  - [`project/listSample`](#projectlistsample)
- [Language Server Management](#language-server-management)
- [Errors](#errors)
  - [`ProjectNameValidationError`](#projectnamevalidationerror)
  - [`ProjectDataStoreError`](#projectdatastoreerror)
  - [`ProjectExistsError`](#projectexistserror)
  - [`ProjectNotFoundError`](#projectnotfounderror)
  - [`ProjectOpenError`](#projectopenerror)
  - [`ProjectCloseError`](#projectcloseerror)
  - [`ProjectNotOpenError`](#projectnotopenerror)
  - [`ProjectOpenByOtherPeersError`](#projectopenbyotherpeerserror)
  - [`CannotRemoveOpenProjectError`](#cannotremoveopenprojecterror)

<!-- /MarkdownTOC -->

## Types
There are a number of types that are used only within the project server's
protocol messages. These are specified here.

### `ProjectMetadata`
This type represents information about a project.

#### Format

```typescript
interface ProjectMetadata {
  name: String;
  id: UUID;
  lastOpened: UTCDateTime;
}
```

## Project Management Operations
The primary responsibility of the project managers is to allow users to manage
their projects.

### `project/open`
This message requests that the project manager open a specified project. This
operation also includes spawning an instance of the language server open on the
specified project.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
interface ProjectOpenRequest {
  projectId: UUID;
}
```

#### Result

```typescript
interface ProjectOpenResult {
  languageServerJsonAddress: IPWithSocket;
  languageServerBinaryAddress: IPWithSocket;
}
```

#### Errors
- [`ProjectNotFoundError`](#projectnotfounderror) to signal that the project
  doesn't exist.
- [`ProjectDataStoreError`](#projectdatastoreerror) to signal problems with
  underlying data store.
- [`ProjectOpenError`](#projectopenerror) to signal failures during server boot.

### `project/close`
This message requests that the project manager close a specified project. This
operation includes shutting down the language server gracefully so that it can
persist state to disk as needed.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
interface ProjectCloseRequest {
  projectId: UUID;
}
```

#### Result

```typescript
{}
```

#### Errors
- [`ProjectNotFoundError`](#projectnotfounderror) to signal that the project
  doesn't exist.
- [`ProjectDataStoreError`](#projectdatastoreerror) to signal problems with
  underlying data store.
- [`ProjectCloseError`](#projectcloseerror) to signal failures that occurred
  during language server stoppage.
- [`ProjectNotOpenError`](#projectnotopenerror) to signal cannot close a project
  that is not open.
- [`ProjectOpenByOtherPeersError`](#projectopenbyotherpeerserror) to signal
  that cannot close a project that is open by other clients.

### `project/list`
This message requests that the project manager lists all user's projects. The 
list of projects is sorted by the open time.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
interface ProjectListRequest {
  numberOfProjects?: Int;
}
```

#### Result

```typescript
interface ProjectListResponse {
  projects: [ProjectMetadata];
}
```

#### Errors
- [`ProjectDataStoreError`](#projectdatastoreerror) to signal problems with
  underlying data store.

### `project/create`
This message requests the creation of a new project.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
interface ProjectCreateRequest {
  name: String;
}
```

#### Result

```typescript
interface ProjectOpenResponse {
  projectId: UUID;
}
```

#### Errors
- [`ProjectNameValidationError`](#projectnamevalidationerror) to signal
  validation failures.
- [`ProjectDataStoreError`](#projectdatastoreerror) to signal problems with
  underlying data store.
- [`ProjectExistsError`](#projectexistserror) to signal that the project
  already exists.

### `project/rename`
This message requests the renaming of a project.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
interface ProjectRenameRequest {
  projectId: UUID;
  name: String;
}
```

#### Result

```
null
```

#### Errors
- [`ProjectNameValidationError`](#projectnamevalidationerror) to signal
  validation failures.
- [`ProjectDataStoreError`](#projectdatastoreerror) to signal problems with
  underlying data store.
- [`ProjectExistsError`](#projectexistserror) to signal that the project with
  the provided name already exists.
- [`ServiceError`](#serviceerror) to signal that the 
  the operation timed out.
- [`LanguageServerError`](#languageservererror) to signal generic language
  server failures.

### `project/delete`
This message requests the deletion of a project.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
interface ProjectDeleteRequest {
  projectId: UUID;
}
```

#### Result

```typescript
{}
```

#### Errors
- [`ProjectDataStoreError`](#projectdatastoreerror) to signal problems with
  underlying data store.
- [`ProjectNotFoundError`](#projectnotfounderror) to signal that the project
  doesn't exist.
- [`CannotRemoveOpenProjectError`](#cannotremoveopenprojecterror) to signal that
  the project cannot be removed, because is open by at least one user.


### `project/listSample`
This request lists the sample projects that are available to the user.

- **Type:** Request
- **Direction:** Client -> Server
- **Connection:** Protocol
- **Visibility:** Public

#### Parameters

```typescript
interface ProjectListSampleRequest {
  numProjects: Int;
}
```

#### Result

```typescript
interface ProjectListSampleResponse {
  projects: [ProjectMetadata];
}
```

#### Errors
TBC

## Language Server Management
The project manager is also responsible for managing the language server. This
means that it needs to be able to spawn the process, but also tell the process
when to shut down.

> The actionables for this section are:
>
> - Fill it in when we have more of an idea about exactly how this spawning
>   relationship is going to work.

## Errors
The project manager component has its own set of errors. This section is not a
complete specification and will be updated as new errors are added.


### `ProjectNameValidationError`
Signals validation failures.

```typescript
"error" : {
  "code" : 4001,
  "message" : "Cannot create project with empty name"
}
```

### `ProjectDataStoreError`
Signals problems with underlying data store.

```typescript
"error" : {
  "code" : 4002,
  "message" : "Cannot load project index"
}
```

### `ProjectExistsError`
Signals that the project already exists.

```typescript
"error" : {
  "code" : 4003,
  "message" : "Project with the provided name exists"
}
```

### `ProjectNotFoundError`
Signals that the project doesn't exist.

```typescript
"error" : {
  "code" : 4004,
  "message" : "Project with the provided id does not exist"
}
```


### `ProjectOpenError`
Signals that the project cannot be open due to boot failures.

```typescript
"error" : {
  "code" : 4005,
  "message" : "A boot failure."
}
```

### `ProjectNotOpenError`
Signals that cannot close project that is not open.

```typescript
"error" : {
  "code" : 4006,
  "message" : "Cannot close project that is not open"
}
```

### `ProjectOpenByOtherPeersError`
Signals that cannot close a project that is open by other clients.

```typescript
"error" : {
  "code" : 4007,
  "message" : "Cannot close project because it is open by other peers"
}
```

### `CannotRemoveOpenProjectError`
Signals that cannot remove open project.

```typescript
"error" : {
  "code" : 4008,
  "message" : "Cannot remove open project"
}
```

### `ProjectCloseError`
Signals failures during shutdown of a server.

```typescript
"error" : {
  "code" : 4009,
  "message" : "A shutdown failure."
}
```

### `LanguageServerError`
Signals generic language server errors.

```typescript
"error" : {
  "code" : 4010,
  "message" : "The language server is unresponsive"
}
```