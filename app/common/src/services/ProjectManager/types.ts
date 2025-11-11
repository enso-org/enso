/**
 * @file This module defines the Project Manager types.
 * @see
 * https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-project-manager.md
 */
import type * as dateTime from '../../utilities/data/dateTime.js'
import * as newtype from '../../utilities/data/newtype.js'
import type * as backend from '../Backend.js'

/** Possible actions to take when a component is missing. */
export enum MissingComponentAction {
  fail = 'Fail',
  install = 'Install',
  forceInstallBroken = 'ForceInstallBroken',
}

/** Metadata for a JSON-RPC error. */
export interface JSONRPCError {
  readonly code: number
  readonly message: string
  readonly data?: unknown
}

/** Fields common to all return values of any JSON-RPC call. */
interface JSONRPCBaseResponse {
  readonly jsonrpc: '2.0'
  readonly id: number
}

/** The return value of a successful JSON-RPC call. */
interface JSONRPCSuccessResponse<T> extends JSONRPCBaseResponse {
  readonly result: T
}

/** The return value of a failed JSON-RPC call. */
interface JSONRPCErrorResponse extends JSONRPCBaseResponse {
  readonly error: JSONRPCError
}

/** The return value of a JSON-RPC call. */
export type JSONRPCResponse<T> = JSONRPCErrorResponse | JSONRPCSuccessResponse<T>

/** A UUID. */
export type UUID = newtype.Newtype<string, 'UUID'>
/** Create a {@link UUID}. */
export const UUID = newtype.newtypeConstructor<UUID>()

/** A filesystem path. */
export type Path = newtype.Newtype<string, 'Path'>
/** Create a {@link Path}. */
export const Path = newtype.newtypeConstructor<Path>()

/** An ID of a directory. */
export type DirectoryId = newtype.Newtype<string, 'DirectoryId'>
/** Create a {@link DirectoryId}. */
export const DirectoryId = newtype.newtypeConstructor<DirectoryId>()

/** A name of a project. */
export type ProjectName = newtype.Newtype<string, 'ProjectName'>
/** Create a {@link ProjectName}. */
export const ProjectName = newtype.newtypeConstructor<ProjectName>()
/**
 * The newtype's `TypeName` is intentionally different from the name of this type alias,
 * to match the backend's newtype.
 */
export type UTCDateTime = dateTime.Rfc3339DateTime
/** Create a {@link UTCDateTime}. */
export const UTCDateTime = newtype.newtypeConstructor<UTCDateTime>()

/** Details of a project. */
export interface ProjectMetadata {
  /** The name of the project. */
  readonly name: string
  /** The namespace of the project. */
  readonly namespace: string
  /** The project id. */
  readonly id: UUID
  /**
   * The Enso Engine version to use for the project, represented by a semver version
   * string.
   *
   * If the edition associated with the project could not be resolved, the
   * engine version may be missing.
   */
  readonly engineVersion?: string
  /** The project creation time. */
  readonly created: dateTime.Rfc3339DateTime
  /** The last opened datetime. */
  readonly lastOpened?: dateTime.Rfc3339DateTime
}

/** Attributes of a file or folder. */
export interface Attributes {
  readonly creationTime: dateTime.Rfc3339DateTime
  readonly lastAccessTime: dateTime.Rfc3339DateTime
  readonly lastModifiedTime: dateTime.Rfc3339DateTime
  readonly byteSize: number
}

/** Metadata for an arbitrary file system entry. */
export type FileSystemEntry = DirectoryEntry | FileEntry | ProjectEntry

/** Metadata for a file. */
export interface FileEntry {
  readonly type: 'FileEntry'
  readonly path: Path
  readonly attributes: Attributes
}

/** Metadata for a directory. */
export interface DirectoryEntry {
  readonly type: 'DirectoryEntry'
  readonly path: Path
  readonly attributes: Attributes
}

/** Metadata for a project. */
export interface ProjectEntry {
  readonly type: 'ProjectEntry'
  readonly path: Path
  readonly metadata: ProjectMetadata
  readonly attributes: Attributes
}

/** A value specifying the hostname and port of a socket. */
export interface IpWithSocket {
  readonly host: string
  readonly port: number
}

/** The return value of the "create project" endpoint. */
export interface CreateProject {
  readonly projectId: UUID
  readonly projectName: string
  readonly projectPath: Path
  readonly projectNormalizedName: string
}

/** The return value of the "open project" endpoint. */
export interface OpenProject {
  readonly languageServerJsonAddress: IpWithSocket
  readonly languageServerBinaryAddress: IpWithSocket
  readonly languageServerYdocAddress?: IpWithSocket
  readonly projectName: ProjectName
  readonly projectNormalizedName: string
  readonly projectNamespace: string
}

/** The return value of the "list available engine versions" endpoint. */
export interface EngineVersion {
  readonly version: string
  readonly markedAsBroken: boolean
}

/** The return value of the "list available engine versions" endpoint. */
export interface VersionList {
  readonly versions: readonly EngineVersion[]
}

/** The return value of the "duplicate project" endpoint. */
export interface DuplicatedProject {
  readonly projectId: UUID
  readonly projectName: string
  readonly projectPath: Path
  readonly projectNormalizedName: string
}

/** A project that is currently opening. */
interface OpenInProgressProjectState {
  readonly state: backend.ProjectState.openInProgress
  readonly data: Promise<OpenProject>
}

/** A project that is currently opened. */
interface OpenedProjectState {
  readonly state: backend.ProjectState.opened
  readonly data: OpenProject
}

/**
 * Possible states and associated metadata of a project.
 * The "closed" state is omitted as it is the default state.
 */
export type ProjectState = OpenedProjectState | OpenInProgressProjectState

/**
 * Extra parameters required for cloud projects.
 */
export interface CloudParams {
  readonly cloudProjectDirectoryPath: string
  readonly cloudProjectId: string
  readonly cloudProjectSessionId: string
}

/** Parameters for the "open project" endpoint. */
export interface OpenProjectParams {
  readonly projectId: UUID
  readonly missingComponentAction: MissingComponentAction
  readonly projectsDirectory: Path
  readonly cloud?: CloudParams
}

/** Parameters for the "close project" endpoint. */
export interface CloseProjectParams {
  readonly projectId: UUID
}

/** Parameters for the "create project" endpoint. */
export interface CreateProjectParams {
  readonly name: ProjectName
  readonly projectTemplate?: string
  readonly version?: string
  readonly missingComponentAction?: MissingComponentAction
  readonly projectsDirectory?: Path
}

/** Parameters for the "rename project" endpoint. */
export interface RenameProjectParams {
  readonly projectId: UUID
  readonly name: ProjectName
  readonly projectsDirectory: Path
}

/** Parameters for the "duplicate project" endpoint. */
export interface DuplicateProjectParams {
  readonly projectId: UUID
  readonly projectsDirectory: Path
}

/** Parameters for the "delete project" endpoint. */
export interface DeleteProjectParams {
  readonly projectId: UUID
  readonly projectsDirectory: Path
}
