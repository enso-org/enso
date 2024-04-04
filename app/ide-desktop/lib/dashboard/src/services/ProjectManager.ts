/** @file This module defines the Project Manager endpoint.
 * @see
 * https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-project-manager.md */
import * as backend from '#/services/Backend'

import * as appBaseUrl from '#/utilities/appBaseUrl'
import type * as dateTime from '#/utilities/dateTime'
import * as newtype from '#/utilities/newtype'

// =================
// === Constants ===
// =================

/** Duration before the {@link ProjectManager} tries to create a WebSocket again. */
const RETRY_INTERVAL_MS = 1000
/** The maximum amount of time for which the {@link ProjectManager} should try loading. */
const MAXIMUM_DELAY_MS = 10_000

// =============
// === Types ===
// =============

/** Possible actions to take when a component is missing. */
export enum MissingComponentAction {
  fail = 'Fail',
  install = 'Install',
  forceInstallBroken = 'ForceInstallBroken',
}

/** Metadata for a JSON-RPC error. */
interface JSONRPCError {
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

// These are constructor functions that construct values of the type they are named after.
/* eslint-disable @typescript-eslint/no-redeclare */

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
/** The newtype's `TypeName` is intentionally different from the name of this type alias,
 * to match the backend's newtype. */
export type UTCDateTime = dateTime.Rfc3339DateTime
/** Create a {@link UTCDateTime}. */
export const UTCDateTime = newtype.newtypeConstructor<UTCDateTime>()

/* eslint-enable @typescript-eslint/no-redeclare */

/** Details of a project. */
interface ProjectMetadata {
  /** The name of the project. */
  readonly name: string
  /** The namespace of the project. */
  readonly namespace: string
  /** The project id. */
  readonly id: UUID
  /** The Enso Engine version to use for the project, represented by a semver version
   * string.
   *
   * If the edition associated with the project could not be resolved, the
   * engine version may be missing. */
  readonly engineVersion?: string
  /** The project creation time. */
  readonly created: dateTime.Rfc3339DateTime
  /** The last opened datetime. */
  readonly lastOpened?: dateTime.Rfc3339DateTime
}

/** Attributes of a file or folder. */
interface Attributes {
  readonly creationTime: dateTime.Rfc3339DateTime
  readonly lastAccessTime: dateTime.Rfc3339DateTime
  readonly lastModifiedTime: dateTime.Rfc3339DateTime
  readonly byteSize: number
}

/** Metadata for an arbitrary file system entry. */
type FileSystemEntry = DirectoryEntry | FileEntry | ProjectEntry

/** The discriminator value for {@link FileSystemEntry}. */
export enum FileSystemEntryType {
  DirectoryEntry = 'DirectoryEntry',
  ProjectEntry = 'ProjectEntry',
  FileEntry = 'FileEntry',
}

/** Metadata for a file. */
interface FileEntry {
  readonly type: FileSystemEntryType.FileEntry
  readonly path: Path
  readonly attributes: Attributes
}

/** Metadata for a directory. */
interface DirectoryEntry {
  readonly type: FileSystemEntryType.DirectoryEntry
  readonly path: Path
  readonly attributes: Attributes
}

/** Metadata for a project. */
interface ProjectEntry {
  readonly type: FileSystemEntryType.ProjectEntry
  readonly path: Path
  readonly metadata: ProjectMetadata
  readonly attributes: Attributes
}

/** A value specifying the hostname and port of a socket. */
export interface IpWithSocket {
  readonly host: string
  readonly port: number
}

/** The return value of the "list projects" endpoint. */
export interface ProjectList {
  readonly projects: ProjectMetadata[]
}

/** The return value of the "create project" endpoint. */
export interface CreateProject {
  readonly projectId: UUID
  readonly projectName: string
  readonly projectNormalizedName: string
}

/** The return value of the "open project" endpoint. */
export interface OpenProject {
  readonly engineVersion: string
  readonly languageServerJsonAddress: IpWithSocket
  readonly languageServerBinaryAddress: IpWithSocket
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
  readonly versions: EngineVersion[]
}

// ====================
// === ProjectState ===
// ====================

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

/** Possible states and associated metadata of a project.
 * The "closed" state is omitted as it is the default state. */
type ProjectState = OpenedProjectState | OpenInProgressProjectState

// ================================
// === Parameters for endpoints ===
// ================================

/** Parameters for the "open project" endpoint. */
export interface OpenProjectParams {
  readonly projectId: UUID
  readonly missingComponentAction: MissingComponentAction
  readonly projectsDirectory?: string
}

/** Parameters for the "close project" endpoint. */
export interface CloseProjectParams {
  readonly projectId: UUID
}

/** Parameters for the "list projects" endpoint. */
export interface ListProjectsParams {
  readonly numberOfProjects?: number
}

/** Parameters for the "create project" endpoint. */
export interface CreateProjectParams {
  readonly name: ProjectName
  readonly projectTemplate?: string
  readonly version?: string
  readonly missingComponentAction?: MissingComponentAction
  readonly projectsDirectory?: Path
}

/** Parameters for the "list samples" endpoint. */
export interface RenameProjectParams {
  readonly projectId: UUID
  readonly name: ProjectName
  readonly projectsDirectory?: Path
}

/** Parameters for the "delete project" endpoint. */
export interface DeleteProjectParams {
  readonly projectId: UUID
  readonly projectsDirectory?: Path
}

// ================
// === joinPath ===
// ================

/** Construct a {@link Path} from an existing {@link Path} of the parent directory. */
export function joinPath(directoryPath: Path, fileName: string) {
  return Path(`${directoryPath}/${fileName}`)
}

// =======================
// === Project Manager ===
// =======================

/** Possible events that may be emitted by a {@link ProjectManager}. */
export enum ProjectManagerEvents {
  loadingFailed = 'project-manager-loading-failed',
}

/** A {@link WebSocket} endpoint to the project manager.
 *
 * It should always be in sync with the Rust interface at
 * `app/gui/controller/engine-protocol/src/project_manager.rs`. */
export default class ProjectManager {
  private readonly internalProjects = new Map<UUID, ProjectState>()
  // This MUST be declared after `internalProjects` because it depends on `internalProjects.
  // eslint-disable-next-line @typescript-eslint/member-ordering
  readonly projects: ReadonlyMap<UUID, ProjectState> = this.internalProjects
  private id = 0
  private resolvers = new Map<number, (value: never) => void>()
  private rejecters = new Map<number, (reason?: JSONRPCError) => void>()
  private socketPromise: Promise<WebSocket>

  /** Create a {@link ProjectManager} */
  constructor(
    private readonly connectionUrl: string,
    readonly rootDirectory: Path
  ) {
    const firstConnectionStartMs = Number(new Date())
    let lastConnectionStartMs = 0
    let justErrored = false
    const createSocket = () => {
      lastConnectionStartMs = Number(new Date())
      this.resolvers = new Map()
      const oldRejecters = this.rejecters
      this.rejecters = new Map()
      for (const reject of oldRejecters.values()) {
        reject()
      }
      return new Promise<WebSocket>((resolve, reject) => {
        const socket = new WebSocket(this.connectionUrl)
        socket.onmessage = event => {
          // There is no way to avoid this as `JSON.parse` returns `any`.
          // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-argument
          const message: JSONRPCResponse<never> = JSON.parse(event.data)
          if ('result' in message) {
            this.resolvers.get(message.id)?.(message.result)
          } else {
            this.rejecters.get(message.id)?.(message.error)
          }
        }
        socket.onopen = () => {
          resolve(socket)
        }
        socket.onerror = event => {
          event.preventDefault()
          justErrored = true
          if (Number(new Date()) - firstConnectionStartMs > MAXIMUM_DELAY_MS) {
            document.dispatchEvent(new Event(ProjectManagerEvents.loadingFailed))
            reject()
          } else {
            const delay = RETRY_INTERVAL_MS - (Number(new Date()) - lastConnectionStartMs)
            window.setTimeout(
              () => {
                void createSocket().then(resolve)
              },
              Math.max(0, delay)
            )
          }
        }
        socket.onclose = () => {
          if (!justErrored) {
            this.socketPromise = createSocket()
          }
          justErrored = false
        }
      })
    }
    this.socketPromise = createSocket()
  }

  /** Open an existing project. */
  async openProject(params: OpenProjectParams): Promise<OpenProject> {
    const cached = this.internalProjects.get(params.projectId)
    if (cached) {
      return cached.data
    } else {
      const promise = this.sendRequest<OpenProject>('project/open', params)
      this.internalProjects.set(params.projectId, {
        state: backend.ProjectState.openInProgress,
        data: promise,
      })
      const result = await promise
      this.internalProjects.set(params.projectId, {
        state: backend.ProjectState.opened,
        data: result,
      })
      return result
    }
  }

  /** Close an open project. */
  async closeProject(params: CloseProjectParams): Promise<void> {
    this.internalProjects.delete(params.projectId)
    return this.sendRequest('project/close', params)
  }

  /** Get the projects list, sorted by open time. */
  async listProjects(params: ListProjectsParams): Promise<ProjectList> {
    return this.sendRequest<ProjectList>('project/list', params)
  }

  /** Create a new project. */
  async createProject(params: CreateProjectParams): Promise<CreateProject> {
    return this.sendRequest<CreateProject>('project/create', {
      missingComponentAction: MissingComponentAction.install,
      ...params,
    })
  }

  /** Rename a project. */
  async renameProject(params: RenameProjectParams): Promise<void> {
    return this.sendRequest('project/rename', params)
  }

  /** Delete a project. */
  async deleteProject(params: DeleteProjectParams): Promise<void> {
    this.internalProjects.delete(params.projectId)
    return this.sendRequest('project/delete', params)
  }

  /** List installed engine versions. */
  async listInstalledEngineVersions(): Promise<VersionList> {
    return await this.sendRequest<VersionList>('engine/list-installed', {})
  }

  /** List available engine versions. */
  async listAvailableEngineVersions(): Promise<VersionList> {
    return await this.sendRequest<VersionList>('engine/list-available', {})
  }

  /** List directories, projects and files in the given folder. */
  async listDirectory(parentId: Path | null) {
    /** The type of the response body of this endpoint. */
    interface ResponseBody {
      readonly entries: FileSystemEntry[]
    }
    const response = await this.runStandaloneCommand<ResponseBody>(
      null,
      'filesystem-list',
      parentId ?? this.rootDirectory
    )
    return response.entries
  }

  /** Create a directory. */
  async createDirectory(path: Path) {
    await this.runStandaloneCommand(null, 'filesystem-create-directory', path)
  }

  /** Create a file. */
  async createFile(path: Path, file: Blob) {
    await this.runStandaloneCommand(file, 'filesystem-write-path', path)
  }

  /** Move a file or directory. */
  async moveFile(from: Path, to: Path) {
    await this.runStandaloneCommand(null, 'filesystem-move-from', from, '--filesystem-move-to', to)
  }

  /** Create a file or directory. */
  async deleteFile(path: Path) {
    await this.runStandaloneCommand(null, 'filesystem-delete', path)
  }

  /** Remove all handlers for a specified request ID. */
  private cleanup(id: number) {
    this.resolvers.delete(id)
    this.rejecters.delete(id)
  }

  /** Send a JSON-RPC request to the project manager. */
  private async sendRequest<T = void>(method: string, params: unknown): Promise<T> {
    const socket = await this.socketPromise
    const id = this.id++
    socket.send(JSON.stringify({ jsonrpc: '2.0', id, method, params }))
    return new Promise<T>((resolve, reject) => {
      this.resolvers.set(id, value => {
        this.cleanup(id)
        resolve(value)
      })
      this.rejecters.set(id, value => {
        this.cleanup(id)
        reject(value)
      })
    })
  }

  /** Run the Project Manager binary with the given command-line arguments. */
  private async runStandaloneCommand<T = void>(
    body: BodyInit | null,
    name: string,
    ...cliArguments: string[]
  ): Promise<T> {
    const searchParams = new URLSearchParams({
      // The names come from a third-party API and cannot be changed.
      // eslint-disable-next-line @typescript-eslint/naming-convention
      'cli-arguments': JSON.stringify([`--${name}`, ...cliArguments]),
    }).toString()
    const response = await fetch(
      `${appBaseUrl.APP_BASE_URL}/api/run-project-manager-command?${searchParams}`,
      {
        method: 'POST',
        body,
      }
    )
    // There is no way to avoid this as `JSON.parse` returns `any`.
    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-argument
    const json: JSONRPCResponse<never> = await response.json()
    if ('result' in json) {
      return json.result
    } else {
      throw new Error(json.error.message)
    }
  }
}
