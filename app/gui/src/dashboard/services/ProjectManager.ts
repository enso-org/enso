/**
 * @file The Project Manager endpoint.
 * @see https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-project-manager.md
 */
import * as backend from '#/services/Backend'
import { newtypeConstructor, type Newtype } from '#/utilities/newtype'
import { omit } from '#/utilities/object'
import { getDirectoryAndName } from '#/utilities/path'
import type { Rfc3339DateTime } from 'enso-common/src/utilities/data/dateTime'

/** Duration before the {@link ProjectManager} tries to create a WebSocket again. */
const RETRY_INTERVAL_MS = 1000
/** The maximum amount of time for which the {@link ProjectManager} should try loading. */
const MAXIMUM_DELAY_MS = 10_000

/** The Project Manager's metadata associated with a project. */
interface ProjectMetadata {
  /**
   * The ID of the project. It is only used in communication with project manager;
   * it has no semantic meaning.
   */
  readonly id: string
  /** The project variant. This is currently always `UserProject`. */
  readonly kind: 'UserProject'
  /** The date at which the project was created, in RFC3339 format. */
  readonly created: string
  /** The date at which the project was last opened, in RFC3339 format. */
  readonly lastOpened: string
}

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
export type UUID = Newtype<string, 'UUID'>
/** Create a {@link UUID}. */
export const UUID = newtypeConstructor<UUID>()
/** A filesystem path. */
export type Path = Newtype<string, 'Path'>
/** Create a {@link Path}. */
export const Path = newtypeConstructor<Path>()
/** An ID of a directory. */
export type DirectoryId = Newtype<string, 'DirectoryId'>
/** Create a {@link DirectoryId}. */
export const DirectoryId = newtypeConstructor<DirectoryId>()
/** A name of a project. */
export type ProjectName = Newtype<string, 'ProjectName'>
/** Create a {@link ProjectName}. */
export const ProjectName = newtypeConstructor<ProjectName>()
/**
 * The newtype's `TypeName` is intentionally different from the name of this type alias,
 * to match the backend's newtype.
 */
export type UTCDateTime = Rfc3339DateTime
/** Create a {@link UTCDateTime}. */
export const UTCDateTime = newtypeConstructor<UTCDateTime>()

/* eslint-enable @typescript-eslint/no-redeclare */

/** Attributes of a file or folder. */
interface Attributes {
  readonly creationTime: Rfc3339DateTime
  readonly lastAccessTime: Rfc3339DateTime
  readonly lastModifiedTime: Rfc3339DateTime
  readonly byteSize: number
}

/** Metadata for an arbitrary file system entry. */
export type FileSystemEntry = DirectoryEntry | FileEntry | ProjectEntry

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

/** The return value of the "duplicate project" endpoint. */
export interface DuplicatedProject {
  readonly projectId: UUID
  readonly projectName: string
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
type ProjectState = OpenedProjectState | OpenInProgressProjectState

/** Parameters for the "open project" endpoint. */
export interface OpenProjectParams {
  readonly projectId: UUID
  readonly missingComponentAction: MissingComponentAction
  readonly cloudProjectDirectoryPath?: string
  readonly projectsDirectory: Path
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

/** A project with its path provided instead of its id. */
type WithProjectPath<T> = Omit<T, 'projectId' | 'projectsDirectory'> & {
  readonly projectPath: Path
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

/** Possible events that may be emitted by a {@link ProjectManager}. */
export enum ProjectManagerEvents {
  // If this member is renamed, the corresponding event listener should also be renamed in
  // `app/gui/src/project-view/components/GraphEditor/toasts.ts`.
  loadingFailed = 'project-manager-loading-failed',
}

/**
 * A {@link WebSocket} endpoint to the project manager.
 *
 * It should always be in sync with the Rust interface at
 * `app/gui/controller/engine-protocol/src/project_manager.rs`.
 */
export default class ProjectManager {
  private readonly projects = new Map<Path, ProjectState>()
  private id = 0
  private reconnecting = false
  private resolvers = new Map<number, (value: never) => void>()
  private rejecters = new Map<number, (reason?: JSONRPCError) => void>()
  private socketPromise: Promise<WebSocket>

  /** Create a {@link ProjectManager} */
  constructor(
    private readonly connectionUrl: string,
    public readonly rootDirectory: Path,
  ) {
    this.socketPromise = this.reconnect()
  }

  /** Begin reconnecting the {@link WebSocket}. */
  reconnect() {
    if (this.reconnecting) {
      return this.socketPromise
    }
    this.reconnecting = true
    const firstConnectionStartMs = Number(new Date())
    let lastConnectionStartMs = 0
    let justErrored = false
    const reconnect = () => {
      lastConnectionStartMs = Number(new Date())
      this.resolvers = new Map()
      const oldRejecters = this.rejecters
      this.rejecters = new Map()
      for (const reject of oldRejecters.values()) {
        reject()
      }
      return new Promise<WebSocket>((resolve, reject) => {
        const socket = new WebSocket(this.connectionUrl)
        socket.onmessage = (event) => {
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
          this.reconnecting = false
          resolve(socket)
        }
        socket.onerror = (event) => {
          event.preventDefault()
          justErrored = true
          if (Number(new Date()) - firstConnectionStartMs > MAXIMUM_DELAY_MS) {
            document.dispatchEvent(new Event(ProjectManagerEvents.loadingFailed))
            reject(new Error())
          } else {
            const delay = RETRY_INTERVAL_MS - (Number(new Date()) - lastConnectionStartMs)
            window.setTimeout(
              () => {
                void reconnect().then(resolve)
              },
              Math.max(0, delay),
            )
          }
        }
        socket.onclose = () => {
          if (!justErrored) {
            this.socketPromise = reconnect()
          }
          justErrored = false
        }
      })
    }
    this.socketPromise = reconnect()
    return this.socketPromise
  }

  /** Dispose of the {@link ProjectManager}. */
  async dispose() {
    const socket = await this.socketPromise
    socket.close()
  }

  /** Get the state of a project given its path. */
  getProjectState(projectPath: Path) {
    return this.projects.get(projectPath)
  }

  /** Open an existing project. */
  async openProject(params: WithProjectPath<OpenProjectParams>): Promise<OpenProject> {
    const fullParams: OpenProjectParams = await this.paramsWithPathToWithId(params)
    const cached = this.projects.get(params.projectPath)
    if (cached) {
      return cached.data
    } else {
      const promise = this.sendRequest<OpenProject>('project/open', fullParams)
      this.projects.set(params.projectPath, {
        state: backend.ProjectState.openInProgress,
        data: promise,
      })
      try {
        const result = await promise
        this.projects.set(params.projectPath, {
          state: backend.ProjectState.opened,
          data: result,
        })
        return result
      } catch (error) {
        this.projects.delete(params.projectPath)
        throw error
      }
    }
  }

  /** Close an open project. */
  async closeProject(params: WithProjectPath<CloseProjectParams>): Promise<void> {
    const state = this.projects.get(params.projectPath)
    if (state?.state === backend.ProjectState.openInProgress) {
      // Projects that are not opened cannot be closed.
      // This is the only way to wait until the project is open.
      await this.openProject({
        projectPath: params.projectPath,
        missingComponentAction: MissingComponentAction.install,
      })
    }
    const fullParams: CloseProjectParams = await this.paramsWithPathToWithId(params)
    this.projects.delete(params.projectPath)
    return this.sendRequest('project/close', fullParams)
  }

  /** Create a new project. */
  async createProject(params: CreateProjectParams): Promise<CreateProject> {
    return await this.sendRequest<CreateProject>('project/create', {
      missingComponentAction: MissingComponentAction.install,
      ...params,
    })
  }

  /** Rename a project. */
  async renameProject(params: WithProjectPath<RenameProjectParams>): Promise<void> {
    const fullParams: RenameProjectParams = await this.paramsWithPathToWithId(params)
    await this.sendRequest('project/rename', fullParams)
    const state = this.projects.get(params.projectPath)
    if (state?.state === backend.ProjectState.opened) {
      this.projects.set(params.projectPath, {
        state: state.state,
        data: { ...state.data, projectName: params.name },
      })
    }
  }

  /** Duplicate a project. */
  async duplicateProject(
    params: WithProjectPath<DuplicateProjectParams>,
  ): Promise<DuplicatedProject> {
    const fullParams: DuplicateProjectParams = await this.paramsWithPathToWithId(params)
    return await this.sendRequest<DuplicatedProject>('project/duplicate', fullParams)
  }

  /** Delete a project. */
  async deleteProject(params: WithProjectPath<DeleteProjectParams>): Promise<void> {
    const fullParams: DeleteProjectParams = await this.paramsWithPathToWithId(params)
    const cached = this.projects.get(params.projectPath)
    if (cached && backend.IS_OPENING_OR_OPENED[cached.state]) {
      await this.closeProject({ projectPath: params.projectPath })
    }
    await this.sendRequest('project/delete', fullParams)
    this.projects.delete(params.projectPath)
  }

  /** Get a project's metadata by its path. */
  async getProjectMetadata(path: Path) {
    const response = await fetch(`/api/project-${encodeURIComponent(path)}/metadata`)
    if (!response.ok) {
      return null
    }
    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
    const metadata: ProjectMetadata = await response.json()
    return metadata
  }

  /** Get a project's metadata by its path, throwing an error if it is not present. */
  async getProjectMetadataStrict(path: Path) {
    const metadata = await this.getProjectMetadata(path)
    if (!metadata) {
      throw new Error(`Project with path '${path}' does not exist`)
    }
    return metadata
  }

  /** Remove all handlers for a specified request ID. */
  private cleanup(id: number) {
    this.resolvers.delete(id)
    this.rejecters.delete(id)
  }

  /**
   * Convert {@link WithProjectPath<T>} to `T`.
   * @throws {Error} when the `id` is not cached.
   */
  private async paramsWithPathToWithId<T>(obj: WithProjectPath<T>) {
    const { id } = await this.getProjectMetadataStrict(obj.projectPath)
    const directoryPath = getDirectoryAndName(obj.projectPath).directoryPath
    return {
      ...omit(obj, 'projectPath'),
      projectId: UUID(id),
      projectsDirectory: directoryPath,
    }
  }

  /** Send a JSON-RPC request to the project manager. */
  private async sendRequest<T = void>(method: string, params: unknown): Promise<T> {
    const socket = await this.socketPromise
    const id = this.id++
    socket.send(JSON.stringify({ jsonrpc: '2.0', id, method, params }))
    return new Promise<T>((resolve, reject) => {
      this.resolvers.set(id, (value) => {
        this.cleanup(id)
        resolve(value)
      })
      this.rejecters.set(id, (value) => {
        this.cleanup(id)
        // eslint-disable-next-line @typescript-eslint/prefer-promise-reject-errors
        reject(value)
      })
    })
  }
}
