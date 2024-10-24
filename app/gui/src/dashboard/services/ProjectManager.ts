/**
 * @file This module defines the Project Manager endpoint.
 * @see
 * https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-project-manager.md
 */
import invariant from 'tiny-invariant'

import * as backend from '#/services/Backend'
import * as appBaseUrl from '#/utilities/appBaseUrl'
import * as dateTime from '#/utilities/dateTime'
import * as newtype from '#/utilities/newtype'
import { getDirectoryAndName, normalizeSlashes } from '#/utilities/path'

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
/**
 * The newtype's `TypeName` is intentionally different from the name of this type alias,
 * to match the backend's newtype.
 */
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
  readonly versions: readonly EngineVersion[]
}

/** The return value of the "duplicate project" endpoint. */
export interface DuplicatedProject {
  readonly projectId: UUID
  readonly projectName: string
  readonly projectNormalizedName: string
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

/**
 * Possible states and associated metadata of a project.
 * The "closed" state is omitted as it is the default state.
 */
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

/** Parameters for the "rename project" endpoint. */
export interface RenameProjectParams {
  readonly projectId: UUID
  readonly name: ProjectName
  readonly projectsDirectory?: Path
}

/** Parameters for the "duplicate project" endpoint. */
export interface DuplicateProjectParams {
  readonly projectId: UUID
  readonly projectsDirectory?: Path
}

/** Parameters for the "delete project" endpoint. */
export interface DeleteProjectParams {
  readonly projectId: UUID
  readonly projectsDirectory?: Path
}

// =======================
// === Project Manager ===
// =======================

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
  private readonly initialRootDirectory: Path
  // This is required so that projects get recursively updated (deleted, renamed or moved).
  private readonly internalDirectories = new Map<Path, readonly FileSystemEntry[]>()
  private readonly internalProjects = new Map<UUID, ProjectState>()
  private readonly internalProjectPaths = new Map<UUID, Path>()
  // This MUST be declared after `internalDirectories` because it depends on `internalDirectories`.
  // eslint-disable-next-line @typescript-eslint/member-ordering
  readonly directories: ReadonlyMap<UUID, ProjectState> = this.internalProjects
  // This MUST be declared after `internalProjects` because it depends on `internalProjects`.
  // eslint-disable-next-line @typescript-eslint/member-ordering
  readonly projects: ReadonlyMap<UUID, ProjectState> = this.internalProjects
  // This MUST be declared after `internalProjectPaths` because it depends on `internalProjectPaths`.
  // eslint-disable-next-line @typescript-eslint/member-ordering
  readonly projectPaths: ReadonlyMap<UUID, Path> = this.internalProjectPaths
  private id = 0
  private reconnecting = false
  private resolvers = new Map<number, (value: never) => void>()
  private rejecters = new Map<number, (reason?: JSONRPCError) => void>()
  private socketPromise: Promise<WebSocket>

  /** Create a {@link ProjectManager} */
  constructor(
    private readonly connectionUrl: string,
    public rootDirectory: Path,
  ) {
    this.initialRootDirectory = this.rootDirectory
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

  /** Set the root directory to the initial root directory. */
  resetRootDirectory() {
    this.rootDirectory = this.initialRootDirectory
  }

  /** Dispose of the {@link ProjectManager}. */
  async dispose() {
    const socket = await this.socketPromise
    socket.close()
  }

  /** Get the path of a project. */
  getProjectPath(projectId: UUID) {
    const projectPath = this.internalProjectPaths.get(projectId)
    invariant(projectPath, `Unknown project path for project '${projectId}'.`)
    return projectPath
  }

  /** Get the directory path of a project. */
  getProjectDirectoryPath(projectId: UUID) {
    const projectPath = this.internalProjectPaths.get(projectId)
    return projectPath == null ? this.rootDirectory : getDirectoryAndName(projectPath).directoryPath
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
      try {
        const result = await promise
        this.internalProjects.set(params.projectId, {
          state: backend.ProjectState.opened,
          data: result,
        })
        return result
      } catch (error) {
        this.internalProjects.delete(params.projectId)
        throw error
      }
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
    const result = await this.sendRequest<CreateProject>('project/create', {
      missingComponentAction: MissingComponentAction.install,
      ...params,
    })
    const directoryPath = params.projectsDirectory ?? this.rootDirectory
    // Update `internalDirectories` by listing the project's parent directory, because the
    // directory name of the project is unknown. Deleting the directory is not an option because
    // that will prevent ALL descendants of the parent directory from being updated.
    await this.listDirectory(directoryPath)
    return result
  }

  /** Rename a project. */
  async renameProject(params: Omit<RenameProjectParams, 'projectsDirectory'>): Promise<void> {
    const path = this.internalProjectPaths.get(params.projectId)
    const directoryPath =
      path == null ? this.rootDirectory : getDirectoryAndName(path).directoryPath
    const fullParams: RenameProjectParams = { ...params, projectsDirectory: directoryPath }
    await this.sendRequest('project/rename', fullParams)
    const state = this.internalProjects.get(params.projectId)
    if (state?.state === backend.ProjectState.opened) {
      this.internalProjects.set(params.projectId, {
        state: state.state,
        data: { ...state.data, projectName: params.name },
      })
    }
    // Update `internalDirectories` by listing the project's parent directory, because the new
    // directory name of the project is unknown. Deleting the directory is not an option because
    // that will prevent ALL descendants of the parent directory from being updated.
    await this.listDirectory(directoryPath)
  }

  /** Duplicate a project. */
  async duplicateProject(
    params: Omit<DuplicateProjectParams, 'projectsDirectory'>,
  ): Promise<DuplicatedProject> {
    const path = this.internalProjectPaths.get(params.projectId)
    const directoryPath =
      path == null ? this.rootDirectory : getDirectoryAndName(path).directoryPath
    const fullParams: DuplicateProjectParams = { ...params, projectsDirectory: directoryPath }
    const result = this.sendRequest<DuplicatedProject>('project/duplicate', fullParams)
    // Update `internalDirectories` by listing the project's parent directory, because the
    // directory name of the project is unknown. Deleting the directory is not an option because
    // that will prevent ALL descendants of the parent directory from being updated.
    await this.listDirectory(directoryPath)
    return result
  }

  /** Delete a project. */
  async deleteProject(params: Omit<DeleteProjectParams, 'projectsDirectory'>): Promise<void> {
    const path = this.internalProjectPaths.get(params.projectId)
    const directoryPath =
      path == null ? this.rootDirectory : getDirectoryAndName(path).directoryPath
    const fullParams: DeleteProjectParams = { ...params, projectsDirectory: directoryPath }
    await this.sendRequest('project/delete', fullParams)
    this.internalProjectPaths.delete(params.projectId)
    this.internalProjects.delete(params.projectId)
    const siblings = this.internalDirectories.get(directoryPath)
    if (siblings != null) {
      this.internalDirectories.set(
        directoryPath,
        siblings.filter(
          (entry) =>
            entry.type !== FileSystemEntryType.ProjectEntry ||
            entry.metadata.id !== params.projectId,
        ),
      )
    }
  }

  /** List installed engine versions. */
  async listInstalledEngineVersions(): Promise<VersionList> {
    return await this.sendRequest<VersionList>('engine/list-installed', {})
  }

  /** List available engine versions. */
  async listAvailableEngineVersions(): Promise<VersionList> {
    return await this.sendRequest<VersionList>('engine/list-available', {})
  }

  /** Checks if a file or directory exists. */
  async exists(parentId: Path | null) {
    /** The type of the response body of this endpoint. */
    interface ResponseBody {
      readonly exists: boolean
    }
    const response = await this.runStandaloneCommand<ResponseBody>(
      null,
      'filesystem-exists',
      parentId ?? this.rootDirectory,
    )
    return response.exists
  }

  /** List directories, projects and files in the given folder. */
  async listDirectory(parentId: Path | null): Promise<readonly FileSystemEntry[]> {
    /** The type of the response body of this endpoint. */
    interface ResponseBody {
      readonly entries: FileSystemEntry[]
    }
    parentId ??= this.rootDirectory
    const response = await this.runStandaloneCommand<ResponseBody>(
      null,
      'filesystem-list',
      parentId,
    )
    const result = response.entries.map((entry) => ({
      ...entry,
      path: normalizeSlashes(entry.path),
    }))
    this.internalDirectories.set(parentId, result)
    for (const entry of result) {
      if (entry.type === FileSystemEntryType.ProjectEntry) {
        this.internalProjectPaths.set(entry.metadata.id, entry.path)
      }
    }
    return result
  }

  /** Create a directory. */
  async createDirectory(path: Path) {
    await this.runStandaloneCommand(null, 'filesystem-create-directory', path)
    this.internalDirectories.set(path, [])
    const directoryPath = getDirectoryAndName(path).directoryPath
    const siblings = this.internalDirectories.get(directoryPath)
    if (siblings) {
      const now = dateTime.toRfc3339(new Date())
      this.internalDirectories.set(directoryPath, [
        ...siblings.filter((sibling) => sibling.type === FileSystemEntryType.DirectoryEntry),
        {
          type: FileSystemEntryType.DirectoryEntry,
          attributes: {
            byteSize: 0,
            creationTime: now,
            lastAccessTime: now,
            lastModifiedTime: now,
          },
          path,
        },
        ...siblings.filter((sibling) => sibling.type !== FileSystemEntryType.DirectoryEntry),
      ])
    }
  }

  /** Create a file. */
  async createFile(path: Path, file: Blob) {
    await this.runStandaloneCommand(file, 'filesystem-write-path', path)
    const directoryPath = getDirectoryAndName(path).directoryPath
    const siblings = this.internalDirectories.get(directoryPath)
    if (siblings) {
      const now = dateTime.toRfc3339(new Date())
      this.internalDirectories.set(directoryPath, [
        ...siblings.filter((sibling) => sibling.type !== FileSystemEntryType.FileEntry),
        {
          type: FileSystemEntryType.FileEntry,
          attributes: {
            byteSize: file.size,
            creationTime: now,
            lastAccessTime: now,
            lastModifiedTime: now,
          },
          path,
        },
        ...siblings.filter((sibling) => sibling.type === FileSystemEntryType.FileEntry),
      ])
    }
  }

  /** Move a file or directory. */
  async moveFile(from: Path, to: Path) {
    await this.runStandaloneCommand(null, 'filesystem-move-from', from, '--filesystem-move-to', to)
    const children = this.internalDirectories.get(from)
    // Assume a directory needs to be loaded for its children to be loaded.
    if (children) {
      const moveChildren = (directoryChildren: readonly FileSystemEntry[]) => {
        for (const child of directoryChildren) {
          switch (child.type) {
            case FileSystemEntryType.DirectoryEntry: {
              const childChildren = this.internalDirectories.get(child.path)
              if (childChildren) {
                moveChildren(childChildren)
              }
              break
            }
            case FileSystemEntryType.ProjectEntry: {
              const path = this.internalProjectPaths.get(child.metadata.id)
              if (path != null) {
                this.internalProjectPaths.set(child.metadata.id, Path(path.replace(from, to)))
              }
              break
            }
            case FileSystemEntryType.FileEntry: {
              // No special extra metadata is stored for files.
              break
            }
          }
        }
        this.internalDirectories.set(
          from,
          children.map((child) => ({ ...child, path: Path(child.path.replace(from, to)) })),
        )
      }
      moveChildren(children)
    }
    const directoryPath = getDirectoryAndName(from).directoryPath
    const siblings = this.internalDirectories.get(directoryPath)
    if (siblings) {
      this.internalDirectories.set(
        directoryPath,
        siblings.filter((entry) => entry.path !== from),
      )
    }
  }

  /** Delete a file or directory. */
  async deleteFile(path: Path) {
    await this.runStandaloneCommand(null, 'filesystem-delete', path)
    const children = this.internalDirectories.get(path)
    // Assume a directory needs to be loaded for its children to be loaded.
    if (children) {
      const removeChildren = (directoryChildren: readonly FileSystemEntry[]) => {
        for (const child of directoryChildren) {
          switch (child.type) {
            case FileSystemEntryType.DirectoryEntry: {
              const childChildren = this.internalDirectories.get(child.path)
              if (childChildren) {
                removeChildren(childChildren)
              }
              break
            }
            case FileSystemEntryType.ProjectEntry: {
              this.internalProjects.delete(child.metadata.id)
              this.internalProjectPaths.delete(child.metadata.id)
              break
            }
            case FileSystemEntryType.FileEntry: {
              // No special extra metadata is stored for files.
              break
            }
          }
        }
      }
      removeChildren(children)
      this.internalDirectories.delete(path)
    }
    const directoryPath = getDirectoryAndName(path).directoryPath
    const siblings = this.internalDirectories.get(directoryPath)
    if (siblings) {
      this.internalDirectories.set(
        directoryPath,
        siblings.filter((entry) => entry.path !== path),
      )
    }
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
      },
    )
    // There is no way to avoid this as `JSON.parse` returns `any`.
    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
    const json: JSONRPCResponse<never> = await response.json()
    if ('result' in json) {
      return json.result
    } else {
      throw new Error(json.error.message)
    }
  }
}
