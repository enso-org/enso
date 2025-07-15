/**
 * @file This module defines the Project Manager endpoint.
 * @see
 * https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-project-manager.md
 */
import invariant from 'tiny-invariant'

import * as backend from '#/services/Backend'
import { getDirectoryAndName, normalizeSlashes } from '#/utilities/path'
import * as dateTime from 'enso-common/src/utilities/data/dateTime'
import { getFileName } from '../../utilities/fileInfo'
import {
  MissingComponentAction,
  PROJECT_MANAGER_LOADING_FAILED_EVENT,
  type CloseProjectParams,
  type CreateProject,
  type CreateProjectParams,
  type DeleteProjectParams,
  type DuplicatedProject,
  type DuplicateProjectParams,
  type FileSystemEntry,
  type JSONRPCError,
  type JSONRPCResponse,
  type OpenProject,
  type OpenProjectParams,
  type Path,
  type ProjectState,
  type RenameProjectParams,
  type UUID,
  type VersionList,
} from './types'

/** Duration before the {@link ProjectManager} tries to create a WebSocket again. */
const RETRY_INTERVAL_MS = 1000
/** The maximum amount of time for which the {@link ProjectManager} should try loading. */
const MAXIMUM_DELAY_MS = 10_000

/**
 * A {@link WebSocket} endpoint to the project manager.
 *
 * It should always be in sync with the Rust interface at
 * `app/gui/controller/engine-protocol/src/project_manager.rs`.
 */
export class ProjectManager {
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
            document.dispatchEvent(new Event(PROJECT_MANAGER_LOADING_FAILED_EVENT))
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

  /**
   * Return the content of a file of the project.
   */
  async getFileContent(projectId: UUID, projectPath: string) {
    const path = this.internalProjectPaths.get(projectId)
    invariant(path, `Unknown project path for project '${projectId}'.`)
    return await this.runStandaloneCommand(null, 'filesystem-read-path', path + '/' + projectPath)
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
    const cached = this.internalProjects.get(params.projectId)
    if (cached && backend.IS_OPENING_OR_OPENED[cached.state]) {
      await this.closeProject({ projectId: params.projectId })
    }
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
          (entry) => entry.type !== 'ProjectEntry' || entry.metadata.id !== params.projectId,
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
    const response = await this.runStandaloneCommandJson<ResponseBody>(
      null,
      'filesystem-exists',
      parentId ?? this.rootDirectory,
    )
    return response.exists
  }

  /** List directories, projects and files in the given folder. */
  async listDirectory(parentPath: Path | null): Promise<readonly FileSystemEntry[]> {
    /** The type of the response body of this endpoint. */
    interface ResponseBody {
      readonly entries: FileSystemEntry[]
    }
    parentPath ??= this.rootDirectory
    const response = await this.runStandaloneCommandJson<ResponseBody>(
      null,
      'filesystem-list',
      parentPath,
    )
    const result = response.entries
      .filter((entry) => {
        // Ignore hybrid project directories.
        if (entry.type === 'DirectoryEntry') {
          const directoryName = getFileName(entry.path)
          return !backend.HYBRID_PROJECT_DIRECTORY_MASK.test(directoryName)
        }

        return true
      })
      .map((entry) => ({
        ...entry,
        path: normalizeSlashes(entry.path),
      }))

    this.internalDirectories.set(parentPath, result)

    for (const entry of result) {
      if (entry.type === 'ProjectEntry') {
        this.internalProjectPaths.set(entry.metadata.id, entry.path)
      }
    }
    return result
  }

  /** Create a directory. */
  async createDirectory(path: Path) {
    await this.runStandaloneCommandJson(null, 'filesystem-create-directory', path)
    this.internalDirectories.set(path, [])
    const directoryPath = getDirectoryAndName(path).directoryPath
    const siblings = this.internalDirectories.get(directoryPath)
    if (siblings) {
      const now = dateTime.toRfc3339(new Date())
      this.internalDirectories.set(directoryPath, [
        ...siblings.filter((sibling) => sibling.type === 'DirectoryEntry'),
        {
          type: 'DirectoryEntry',
          attributes: {
            byteSize: 0,
            creationTime: now,
            lastAccessTime: now,
            lastModifiedTime: now,
          },
          path,
        },
        ...siblings.filter((sibling) => sibling.type !== 'DirectoryEntry'),
      ])
    }
  }

  /** Create a file. */
  async createFile(path: Path, file: Blob) {
    await this.runStandaloneCommandJson(file, 'filesystem-write-path', path)
    const directoryPath = getDirectoryAndName(path).directoryPath
    const siblings = this.internalDirectories.get(directoryPath)
    if (siblings) {
      const now = dateTime.toRfc3339(new Date())
      this.internalDirectories.set(directoryPath, [
        ...siblings.filter((sibling) => sibling.type !== 'FileEntry'),
        {
          type: 'FileEntry',
          attributes: {
            byteSize: file.size,
            creationTime: now,
            lastAccessTime: now,
            lastModifiedTime: now,
          },
          path,
        },
        ...siblings.filter((sibling) => sibling.type === 'FileEntry'),
      ])
    }
  }

  /** Move a file or directory. */
  async moveFile(from: Path, to: Path) {
    await this.runStandaloneCommand(
      null,
      'filesystem-move-from',
      'json',
      from,
      '--filesystem-move-to',
      to,
    )
  }

  /** Delete a file or directory. */
  async deleteFile(path: Path) {
    await this.runStandaloneCommandJson(null, 'filesystem-delete', path)
    const children = this.internalDirectories.get(path)
    // Assume a directory needs to be loaded for its children to be loaded.
    if (children) {
      const removeChildren = (directoryChildren: readonly FileSystemEntry[]) => {
        for (const child of directoryChildren) {
          switch (child.type) {
            case 'DirectoryEntry': {
              const childChildren = this.internalDirectories.get(child.path)
              if (childChildren) {
                removeChildren(childChildren)
              }
              break
            }
            case 'ProjectEntry': {
              this.internalProjects.delete(child.metadata.id)
              this.internalProjectPaths.delete(child.metadata.id)
              break
            }
            case 'FileEntry': {
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
  private async runStandaloneCommand(
    body: BodyInit | null,
    name: string,
    ...cliArguments: string[]
  ): Promise<Response> {
    const searchParams = new URLSearchParams({
      ['cli-arguments']: JSON.stringify([`--${name}`, ...cliArguments]),
    })
    return await fetch(`/api/run-project-manager-command?${searchParams}`, { method: 'POST', body })
  }

  /** Run the Project Manager binary with the given command-line arguments, expecting JSON data of given type T. */
  private async runStandaloneCommandJson<T = void>(
    body: BodyInit | null,
    name: string,
    ...cliArguments: string[]
  ): Promise<T> {
    const response = await this.runStandaloneCommand(body, name, ...cliArguments)
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
