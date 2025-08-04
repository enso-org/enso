/**
 * @file This module defines the Project Manager endpoint.
 * @see
 * https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-project-manager.md
 */
import * as backend from '#/services/Backend'
import { getFileName, getFolderPath } from '#/utilities/fileInfo'
import { omit } from '#/utilities/object'
import { getDirectoryAndName, normalizeSlashes } from '#/utilities/path'
import { normalizeName } from '@/util/nameValidation'
import * as dateTime from 'enso-common/src/utilities/data/dateTime'
import invariant from 'tiny-invariant'
import {
  MissingComponentAction,
  Path,
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
  type ProjectState,
  type RenameProjectParams,
  type UUID,
} from './types'

/** Duration before the {@link ProjectManager} tries to create a WebSocket again. */
const RETRY_INTERVAL_MS = 1000
/** The maximum amount of time for which the {@link ProjectManager} should try loading. */
const MAXIMUM_DELAY_MS = 10_000

/** A project with its path provided instead of its id. */
type WithProjectPath<T> = Omit<T, 'projectId' | 'projectsDirectory'> & {
  readonly projectPath: Path
}

/**
 * A {@link WebSocket} endpoint to the project manager.
 *
 * It should always be in sync with the Rust interface at
 * `app/gui/controller/engine-protocol/src/project_manager.rs`.
 */
export class ProjectManager {
  // This is required so that projects get recursively updated (deleted, renamed or moved).
  private readonly directories = new Map<Path, readonly FileSystemEntry[]>()
  private readonly projects = new Map<UUID, ProjectState>()
  private readonly projectIds = new Map<Path, UUID>()
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

  /** Dispose of the {@link ProjectManager}. */
  async dispose() {
    const socket = await this.socketPromise
    socket.close()
  }

  /** Get the state of a project given its path. */
  getProjectId(projectPath: Path) {
    return this.projectIds.get(projectPath)
  }

  /** Get the state of a project given its path. */
  async getProject(projectPath: Path) {
    await this.listDirectory(Path(getFolderPath(projectPath)))
    const projectId = this.projectIds.get(projectPath)
    invariant(projectId, `Unknown project id for project '${projectPath}'.`)
    return this.projects.get(projectId)
  }

  /** Open an existing project. */
  async openProject(params: WithProjectPath<OpenProjectParams>): Promise<OpenProject> {
    const fullParams: OpenProjectParams = this.paramsWithPathToWithId(params)
    const cached = this.projects.get(fullParams.projectId)
    if (cached) {
      return cached.data
    } else {
      const promise = this.sendRequest<OpenProject>('project/open', fullParams)
      this.projects.set(fullParams.projectId, {
        state: backend.ProjectState.openInProgress,
        data: promise,
      })
      try {
        const result = await promise
        this.projects.set(fullParams.projectId, {
          state: backend.ProjectState.opened,
          data: result,
        })
        return result
      } catch (error) {
        this.projects.delete(fullParams.projectId)
        throw error
      }
    }
  }

  /** Close an open project. */
  async closeProject(params: WithProjectPath<CloseProjectParams>): Promise<void> {
    const id = this.projectIds.get(params.projectPath)
    const state = id != null ? this.projects.get(id) : null
    if (state?.state === backend.ProjectState.openInProgress) {
      // Projects that are not opened cannot be closed.
      // This is the only way to wait until the project is open.
      await this.openProject({
        projectPath: params.projectPath,
        missingComponentAction: MissingComponentAction.install,
      })
    }
    const fullParams: CloseProjectParams = this.paramsWithPathToWithId(params)
    this.projects.delete(fullParams.projectId)
    return this.sendRequest('project/close', fullParams)
  }

  /** Create a new project. */
  async createProject(params: CreateProjectParams): Promise<CreateProject> {
    const result = await this.sendRequest<Omit<CreateProject, 'projectPath'>>('project/create', {
      missingComponentAction: MissingComponentAction.install,
      ...params,
    })
    const directoryPath = params.projectsDirectory ?? this.rootDirectory
    // Update `internalDirectories` by listing the project's parent directory, because the
    // directory name of the project is unknown. Deleting the directory is not an option because
    // that will prevent ALL descendants of the parent directory from being updated.
    const siblings = await this.listDirectory(directoryPath)
    const projectEntry = siblings.find(
      (entry) => entry.type === 'ProjectEntry' && entry.metadata.id === result.projectId,
    )
    if (projectEntry == null) {
      throw new Error('Project failed to be created')
    }
    return { ...result, projectPath: projectEntry.path }
  }

  /** Return the content of a file of the project. */
  async getFileContent(projectPath: Path, pathInProject: string) {
    return await this.runStandaloneCommand(
      null,
      'filesystem-read-path',
      projectPath + '/' + pathInProject,
    )
  }

  /** Rename a project. */
  async renameProject(params: WithProjectPath<RenameProjectParams>): Promise<void> {
    const fullParams: RenameProjectParams = this.paramsWithPathToWithId(params)
    await this.sendRequest('project/rename', fullParams)
    const state = this.projects.get(fullParams.projectId)
    if (state?.state === backend.ProjectState.opened) {
      this.projects.set(fullParams.projectId, {
        state: state.state,
        data: {
          ...state.data,
          projectName: params.name,
          projectNormalizedName: normalizeName(params.name),
        },
      })
    }
    // Update `internalDirectories` by listing the project's parent directory, because the new
    // directory name of the project is unknown. Deleting the directory is not an option because
    // that will prevent ALL descendants of the parent directory from being updated.
    await this.listDirectory(fullParams.projectsDirectory)
  }

  /** Duplicate a project. */
  async duplicateProject(
    params: WithProjectPath<DuplicateProjectParams>,
  ): Promise<DuplicatedProject> {
    const fullParams: DuplicateProjectParams = this.paramsWithPathToWithId(params)
    const result = await this.sendRequest<Omit<DuplicatedProject, 'projectPath'>>(
      'project/duplicate',
      fullParams,
    )
    // Update `internalDirectories` by listing the project's parent directory, because the
    // directory name of the project is unknown. Deleting the directory is not an option because
    // that will prevent ALL descendants of the parent directory from being updated.
    const siblings = await this.listDirectory(fullParams.projectsDirectory)
    const projectEntry = siblings.find(
      (entry) => entry.type === 'ProjectEntry' && entry.metadata.id === result.projectId,
    )
    if (projectEntry == null) {
      throw new Error('Project failed to be created')
    }
    return { ...result, projectPath: projectEntry.path }
  }

  /** Delete a project. */
  async deleteProject(params: WithProjectPath<DeleteProjectParams>): Promise<void> {
    const fullParams: DeleteProjectParams = this.paramsWithPathToWithId(params)
    const cached = this.projects.get(fullParams.projectId)
    if (cached && backend.IS_OPENING_OR_OPENED[cached.state]) {
      await this.closeProject({ projectPath: params.projectPath })
    }
    await this.sendRequest('project/delete', fullParams)
    this.projectIds.delete(params.projectPath)
    this.projects.delete(fullParams.projectId)
    const siblings = this.directories.get(fullParams.projectsDirectory)
    if (siblings != null) {
      this.directories.set(
        fullParams.projectsDirectory,
        siblings.filter(
          (entry) => entry.type !== 'ProjectEntry' || entry.metadata.id !== fullParams.projectId,
        ),
      )
    }
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

    for (const entry of result) {
      if (entry.type === 'ProjectEntry') {
        this.projectIds.set(entry.path, entry.metadata.id)
      }
    }
    return result
  }

  /** Create a directory. */
  async createDirectory(path: Path) {
    await this.runStandaloneCommandJson(null, 'filesystem-create-directory', path)
    this.directories.set(path, [])
    const directoryPath = getDirectoryAndName(path).directoryPath
    const siblings = this.directories.get(directoryPath)
    if (siblings) {
      const now = dateTime.toRfc3339(new Date())
      this.directories.set(directoryPath, [
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
    const siblings = this.directories.get(directoryPath)
    if (siblings) {
      const now = dateTime.toRfc3339(new Date())
      this.directories.set(directoryPath, [
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
    await this.runStandaloneCommand(null, 'filesystem-move-from', from, '--filesystem-move-to', to)
  }

  /** Delete a file or directory. */
  async deleteFile(path: Path) {
    await this.runStandaloneCommandJson(null, 'filesystem-delete', path)
    const children = this.directories.get(path)
    // Assume a directory needs to be loaded for its children to be loaded.
    if (children) {
      const removeChildren = (directoryChildren: readonly FileSystemEntry[]) => {
        for (const child of directoryChildren) {
          switch (child.type) {
            case 'DirectoryEntry': {
              const childChildren = this.directories.get(child.path)
              if (childChildren) {
                removeChildren(childChildren)
              }
              break
            }
            case 'ProjectEntry': {
              this.projects.delete(child.metadata.id)
              this.projectIds.delete(child.path)
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
      this.directories.delete(path)
    }
    const directoryPath = getDirectoryAndName(path).directoryPath
    const siblings = this.directories.get(directoryPath)
    if (siblings) {
      this.directories.set(
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

  /**
   * Convert {@link WithProjectPath<T>} to `T`.
   * @throws {Error} when the `id` is not cached.
   */
  private paramsWithPathToWithId<T>(obj: WithProjectPath<T>) {
    const path = obj.projectPath
    const directoryPath = getDirectoryAndName(path).directoryPath
    const id = this.projectIds.get(path)
    if (id == null) {
      throw new Error(`Project with path '${path}' does not exist`)
    }
    return {
      ...omit(obj, 'projectPath'),
      projectId: id,
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

  /** Run the Project Manager binary with the given command-line arguments. */
  private async runStandaloneCommand(
    body: BodyInit | null,
    name: string,
    ...cliArguments: string[]
  ): Promise<Response> {
    const searchParams = new URLSearchParams({
      // eslint-disable-next-line @typescript-eslint/naming-convention
      'cli-arguments': JSON.stringify([`--${name}`, ...cliArguments]),
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
