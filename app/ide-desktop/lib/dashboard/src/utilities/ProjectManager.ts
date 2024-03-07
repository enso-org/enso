/** @file This module defines the Project Manager endpoint.
 * @see
 * https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-project-manager.md */
import GLOBAL_CONFIG from '../../../../../gui2/config.yaml' assert { type: 'yaml' }
import type * as dateTime from './dateTime'
import * as newtype from './newtype'

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
type JSONRPCResponse<T> = JSONRPCErrorResponse | JSONRPCSuccessResponse<T>

// These are constructor functions that construct values of the type they are named after.
/* eslint-disable @typescript-eslint/no-redeclare */

// This intentionally has the same brand as in the cloud backend API.
/** An ID of a project. */
export type ProjectId = newtype.Newtype<string, 'ProjectId'>
/** Create a {@link ProjectId}. */
export const ProjectId = newtype.newtypeConstructor<ProjectId>()
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

/** Details for a project. */
export interface ProjectMetadata {
  readonly name: ProjectName
  readonly namespace: string
  readonly id: ProjectId
  readonly engineVersion: string | null
  readonly created: UTCDateTime
  readonly lastOpened: UTCDateTime | null
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
  readonly projectId: ProjectId
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

// ================================
// === Parameters for endpoints ===
// ================================

/** Parameters for the "open project" endpoint. */
export interface OpenProjectParams {
  readonly projectId: ProjectId
  readonly missingComponentAction: MissingComponentAction
}

/** Parameters for the "close project" endpoint. */
export interface CloseProjectParams {
  readonly projectId: ProjectId
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
}

/** Parameters for the "list samples" endpoint. */
export interface RenameProjectParams {
  readonly projectId: ProjectId
  readonly name: ProjectName
}

/** Parameters for the "delete project" endpoint. */
export interface DeleteProjectParams {
  readonly projectId: ProjectId
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
  private static instance: ProjectManager
  protected id = 0
  protected resolvers = new Map<number, (value: never) => void>()
  protected rejecters = new Map<number, (reason?: JSONRPCError) => void>()
  protected socketPromise: Promise<WebSocket>

  /** Create a {@link ProjectManager} */
  private constructor(protected readonly connectionUrl: string) {
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

  /** Lazy initialization for the singleton instance. */
  static default(projectManagerUrl: string | null) {
    // `this.instance` is initially undefined as an instance should only be created
    // if a `ProjectManager` is actually needed.
    // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
    return (this.instance ??= new ProjectManager(
      projectManagerUrl ?? GLOBAL_CONFIG.projectManagerEndpoint
    ))
  }

  /** Open an existing project. */
  public async openProject(params: OpenProjectParams): Promise<OpenProject> {
    return this.sendRequest<OpenProject>('project/open', params)
  }

  /** Close an open project. */
  public async closeProject(params: CloseProjectParams): Promise<void> {
    return this.sendRequest('project/close', params)
  }

  /** Get the projects list, sorted by open time. */
  public async listProjects(params: ListProjectsParams): Promise<ProjectList> {
    return this.sendRequest<ProjectList>('project/list', params)
  }

  /** Create a new project. */
  public async createProject(params: CreateProjectParams): Promise<CreateProject> {
    return this.sendRequest<CreateProject>('project/create', {
      missingComponentAction: MissingComponentAction.install,
      ...params,
    })
  }

  /** Rename a project. */
  public async renameProject(params: RenameProjectParams): Promise<void> {
    return this.sendRequest('project/rename', params)
  }

  /** Delete a project. */
  public async deleteProject(params: DeleteProjectParams): Promise<void> {
    return this.sendRequest('project/delete', params)
  }

  /** List installed engine versions. */
  public listInstalledEngineVersions(): Promise<VersionList> {
    return this.sendRequest<VersionList>('engine/list-installed', {})
  }

  /** List available engine versions. */
  public listAvailableEngineVersions(): Promise<VersionList> {
    return this.sendRequest<VersionList>('engine/list-available', {})
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
}
