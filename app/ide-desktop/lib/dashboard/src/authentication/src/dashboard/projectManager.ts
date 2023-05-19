/** @file This module defines the Project Manager endpoint.
 *
 * It should always be in sync with the Rust interface at
 * `app/gui/controller/engine-protocol/src/project_manager.rs`. */
import * as newtype from '../newtype'

import GLOBAL_CONFIG from '../../../../../../../gui/config.yaml' assert { type: 'yaml' }

// =================
// === Constants ===
// =================

/** Duration before the {@link ProjectManager} tries to create a WebSocket again. */
const RETRY_INTERVAL_MS = 1000
/** Duration after which the {@link ProjectManager} stops re-trying to create a WebSocket. */
const STOP_TRYING_AFTER_MS = 10000

// =============
// === Types ===
// =============

export enum MissingComponentAction {
    fail = 'Fail',
    install = 'Install',
    forceInstallBroken = 'ForceInstallBroken',
}

interface JSONRPCError {
    code: number
    message: string
    data?: unknown
}

interface JSONRPCBaseResponse {
    jsonrpc: '2.0'
    id: number
}

interface JSONRPCSuccessResponse<T> extends JSONRPCBaseResponse {
    result: T
}

interface JSONRPCErrorResponse extends JSONRPCBaseResponse {
    error: JSONRPCError
}

type JSONRPCResponse<T> = JSONRPCErrorResponse | JSONRPCSuccessResponse<T>

// This intentionally has the same brand as in the cloud backend API.
export type ProjectId = newtype.Newtype<string, 'ProjectId'>
export type ProjectName = newtype.Newtype<string, 'ProjectName'>
/** The newtype's `TypeName` is intentionally different from the name of this type alias,
 * to match the backend's newtype. */
export type UTCDateTime = newtype.Newtype<string, 'Rfc3339DateTime'>

export interface ProjectMetadata {
    name: ProjectName
    namespace: string
    id: ProjectId
    engineVersion: string | null
    lastOpened: UTCDateTime | null
}

export interface IpWithSocket {
    host: string
    port: number
}

export interface ProjectList {
    projects: ProjectMetadata[]
}

export interface CreateProject {
    projectId: ProjectId
}

export interface OpenProject {
    engineVersion: string
    languageServerJsonAddress: IpWithSocket
    languageServerBinaryAddress: IpWithSocket
    projectName: ProjectName
    projectNamespace: string
}

// ================================
// === Parameters for endpoints ===
// ================================

export interface OpenProjectParams {
    projectId: ProjectId
    missingComponentAction: MissingComponentAction
}

export interface CloseProjectParams {
    projectId: ProjectId
}

export interface ListProjectsParams {
    numberOfProjects?: number
}

export interface CreateProjectParams {
    name: ProjectName
    projectTemplate?: string
    version?: string
    missingComponentAction?: MissingComponentAction
}

export interface RenameProjectParams {
    projectId: ProjectId
    name: ProjectName
}

export interface DeleteProjectParams {
    projectId: ProjectId
}

export interface ListSamplesParams {
    projectId: ProjectId
}

// =======================
// === Project Manager ===
// =======================

/** A {@link WebSocket} endpoint to the project manager.
 *
 * It should always be in sync with the Rust interface at
 * `app/gui/controller/engine-protocol/src/project_manager.rs`. */
export class ProjectManager {
    private static instance: ProjectManager
    protected id = 0
    protected resolvers = new Map<number, (value: never) => void>()
    protected rejecters = new Map<number, (reason?: JSONRPCError) => void>()
    protected socketPromise: Promise<WebSocket>

    /** Create a {@link ProjectManager} */
    private constructor(protected readonly connectionUrl: string) {
        const createSocket = () => {
            this.resolvers = new Map()
            const oldRejecters = this.rejecters
            this.rejecters = new Map()
            for (const reject of oldRejecters.values()) {
                reject()
            }
            this.socketPromise = new Promise<WebSocket>((resolve, reject) => {
                const handle = setInterval(() => {
                    try {
                        const socket = new WebSocket(this.connectionUrl)
                        clearInterval(handle)
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
                        socket.onerror = createSocket
                        socket.onclose = createSocket
                    } catch {
                        // Ignored; the `setInterval` will retry again eventually.
                    }
                }, RETRY_INTERVAL_MS)
                setTimeout(() => {
                    clearInterval(handle)
                    reject()
                }, STOP_TRYING_AFTER_MS)
            })
            return this.socketPromise
        }
        this.socketPromise = createSocket()
    }

    /** Lazy initialization for the singleton instance. */
    static default() {
        // `this.instance` is initially undefined as an instance should only be created
        // if a `ProjectManager` is actually needed.
        // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
        return (this.instance ??= new ProjectManager(GLOBAL_CONFIG.projectManagerEndpoint))
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

    /** Get the list of sample projects that are available to the user. */
    public async listSamples(params: ListSamplesParams): Promise<ProjectList> {
        return this.sendRequest<ProjectList>('project/listSample', params)
    }

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
