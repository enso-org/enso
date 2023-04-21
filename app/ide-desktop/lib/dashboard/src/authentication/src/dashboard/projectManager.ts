/** @file This module defines the Project Manager endpoint. */
import * as newtype from '../newtype'

// =================
// === Constants ===
// =================

const PROJECT_MANAGER_ENDPOINT = 'ws://127.0.0.1:30535'

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
export type UTCDateTime = newtype.Newtype<string, 'UTCDateTime'>

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

/** A WebSocket endpoint to the project manager. */
export class ProjectManager {
    static default = new ProjectManager(PROJECT_MANAGER_ENDPOINT)
    protected id = 0
    protected resolvers = new Map<number, (value: never) => void>()
    protected rejecters = new Map<number, (reason?: JSONRPCError) => void>()
    protected socket: WebSocket

    constructor(protected readonly connectionUrl: string) {
        const createSocket = () => {
            this.resolvers = new Map()
            const oldRejecters = this.rejecters
            this.rejecters = new Map()
            for (const reject of oldRejecters.values()) {
                reject()
            }
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
            socket.onerror = createSocket
            socket.onclose = createSocket
            this.socket = socket
            return socket
        }
        this.socket = createSocket()
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
        const id = this.id++
        this.socket.send(JSON.stringify({ jsonrpc: '2.0', id, method, params }))
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
