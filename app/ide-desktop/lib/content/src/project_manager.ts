/** @file This module defines the Project Manager endpoint. */
import * as utils from './utils'

const PROJECT_MANAGER_ENDPOINT = 'ws://127.0.0.1:30535'

// =============
// === Types ===
// =============

export enum MissingComponentAction {
    fail = 'Fail',
    install = 'Install',
    forceInstallBroken = 'ForceInstallBroken',
}

interface Result<T> {
    result: T
}

export type Uuid = utils.Brand<'Uuid'> & string
export type ProjectName = utils.Brand<'ProjectName'> & string
export type UTCDateTime = utils.Brand<'UTCDateTime'> & string

interface ProjectMetadata {
    name: ProjectName
    namespace: string
    id: Uuid
    engineVersion: string | undefined
    lastOpened: UTCDateTime | undefined
}

interface IpWithSocket {
    host: string
    port: number
}

interface ProjectList {
    projects: ProjectMetadata[]
}

interface CreateProject {
    projectId: Uuid
}

interface OpenProject {
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
    projectId: Uuid
    missingComponentAction: MissingComponentAction
}

export interface CloseProjectParams {
    projectId: Uuid
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
    projectId: Uuid
    name: ProjectName
}

export interface DeleteProjectParams {
    projectId: Uuid
}

export interface ListSamplesParams {
    projectId: Uuid
}

// =======================
// === Project Manager ===
// =======================

/** A WebSocket endpoint to the project manager. */
export class ProjectManager {
    constructor(protected readonly connectionUrl: string) {}

    static default() {
        return new ProjectManager(PROJECT_MANAGER_ENDPOINT)
    }

    public async sendRequest<T = void>(method: string, params: unknown): Promise<Result<T>> {
        const req = {
            jsonrpc: '2.0',
            id: 0,
            method,
            params,
        }

        const ws = new WebSocket(this.connectionUrl)
        return new Promise<Result<T>>((resolve, reject) => {
            ws.onopen = () => {
                ws.send(JSON.stringify(req))
            }
            ws.onmessage = event => {
                // There is no way to avoid this; `JSON.parse` returns `any`.
                // eslint-disable-next-line @typescript-eslint/no-unsafe-argument
                resolve(JSON.parse(event.data))
            }
            ws.onerror = error => {
                reject(error)
            }
        }).finally(() => {
            ws.close()
        })
    }

    /** * Open an existing project. */
    public async openProject(params: OpenProjectParams): Promise<Result<OpenProject>> {
        return this.sendRequest<OpenProject>('project/open', params)
    }

    /** * Close an open project. */
    public async closeProject(params: CloseProjectParams): Promise<Result<void>> {
        return this.sendRequest('project/close', params)
    }

    /** * Get the projects list, sorted by open time. */
    public async listProjects(params: ListProjectsParams): Promise<Result<ProjectList>> {
        return this.sendRequest<ProjectList>('project/list', params)
    }

    /** * Create a new project. */
    public async createProject(params: CreateProjectParams): Promise<Result<CreateProject>> {
        return this.sendRequest<CreateProject>('project/create', {
            missingComponentAction: MissingComponentAction.install,
            ...params,
        })
    }

    /** * Rename a project. */
    public async renameProject(params: RenameProjectParams): Promise<Result<void>> {
        return this.sendRequest('project/rename', params)
    }

    /** * Delete a project. */
    public async deleteProject(params: DeleteProjectParams): Promise<Result<void>> {
        return this.sendRequest('project/delete', params)
    }

    /** * Get the list of sample projects that are available to the user. */
    public async listSamples(params: ListSamplesParams): Promise<Result<ProjectList>> {
        return this.sendRequest<ProjectList>('project/listSample', params)
    }
}
