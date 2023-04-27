/** @file This module defines the Project Manager endpoint. */
import * as newtype from './newtype'

const PROJECT_MANAGER_ENDPOINT = 'ws://127.0.0.1:30535'

// =============
// === Types ===
// =============

/** Possible actions to take when a component is missing. */
export enum MissingComponentAction {
    fail = 'Fail',
    install = 'Install',
    forceInstallBroken = 'ForceInstallBroken',
}

/** The return value of a JSON-RPC call. */
interface Result<T> {
    result: T
}

// This intentionally has the same brand as in the cloud backend API.
/** An ID of a project. */
export type ProjectId = newtype.Newtype<string, 'ProjectId'>
/** A name of a project. */
export type ProjectName = newtype.Newtype<string, 'ProjectName'>
/** A UTC value containing a date and a time. */
export type UTCDateTime = newtype.Newtype<string, 'UTCDateTime'>

/** Details for a project. */
interface ProjectMetadata {
    name: ProjectName
    namespace: string
    id: ProjectId
    engineVersion: string | null
    lastOpened: UTCDateTime | null
}

/** A value specifying a socket's hostname and port. */
interface IpWithSocket {
    host: string
    port: number
}

/** The return value of the "list projects" endpoint. */
interface ProjectList {
    projects: ProjectMetadata[]
}

/** The return value of the "create project" endpoint. */
interface CreateProject {
    projectId: ProjectId
}

/** The return value of the "open project" endpoint. */
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

/** Parameters for the "open project" endpoint. */
export interface OpenProjectParams {
    projectId: ProjectId
    missingComponentAction: MissingComponentAction
}

/** Parameters for the "close project" endpoint. */
export interface CloseProjectParams {
    projectId: ProjectId
}

/** Parameters for the "list projects" endpoint. */
export interface ListProjectsParams {
    numberOfProjects?: number
}

/** Parameters for the "create project" endpoint. */
export interface CreateProjectParams {
    name: ProjectName
    projectTemplate?: string
    version?: string
    missingComponentAction?: MissingComponentAction
}

/** Parameters for the "list samples" endpoint. */
export interface RenameProjectParams {
    projectId: ProjectId
    name: ProjectName
}

/** Parameters for the "delete project" endpoint. */
export interface DeleteProjectParams {
    projectId: ProjectId
}

/** Parameters for the "list samples" endpoint. */
export interface ListSamplesParams {
    projectId: ProjectId
}

// =======================
// === Project Manager ===
// =======================

/** A WebSocket endpoint to the Project Manager. */
export class ProjectManager {
    /** Creates a {@link ProjectManager}. */
    constructor(protected readonly connectionUrl: string) {}

    /** The returns the singleton instance of the {@link ProjectManager}. */
    static default() {
        return new ProjectManager(PROJECT_MANAGER_ENDPOINT)
    }

    /** Sends a JSON-RPC request to the WebSocket endpoint. */
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
