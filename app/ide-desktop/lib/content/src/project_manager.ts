/** @file This module defines the Project Manager endpoint. */

const PROJECT_MANAGER_ENDPOINT = 'ws://127.0.0.1:30535'

const MISSING_COMPONENT_ACTION_INSTALL = 'Install'

/** A WebSocket endpoint to the project manager. */
export class ProjectManager {
    constructor(protected readonly connectionUrl: string) {
        this.connectionUrl = connectionUrl
    }

    static default() {
        return new ProjectManager(PROJECT_MANAGER_ENDPOINT)
    }

    /** * Get the projects list. */
    listProjects(): unknown {
        const req = {
            jsonrpc: '2.0',
            id: 0,
            method: 'project/list',
            params: {},
        }

        const ws = new WebSocket(this.connectionUrl)
        return new Promise((resolve, reject) => {
            ws.onopen = () => {
                ws.send(JSON.stringify(req))
            }
            ws.onmessage = (event: MessageEvent<string>) => {
                resolve(JSON.parse(event.data))
            }
            ws.onerror = (error: unknown) => {
                reject(error)
            }
        }).finally(() => {
            ws.close()
        })
    }

    /** * Create a new project. */
    createProject(
        name: string,
        template?: string,
        action = MISSING_COMPONENT_ACTION_INSTALL
    ): unknown {
        const params = {
            name: name,
            missingComponentAction: action,
        }
        if (template !== undefined) {
            Object.assign(params, {
                projectTemplate: template,
            })
        }
        const req = {
            jsonrpc: '2.0',
            id: 0,
            method: 'project/create',
            params,
        }

        const ws = new WebSocket(this.connectionUrl)
        return new Promise((resolve, reject) => {
            ws.onopen = () => {
                ws.send(JSON.stringify(req))
            }
            ws.onmessage = (event: MessageEvent<string>) => {
                resolve(JSON.parse(event.data))
            }
            ws.onerror = error => {
                reject(error)
            }
        }).finally(() => {
            ws.close()
        })
    }
}
