/// This module defines the Project Manager endpoint.

const PROJECT_MANAGER_ENDPOINT = 'ws://127.0.0.1:30535'

const MISSING_COMPONENT_ACTION_INSTALL = 'Install'

/**
 * A WebSocket endpoint to the project manager.
 */
class ProjectManager {
    protected readonly connection_url: string

    constructor(connection_url: string) {
        this.connection_url = connection_url
    }

    static default() {
        return new ProjectManager(PROJECT_MANAGER_ENDPOINT)
    }

    /**
     * Get the projects list.
     */
    listProjects(): any {
        const req = {
            jsonrpc: '2.0',
            id: 0,
            method: 'project/list',
            params: {},
        }

        const ws = new WebSocket(this.connection_url)
        return new Promise((resolve, reject) => {
            ws.onopen = () => {
                ws.send(JSON.stringify(req))
            }
            ws.onmessage = (event: any) => {
                resolve(JSON.parse(event.data))
            }
            ws.onerror = (error: any) => {
                reject(error)
            }
        }).finally(() => ws.close())
    }

    /**
     * Create an new project.
     *
     * @param name the project name
     * @param template the project template
     * @param action parameter specifying how to handle missing components
     */
    createProject(name: string, template?: string, action = MISSING_COMPONENT_ACTION_INSTALL): any {
        let params = {
            name: name,
            missingComponentAction: action,
        }
        if (template !== undefined) {
            // @ts-ignore
            params['projectTemplate'] = template
        }
        const req = {
            jsonrpc: '2.0',
            id: 0,
            method: 'project/create',
            params: params,
        }

        const ws = new WebSocket(this.connection_url)
        return new Promise((resolve, reject) => {
            ws.onopen = () => {
                ws.send(JSON.stringify(req))
            }
            ws.onmessage = event => {
                resolve(JSON.parse(event.data))
            }
            ws.onerror = error => {
                reject(error)
            }
        }).finally(() => ws.close())
    }
}

export { ProjectManager }
