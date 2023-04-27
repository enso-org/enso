/** @file Module containing the API client for the local backend API.
 *
 * Each exported function in the {@link Backend} in this module corresponds to an API endpoint. The
 * functions are asynchronous and return a `Promise` that resolves to the response from the API. */
import * as cloudService from './cloudService'
import * as newtype from '../newtype'
import * as projectManager from './projectManager'

// ========================
// === Helper functions ===
// ========================

function ipWithSocketToAddress(ipWithSocket: projectManager.IpWithSocket) {
    return newtype.asNewtype<cloudService.Address>(`ws://${ipWithSocket.host}:${ipWithSocket.port}`)
}

// ===============
// === Backend ===
// ===============

interface CurrentlyOpenProjectInfo {
    id: projectManager.ProjectId
    project: projectManager.OpenProject
}

export class Backend implements Partial<cloudService.Backend> {
    private readonly projectManager = projectManager.ProjectManager.default
    private currentlyOpenProject: CurrentlyOpenProjectInfo | null = null

    async listDirectory(): Promise<cloudService.Asset[]> {
        const result = await this.projectManager.listProjects({})
        return result.projects.map(project => ({
            type: cloudService.AssetType.project,
            title: project.name,
            id: project.id,
            parentId: '',
            permissions: [],
        }))
    }

    async listProjects(): Promise<cloudService.ListedProject[]> {
        const result = await this.projectManager.listProjects({})
        return result.projects.map(project => ({
            name: project.name,
            organizationId: '',
            projectId: project.id,
            packageName: project.name,
            state: {
                type: cloudService.ProjectState.created,
            },
            jsonAddress: null,
            binaryAddress: null,
        }))
    }

    async createProject(
        body: cloudService.CreateProjectRequestBody
    ): Promise<cloudService.CreatedProject> {
        const project = await this.projectManager.createProject({
            name: newtype.asNewtype<projectManager.ProjectName>(body.projectName),
            projectTemplate: body.projectTemplateName ?? '',
            missingComponentAction: projectManager.MissingComponentAction.install,
        })
        return {
            name: body.projectName,
            organizationId: '',
            projectId: project.projectId,
            packageName: body.projectName,
            state: {
                type: cloudService.ProjectState.created,
            },
        }
    }

    async closeProject(projectId: cloudService.ProjectId): Promise<void> {
        await this.projectManager.closeProject({ projectId })
        this.currentlyOpenProject = null
    }

    async getProjectDetails(projectId: cloudService.ProjectId): Promise<cloudService.Project> {
        if (projectId !== this.currentlyOpenProject?.id) {
            const result = await this.projectManager.listProjects({})
            const project = result.projects.find(listedProject => listedProject.id === projectId)
            const engineVersion = project?.engineVersion
            if (project == null) {
                throw new Error(`The project ID '${projectId}' is invalid.`)
            } else if (engineVersion == null) {
                throw new Error(`The project '${projectId}' does not have an engine version.`)
            } else {
                return Promise.resolve<cloudService.Project>({
                    name: project.name,
                    engineVersion: {
                        lifecycle: cloudService.VersionLifecycle.stable,
                        value: engineVersion,
                    },
                    ideVersion: {
                        lifecycle: cloudService.VersionLifecycle.stable,
                        value: engineVersion,
                    },
                    jsonAddress: null,
                    binaryAddress: null,
                    organizationId: '',
                    packageName: project.name,
                    projectId,
                    state: {
                        type: cloudService.ProjectState.closed,
                    },
                })
            }
        } else {
            const project = this.currentlyOpenProject.project
            return Promise.resolve<cloudService.Project>({
                name: project.projectName,
                engineVersion: {
                    lifecycle: cloudService.VersionLifecycle.stable,
                    value: project.engineVersion,
                },
                ideVersion: {
                    lifecycle: cloudService.VersionLifecycle.stable,
                    value: project.engineVersion,
                },
                jsonAddress: ipWithSocketToAddress(project.languageServerJsonAddress),
                binaryAddress: ipWithSocketToAddress(project.languageServerBinaryAddress),
                organizationId: '',
                packageName: project.projectName,
                projectId,
                state: {
                    type: cloudService.ProjectState.opened,
                },
            })
        }
    }

    async openProject(projectId: cloudService.ProjectId): Promise<void> {
        const project = await this.projectManager.openProject({
            projectId,
            missingComponentAction: projectManager.MissingComponentAction.install,
        })
        this.currentlyOpenProject = { id: projectId, project }
    }
}

// =====================
// === createBackend ===
// =====================

/** Shorthand method for creating a new instance of the backend API. */
export function createBackend(): Backend {
    return new Backend()
}
