/** @file Module containing the API client for the local backend API.
 *
 * Each exported function in the {@link ProjectManagerBackendAPI} in this module corresponds to an API endpoint. The
 * functions are asynchronous and return a `Promise` that resolves to the response from the API. */
import * as backendApi from './cloudBackendApi'
import * as newtype from '../newtype'
import * as platformModule from '../platform'
import * as projectManager from './projectManager'

// ========================
// === Helper functions ===
// ========================

function ipWithSocketToAddress(ipWithSocket: projectManager.IpWithSocket) {
    return newtype.asNewtype<backendApi.Address>(`ws://${ipWithSocket.host}:${ipWithSocket.port}`)
}

// ===============
// === Backend ===
// ===============

/** The currently open project and its ID. */
interface CurrentlyOpenProjectInfo {
    id: projectManager.ProjectId
    project: projectManager.OpenProject
}

/** Class for sending requests to the Project Manager API endpoints.
 * This is used instead of the cloud backend API when managing local projects from the dashboard. */
export class ProjectManagerBackendAPI
    implements Partial<Omit<backendApi.CloudBackendAPI, 'platform'>>
{
    readonly platform = platformModule.Platform.desktop
    private readonly projectManager = projectManager.ProjectManager.default()
    private currentlyOpeningProjectId: string | null = null
    private currentlyOpenProject: CurrentlyOpenProjectInfo | null = null

    async listDirectory(): Promise<backendApi.Asset[]> {
        const result = await this.projectManager.listProjects({})
        return result.projects.map(project => ({
            type: backendApi.AssetType.project,
            title: project.name,
            id: project.id,
            parentId: '',
            permissions: [],
        }))
    }

    async listProjects(): Promise<backendApi.ListedProject[]> {
        const result = await this.projectManager.listProjects({})
        return result.projects.map(project => ({
            name: project.name,
            organizationId: '',
            projectId: project.id,
            packageName: project.name,
            state: {
                type: backendApi.ProjectState.created,
            },
            jsonAddress: null,
            binaryAddress: null,
        }))
    }

    async createProject(
        body: backendApi.CreateProjectRequestBody
    ): Promise<backendApi.CreatedProject> {
        const project = await this.projectManager.createProject({
            name: newtype.asNewtype<projectManager.ProjectName>(body.projectName),
            ...(body.projectTemplateName ? { projectTemplate: body.projectTemplateName } : {}),
            missingComponentAction: projectManager.MissingComponentAction.install,
        })
        return {
            name: body.projectName,
            organizationId: '',
            projectId: project.projectId,
            packageName: body.projectName,
            state: {
                type: backendApi.ProjectState.created,
            },
        }
    }

    async closeProject(projectId: backendApi.ProjectId): Promise<void> {
        await this.projectManager.closeProject({ projectId })
        this.currentlyOpenProject = null
    }

    async getProjectDetails(projectId: backendApi.ProjectId): Promise<backendApi.Project> {
        if (projectId !== this.currentlyOpenProject?.id) {
            const result = await this.projectManager.listProjects({})
            const project = result.projects.find(listedProject => listedProject.id === projectId)
            const engineVersion = project?.engineVersion
            if (project == null) {
                throw new Error(`The project ID '${projectId}' is invalid.`)
            } else if (engineVersion == null) {
                throw new Error(`The project '${projectId}' does not have an engine version.`)
            } else {
                return Promise.resolve<backendApi.Project>({
                    name: project.name,
                    engineVersion: {
                        lifecycle: backendApi.VersionLifecycle.stable,
                        value: engineVersion,
                    },
                    ideVersion: {
                        lifecycle: backendApi.VersionLifecycle.stable,
                        value: engineVersion,
                    },
                    jsonAddress: null,
                    binaryAddress: null,
                    organizationId: '',
                    packageName: project.name,
                    projectId,
                    state: {
                        type:
                            projectId === this.currentlyOpeningProjectId
                                ? backendApi.ProjectState.openInProgress
                                : backendApi.ProjectState.closed,
                    },
                })
            }
        } else {
            const project = this.currentlyOpenProject.project
            return Promise.resolve<backendApi.Project>({
                name: project.projectName,
                engineVersion: {
                    lifecycle: backendApi.VersionLifecycle.stable,
                    value: project.engineVersion,
                },
                ideVersion: {
                    lifecycle: backendApi.VersionLifecycle.stable,
                    value: project.engineVersion,
                },
                jsonAddress: ipWithSocketToAddress(project.languageServerJsonAddress),
                binaryAddress: ipWithSocketToAddress(project.languageServerBinaryAddress),
                organizationId: '',
                packageName: project.projectName,
                projectId,
                state: {
                    type: backendApi.ProjectState.opened,
                },
            })
        }
    }

    async openProject(projectId: backendApi.ProjectId): Promise<void> {
        this.currentlyOpeningProjectId = projectId
        const project = await this.projectManager.openProject({
            projectId,
            missingComponentAction: projectManager.MissingComponentAction.install,
        })
        this.currentlyOpenProject = { id: projectId, project }
    }
}
