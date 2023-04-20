/** @file Module containing the API client for the local backend API.
 *
 * Each exported function in the {@link Backend} in this module corresponds to an API endpoint. The
 * functions are asynchronous and return a `Promise` that resolves to the response from the API. */
import * as cloudService from './cloudService'
import * as newtype from '../newtype'
import * as projectManager from './projectManager'

// =================
// === Constants ===
// =================

/** The scene name to open a project, instead of a debug scene. */
const PROJECT_SCENE_NAME = 'Project_root'

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
        return result.result.projects.map(project => ({
            type: cloudService.AssetType.project,
            title: project.name,
            id: project.id,
            parentId: '',
            permissions: [],
        }))
    }

    async listProjects(): Promise<cloudService.ListedProject[]> {
        const result = await this.projectManager.listProjects({})
        return result.result.projects.map(project => ({
            name: project.name,
            organizationId: '',
            projectId: project.id,
            packageName: PROJECT_SCENE_NAME,
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
        const result = await this.projectManager.createProject({
            name: newtype.asNewtype<projectManager.ProjectName>(body.projectName),
            projectTemplate: body.projectTemplateName ?? '',
            missingComponentAction: projectManager.MissingComponentAction.install,
        })
        return {
            name: body.projectName,
            organizationId: '',
            projectId: result.result.projectId,
            packageName: PROJECT_SCENE_NAME,
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
            const projects = await this.projectManager.listProjects({})
            const project = projects.result.projects.find(
                listedProject => listedProject.id === projectId
            )
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
                    packageName: PROJECT_SCENE_NAME,
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
                jsonAddress: newtype.asNewtype<cloudService.Address>(
                    project.languageServerJsonAddress.host +
                        ':' +
                        String(project.languageServerJsonAddress.port)
                ),
                binaryAddress: newtype.asNewtype<cloudService.Address>(
                    project.languageServerBinaryAddress.host +
                        ':' +
                        String(project.languageServerBinaryAddress.port)
                ),
                organizationId: '',
                packageName: PROJECT_SCENE_NAME,
                projectId,
                state: {
                    type: cloudService.ProjectState.opened,
                },
            })
        }
    }

    async openProject(projectId: cloudService.ProjectId): Promise<void> {
        const result = await this.projectManager.openProject({
            projectId,
            missingComponentAction: projectManager.MissingComponentAction.install,
        })
        this.currentlyOpenProject = {
            id: projectId,
            project: result.result,
        }
    }
}

// =====================
// === createBackend ===
// =====================

/** Shorthand method for creating a new instance of the backend API. */
export function createBackend(): Backend {
    return new Backend()
}
