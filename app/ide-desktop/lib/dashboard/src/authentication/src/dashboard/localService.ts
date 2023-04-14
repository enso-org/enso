/** @file Module containing the API client for the local backend API.
 *
 * Each exported function in the {@link Backend} in this module corresponds to an API endpoint. The
 * functions are asynchronous and return a `Promise` that resolves to the response from the API. */
import * as cloudService from './cloudService'
import * as newtype from '../newtype'
import * as projectManager from './projectManager'

// ===============
// === Backend ===
// ===============

export class Backend implements Partial<cloudService.Backend> {
    private readonly projectManager = projectManager.ProjectManager.default

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
            packageName: '',
            state: {
                type: cloudService.ProjectState.created,
            },
            address: newtype.asNewtype<cloudService.Address>(''),
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
            packageName: '',
            state: {
                type: cloudService.ProjectState.created,
            },
        }
    }

    async closeProject(projectId: cloudService.ProjectId): Promise<void> {
        await this.projectManager.closeProject({ projectId })
    }

    async getProjectDetails(projectId: cloudService.ProjectId): Promise<cloudService.Project> {
        // FIXME[sb]: It is a mistake for `getProjectDetails` to open a project,
        // however this appears to be unavoidable for now
        // due to the project manager having a different API.
        const result = await this.projectManager.openProject({
            projectId,
            missingComponentAction: projectManager.MissingComponentAction.install,
        })
        const project = result.result
        console.log(result, project)
        return {
            name: project.projectName,
            engineVersion: {
                lifecycle: cloudService.VersionLifecycle.stable,
                value: project.engineVersion,
            },
            ideVersion: {
                lifecycle: cloudService.VersionLifecycle.stable,
                value: project.engineVersion,
            },
            address: newtype.asNewtype<cloudService.Address>(''),
            organizationId: '',
            packageName: '',
            projectId,
            state: {
                type: cloudService.ProjectState.opened,
            },
        }
    }

    async openProject(projectId: cloudService.ProjectId): Promise<void> {
        await this.projectManager.openProject({
            projectId,
            missingComponentAction: projectManager.MissingComponentAction.install,
        })
    }
}

// =====================
// === createBackend ===
// =====================

/** Shorthand method for creating a new instance of the backend API. */
export function createBackend(): Backend {
    return new Backend()
}
