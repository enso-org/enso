/** @file Make the projectManager support both desktop and web side. */
import { ProjectManager } from 'enso-studio-content/src/project_manager'
import * as auth from '../../authentication/providers/auth'
import { Project, ProjectState, useBackendService } from '../service'

// =============
// === Types ===
// =============

/** Temp declare ProjectManager.createProject returned value. */
interface CreateProjectResult {
    result: {
        projectId: string
        projectName: string
    }
}
/** Temp declare ProjectManager.createProject returned value. */
interface ListProjectResult {
    result: {
        projects: {
            id: string
            name: string
        }[]
    }
}

/** All platform adapter need suit to this interface. */
interface ProjectManagerAdapter {
    createProject(projectName: string, projectTemplateName?: string): Promise<Project>
    listProjects(): Promise<Project[]>
}

// ===============================
// === useProjectManageAdapter ===
// ===============================

// === useProjectManagerDesktopClient ===
/** Implement the project manager desktop side adapter(electron). */
const useProjectManagerDesktopClient = (
    projectManager?: ProjectManager
): ProjectManagerAdapter | null => {
    if (!projectManager) return null
    const { organization } = auth.useFullUserSession()
    const baseOnEmptyProject = (
        project: Partial<Project> & Pick<Project, 'organizationId' | 'name' | 'projectId'>
    ): Project => ({
        state: { type: ProjectState.created },
        packageName: 'Main',
        address: null,
        ami: null,
        ideVersion: null,
        engineVersion: null,
        ...project,
    })
    return {
        async createProject(projectName, projectTemplateName) {
            const createdProject = (await projectManager.createProject(
                projectName,
                projectTemplateName
            )) as CreateProjectResult
            const project = baseOnEmptyProject({
                organizationId: organization.id,
                projectId: createdProject.result.projectId,
                name: createdProject.result.projectName,
            })
            return project
        },
        async listProjects() {
            const localProjects = ((await projectManager.listProjects()) as ListProjectResult)
                .result.projects
            const projects = localProjects.map(p =>
                baseOnEmptyProject({
                    organizationId: organization.id,
                    projectId: p.id,
                    name: p.name,
                })
            )
            return projects
        },
    }
}

// === useProjectManagerWebClient ===
/** Implement the project manager web side adapter(web). */
const useProjectManagerWebClient = (): ProjectManagerAdapter => {
    const backendService = useBackendService()
    return {
        async createProject(projectName, projectTemplateName) {
            const project = await backendService.createProject({
                projectName,
                projectTemplateName: projectTemplateName?.toLowerCase(),
            })
            return project
        },
        async listProjects() {
            return await backendService.listProjects()
        },
    }
}

/** The options of useProjectManageAdapter. */
interface UseProjectManageAdapterOptions {
    /**
     * If true, this package is Electron desktop app,
     * else it is a standalone React app
     */
    runningOnDesktop: boolean
    projectManager?: ProjectManager
}

/** A React hook to use the adapter of the project manager.
 *
 * The purpose of this hook is to resolve platform differences in requests to the projectManager.
 */
const useProjectManageAdapter = (
    options: UseProjectManageAdapterOptions
): ProjectManagerAdapter => {
    const { runningOnDesktop, projectManager } = options
    const projectManagerDesktopClient = useProjectManagerDesktopClient(projectManager)
    const projectManagerWebClient = useProjectManagerWebClient()

    /**
     * If runningOnDesktop is true, Enso will automatically inject the projectManager,
     * so we can assert that projectManagerDesktopClient is not null.
     */
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    return runningOnDesktop ? projectManagerDesktopClient! : projectManagerWebClient
}

export { useProjectManageAdapter }
