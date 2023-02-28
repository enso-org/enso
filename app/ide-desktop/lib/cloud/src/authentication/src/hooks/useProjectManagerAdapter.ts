/** @file Compatible with the desktop and the web client */
import { useMemo } from 'react'
import { ProjectManager } from 'enso-studio-content/src/project_manager'
import { createProject, listProjects, Project, ProjectState } from '../api'
import { useFullUserSession } from '../authentication'

interface ProjectManagerAdapter {
    createProject(projectName: string, projectTemplateName?: string): Promise<Project>
    listProjects(): Promise<Project[]>
}

const useProjectManagerDesktopClient = (
    projectManager?: ProjectManager
): ProjectManagerAdapter | null => {
    if (!projectManager) return null
    const { organization } = useFullUserSession()
    const baseOnEmptyProject = (
        project: Partial<Project> & Pick<Project, 'organizationId' | 'name' | 'projectId'>
    ): Project => ({
        state: { type: ProjectState.Created },
        packageName: 'Main',
        address: null,
        ami: null,
        ideVersion: null,
        engineVersion: null,
        ...project,
    })
    return {
        async createProject(projectName, projectTemplateName) {
            const createdProject = await projectManager.createProject(
                projectName,
                projectTemplateName
            )
            const project = baseOnEmptyProject({
                organizationId: organization.id,
                projectId: createdProject['result'].projectId,
                name: createdProject['result'].projectName,
            })
            return project
        },
        async listProjects() {
            /** TODO: move to a better place when refactoring */
            type Result = { name: string; id: string }
            const localProjects: Result[] = (await projectManager.listProjects())['result'][
                'projects'
            ]
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

const useProjectManagerWebClient = (): ProjectManagerAdapter => {
    const { accessToken } = useFullUserSession()
    return {
        async createProject(projectName, projectTemplateName) {
            const project = await createProject(accessToken, {
                projectName,
                projectTemplateName: projectTemplateName?.toLowerCase(),
            })
            return project
        },
        async listProjects() {
            return await listProjects(accessToken)
        },
    }
}

interface UseProjectManageAdapterOptions {
    runningOnDesktop: boolean
    projectManager?: ProjectManager
}
const useProjectManageAdapter = (
    options: UseProjectManageAdapterOptions
): ProjectManagerAdapter => {
    const { runningOnDesktop, projectManager } = options
    const projectManagerDesktopClient = useProjectManagerDesktopClient(projectManager)
    const projectManagerWebClient = useProjectManagerWebClient()
    const projectManagerAdapter = useMemo(() => {
        return runningOnDesktop ? projectManagerDesktopClient! : projectManagerWebClient
    }, [runningOnDesktop])

    return projectManagerAdapter
}

export { useProjectManageAdapter }
