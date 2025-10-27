import {
  AssetType,
  extractTypeAndPath,
  extractTypeFromId,
  type PathResolveResponse,
} from 'enso-common/src/services/Backend'
import { EnsoPath } from 'enso-common/src/services/Backend/types'
import type { ProjectManager } from 'enso-common/src/services/ProjectManager/ProjectManager'
import type { Path, ProjectEntry, UUID } from 'enso-common/src/services/ProjectManager/types'
import type { RemoteBackend } from 'enso-common/src/services/RemoteBackend'
import { ProjectService } from './projectService'

/** Start a hybrid project. */
export async function startHybridProject(
  path: EnsoPath,
  projectManager: ProjectManager,
  remoteBackend: RemoteBackend,
  projectService = ProjectService.default(),
) {
  let project: ProjectEntry | undefined
  let asset: PathResolveResponse | undefined
  let projectId: UUID | undefined
  try {
    asset = await remoteBackend.resolveEnsoPath(path)
    const typeAndId = extractTypeFromId(asset.id)
    if (typeAndId.type !== AssetType.project) {
      throw new Error(`The path '${path}' does not point to a project.`)
    }
    const localProject = await remoteBackend.downloadProject(typeAndId.id)
    // @ts-expect-error `projectManager` is a private property that we need to access.
    const projectManager = localBackend.projectManager as ProjectManager
    let parentPath: Path | undefined
    for (const parentId of [localProject.parentId, localProject.projectRootId]) {
      parentPath = extractTypeAndPath(parentId).path
      const entries = await projectManager.listDirectory(parentPath)
      project = entries.filter((entry) => entry.type === 'ProjectEntry')[0]
      if (project) break
    }

    if (!project || !parentPath) {
      throw new Error('Downloaded cloud project does not exist in Local Backend.')
    }
    projectService.openProject(project.metadata.id, parentPath)
  } catch (error) {
    console.error(`Error starting hybrid project '${asset?.title ?? '(unknown)'}':`, error)
    if (projectId) {
      await projectService.closeProject(projectId)
    }
  }
}
