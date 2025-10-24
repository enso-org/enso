import {
  AssetType,
  extractTypeFromId,
  type PathResolveResponse,
  type ProjectAsset,
} from 'enso-common/src/services/Backend'
import { EnsoPath } from 'enso-common/src/services/Backend/types'
import type { LocalBackend } from 'enso-common/src/services/LocalBackend'
import type { RemoteBackend } from 'enso-common/src/services/RemoteBackend'
import { ProjectService } from './projectService'

/** Start a hybrid project. */
export async function startHybridProject(
  path: EnsoPath,
  localBackend: LocalBackend,
  remoteBackend: RemoteBackend,
  projectService = ProjectService.default(),
) {
  let launchedProjectAsset: ProjectAsset | undefined
  let asset: PathResolveResponse | undefined
  try {
    asset = await remoteBackend.resolveEnsoPath(path)
    const typeAndId = extractTypeFromId(asset.id)
    if (typeAndId.type !== AssetType.project) {
      throw new Error(`The path '${path}' does not point to a project.`)
    }
    const localProject = await remoteBackend.downloadProject(typeAndId.id)
    let project
    for (const parentId of [localProject.parentId, localProject.projectRootId]) {
      const { assets } = await localBackend.listDirectory({
        parentId: parentId,
        filterBy: null,
        labels: null,
        sortExpression: null,
        sortDirection: null,
        from: null,
        pageSize: null,
        recentProjects: false,
      })
      project = assets.filter((item) => item.type === AssetType.project).at(0)
      if (project) {
        break
      }
    }

    if (!project) {
      throw new Error('Downloaded cloud project does not exist in Local Backend.')
    }
    launchedProjectAsset = project
    projectService.openProject(project.id, project.parentId)
  } catch (error) {
    console.error(`Error starting hybrid project '${asset?.title ?? '(unknown)'}':`, error)
    if (launchedProjectAsset) {
      await localBackend.closeProject(launchedProjectAsset.id, asset?.title ?? null)
    }
  }
}
