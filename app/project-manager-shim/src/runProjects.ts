import { PRODUCT_NAME } from 'enso-common/src/constants'
import { AssetType, extractTypeAndPath, type ProjectAsset } from 'enso-common/src/services/Backend'
import { EnsoPath } from 'enso-common/src/services/Backend/types'
import { Path, type ProjectEntry, type UUID } from 'enso-common/src/services/ProjectManager/types'
import type { RemoteBackend } from 'enso-common/src/services/RemoteBackend'
import { dirname, resolve } from 'node:path'
import { getFileSystemEntry } from './handler/index.js'
import { EnsoRunner, findEnsoExecutable } from './projectService/ensoRunner.js'
import { ProjectService, type CloudParams } from './projectService/index.js'

function getWorkDir() {
  if (process.env.NODE_ENV === 'development') {
    return resolve('../..')
  } else {
    return '.'
  }
}

function createProjectService(): ProjectService {
  const ensoPath = findEnsoExecutable(getWorkDir())
  if (!ensoPath) {
    throw new Error(`${PRODUCT_NAME} executable not found`)
  }
  const runner = new EnsoRunner(ensoPath)
  return new ProjectService(runner, [])
}

/** Run a hybrid project by URL. */
export async function runHybridProjectByUrl(
  path: EnsoPath,
  remoteBackend: RemoteBackend,
): Promise<void> {
  let project: ProjectEntry | undefined
  let asset: ProjectAsset | undefined
  try {
    const unknownAsset = await remoteBackend.resolveEnsoPath(EnsoPath(decodeURIComponent(path)))
    if (unknownAsset.type !== AssetType.project) {
      throw new Error(`The path '${path}' does not point to a project.`)
    }
    asset = unknownAsset
    const cloudProjectSessionId = await remoteBackend.setHybridOpenInProgress(asset.id, asset.title)
    const localProject = await remoteBackend.downloadProject(asset.id)
    let parentPath: Path | undefined
    for (const projectId of [localProject.parentId, localProject.projectRootId]) {
      const projectPath = extractTypeAndPath(projectId).path
      parentPath = Path(dirname(projectPath))
      const entry = await getFileSystemEntry(projectPath)
      if (entry.type === 'ProjectEntry') {
        project = entry as ProjectEntry
        break
      }
    }

    if (!project || !parentPath) {
      throw new Error('Downloaded cloud project does not exist in Local Backend.')
    }
    const cloudProjectDirectoryPath = Path(asset.ensoPath.slice(0, asset.ensoPath.lastIndexOf('/')))
    await remoteBackend.setHybridOpened(asset.id, asset.title)
    await runLocalProjectByUuid(project.metadata.id, parentPath, {
      cloudProjectDirectoryPath,
      cloudProjectId: asset.id,
      cloudProjectSessionId,
    })
  } catch (error) {
    console.error(`Error starting hybrid project '${asset?.title ?? '(unknown)'}':`, error)
  } finally {
    if (asset) {
      await remoteBackend.closeProject(asset.id, asset.title)
    }
  }
}

/** Run a local project by UUID. */
export async function runLocalProjectByUuid(
  projectId: UUID,
  projectsDirectory: Path,
  cloudParams?: CloudParams,
): Promise<void> {
  const projectService = createProjectService()
  try {
    await projectService.runProject(projectId, projectsDirectory, cloudParams)
  } catch (error) {
    console.error(`Error starting local project '${projectId}':`, error)
    await projectService.closeProject(projectId)
    throw error
  }
}

/** Run a local project by path. */
export async function runLocalProjectByPath(projectPath: Path): Promise<void> {
  const directoryId = Path(dirname(projectPath))
  const project = await getFileSystemEntry(projectPath)
  if (project.type !== 'ProjectEntry') {
    throw new Error(`The path '${projectPath}' does not point to a project.`)
  }
  await runLocalProjectByUuid(project.metadata.id as UUID, directoryId)
}
