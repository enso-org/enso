import {
  AssetType,
  extractTypeAndPath,
  extractTypeFromId,
  type PathResolveResponse,
} from 'enso-common/src/services/Backend'
import type { EnsoPath } from 'enso-common/src/services/Backend/types'
import type { ProjectManager } from 'enso-common/src/services/ProjectManager/ProjectManager'
import { Path, type ProjectEntry, type UUID } from 'enso-common/src/services/ProjectManager/types'
import type { RemoteBackend } from 'enso-common/src/services/RemoteBackend'
import { dirname } from 'path'
import { EnsoRunner, findEnsoExecutable } from './projectService/ensoRunner.js'
import { ProjectService } from './projectService/index.js'

declare module './projectService/ensoRunner.js' {
  interface ShutdownHookRegistry {
    'headless-project-execution': true
  }
}

/** Run a hybrid project by URL. */
export async function runHybridProjectByUrl(
  path: EnsoPath,
  projectManager: ProjectManager,
  remoteBackend: RemoteBackend,
): Promise<void> {
  let project: ProjectEntry | undefined
  let asset: PathResolveResponse | undefined
  let projectId: UUID | undefined
  const ensoPath = findEnsoExecutable('.')
  if (!ensoPath) {
    throw new Error('Enso executable not found')
  }
  const runner = new EnsoRunner(ensoPath)
  const projectService = new ProjectService(runner, [])
  try {
    asset = await remoteBackend.resolveEnsoPath(path)
    const typeAndId = extractTypeFromId(asset.id)
    if (typeAndId.type !== AssetType.project) {
      throw new Error(`The path '${path}' does not point to a project.`)
    }
    const localProject = await remoteBackend.downloadProject(typeAndId.id)
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
    await runLocalProjectByUuid(project.metadata.id, parentPath)
  } catch (error) {
    console.error(`Error starting hybrid project '${asset?.title ?? '(unknown)'}':`, error)
    if (projectId) {
      await projectService.closeProject(projectId)
    }
  }
}

/** Run a local project by UUID. */
export async function runLocalProjectByUuid(
  projectId: UUID,
  projectsDirectory: Path,
): Promise<void> {
  const ensoPath = findEnsoExecutable('.')
  if (!ensoPath) {
    throw new Error('Enso executable not found')
  }
  const runner = new EnsoRunner(ensoPath)
  const projectService = new ProjectService(runner, [])
  try {
    await projectService.openProject(projectId, projectsDirectory)
    return new Promise<void>((resolve) => {
      runner.registerShutdownHook(projectId, 'headless-project-execution', resolve)
    })
  } catch (error) {
    console.error(`Error starting local project '${projectId}':`, error)
    await projectService.closeProject(projectId)
    throw error
  }
}

/** Run a local project by path. */
export async function runLocalProjectByPath(
  projectPath: Path,
  projectManager: ProjectManager,
): Promise<void> {
  const directoryId = Path(dirname(projectPath))
  const entries = await projectManager.listDirectory(directoryId)
  const project = entries.find(
    (entry) => entry.type === 'ProjectEntry' && entry.path === projectPath,
  ) as ProjectEntry | undefined
  if (!project) {
    throw new Error(`Project at path '${projectPath}' not found in Local Backend.`)
  }
  await runLocalProjectByUuid(project.metadata.id, directoryId)
}
