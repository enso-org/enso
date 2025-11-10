import { PRODUCT_NAME } from 'enso-common/src/constants'
import {
  AssetType,
  extractTypeAndPath,
  extractTypeFromId,
  type PathResolveResponse,
} from 'enso-common/src/services/Backend'
import { EnsoPath } from 'enso-common/src/services/Backend/types'
import { Path, type ProjectEntry, type UUID } from 'enso-common/src/services/ProjectManager/types'
import type { RemoteBackend } from 'enso-common/src/services/RemoteBackend'
import { dirname } from 'node:path'
import { getFileSystemEntry } from './handler/index.js'
import { EnsoRunner, findEnsoExecutable } from './projectService/ensoRunner.js'
import { ProjectService } from './projectService/index.js'

declare module './projectService/ensoRunner.js' {
  interface ShutdownHookRegistry {
    'headless-project-execution': true
  }
}

function getWorkDir() {
  if (process.env.NODE_ENV === 'development') {
    return '../..'
  } else {
    return '.'
  }
}

function createRunnerAndService(): {
  readonly runner: EnsoRunner
  readonly projectService: ProjectService
} {
  const ensoPath = findEnsoExecutable(getWorkDir())
  if (!ensoPath) {
    throw new Error(`${PRODUCT_NAME} executable not found`)
  }
  const runner = new EnsoRunner(ensoPath)
  const projectService = new ProjectService(runner, [])
  return { runner, projectService }
}

/** Run a hybrid project by URL. */
export async function runHybridProjectByUrl(
  path: EnsoPath,
  remoteBackend: RemoteBackend,
): Promise<void> {
  let project: ProjectEntry | undefined
  let asset: PathResolveResponse | undefined
  let projectId: UUID | undefined
  const { projectService } = createRunnerAndService()
  try {
    asset = await remoteBackend.resolveEnsoPath(EnsoPath(decodeURIComponent(path)))
    const typeAndId = extractTypeFromId(asset.id)
    if (typeAndId.type !== AssetType.project) {
      throw new Error(`The path '${path}' does not point to a project.`)
    }
    const localProject = await remoteBackend.downloadProject(typeAndId.id)
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
  const { runner, projectService } = createRunnerAndService()
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
export async function runLocalProjectByPath(projectPath: Path): Promise<void> {
  const directoryId = Path(dirname(projectPath))
  const project = await getFileSystemEntry(projectPath)
  if (project.type !== 'ProjectEntry') {
    throw new Error(`The path '${projectPath}' does not point to a project.`)
  }
  await runLocalProjectByUuid(project.metadata.id as UUID, directoryId)
}
