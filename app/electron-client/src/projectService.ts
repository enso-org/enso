/** @file Project Manager Shim bindings. */

import { net } from 'electron'
import * as url from 'node:url'

import type { Electron } from '@/electron'
import * as paths from '@/paths'
import { getProjectRoot } from 'project-manager-shim'
import { ProjectService } from 'project-manager-shim/projectService'

let projectService: ProjectService | null = null
let extraArgs: readonly string[] = []

/** Get the project service. */
function getProjectService(electron: Electron | undefined, electronIsDev: boolean): ProjectService {
  if (!projectService) {
    projectService = ProjectService.default(paths.resourcesPath(electron, electronIsDev), extraArgs)
  }
  return projectService
}

/** Setup the project service.*/
export function setupProjectService(
  args: readonly string[],
  electron: Electron,
  electronIsDev: boolean,
): ProjectService {
  extraArgs = args
  if (!projectService) {
    projectService = ProjectService.default(paths.resourcesPath(electron, electronIsDev), args)
  }
  return projectService
}

/** Get the Project Manager version. */
export async function version(
  electron: Electron | undefined,
  electronIsDev: boolean,
): Promise<string> {
  return await getProjectService(electron, electronIsDev).version()
}

/**
 * Handle requests to the `enso://` protocol.
 *
 * The protocol is used to fetch project assets from the backend.
 * If a given path is not inside a project, the request is rejected with a 403 error.
 */
export async function handleProjectProtocol(absolutePath: string): Promise<Response> {
  if (getProjectRoot(absolutePath) == null) {
    console.error(`The given path is not inside a project: ${absolutePath}.`)
    return new Response(null, { status: 403 })
  }
  return net.fetch(url.pathToFileURL(absolutePath).toString())
}
