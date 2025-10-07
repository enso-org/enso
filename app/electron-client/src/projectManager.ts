/** @file Project Manager bindings. */

import { net } from 'electron'
import * as url from 'node:url'

import * as paths from '@/paths'
import type { Options } from 'enso-common/src/options'
import { getProjectRoot } from 'project-manager-shim'
import { ProjectService } from 'project-manager-shim/projectService'


// =======================
// === Project Service ===
// =======================

let projectService: ProjectService | null = null
let extraArgs: string[] = []

/** Get the project service. */
export function getProjectService(): ProjectService {
  if (!projectService) {
    projectService = ProjectService.default(paths.RESOURCES_PATH, extraArgs)
  }
  return projectService
}

/** Setup the project service.*/
export function setupProjectService(args: string[]) {
  extraArgs = args
}

/** Get the Project Manager version. */
export async function version() {
  return await getProjectService().version()
}

/**
 * Handle requests to the `enso://` protocol.
 *
 * The protocol is used to fetch project assets from the backend.
 * If a given path is not inside a project, the request is rejected with a 403 error.
 */
export async function handleProjectProtocol(absolutePath: string) {
  if (getProjectRoot(absolutePath) == null) {
    console.error(`The given path is not inside a project: ${absolutePath}.`)
    return new Response(null, { status: 403 })
  }

  return net.fetch(url.pathToFileURL(absolutePath).toString())
}
