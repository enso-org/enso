import type { ProjectId } from 'enso-common/src/services/Backend'
import { mkdir, rm } from 'node:fs/promises'
import type { IncomingMessage } from 'node:http'
import * as https from 'node:https'
import * as path from 'node:path'
import { getProjectsDirectory, unpackBundle } from 'project-manager-shim'

/** Download a project from the cloud. */
export async function downloadCloudProject(downloadUrl: string, projectId: ProjectId) {
  const response = await new Promise<IncomingMessage>((resolve) => https.get(downloadUrl, resolve))
  const projectsDirectory = getProjectsDirectory()
  const parentDirectory = path.join(projectsDirectory, `cloud-${projectId}`)
  const projectRootDirectory = path.join(parentDirectory, 'project_root')

  await rm(parentDirectory, { recursive: true, force: true, maxRetries: 3 })
  await mkdir(projectRootDirectory, { recursive: true })
  await unpackBundle(response, projectRootDirectory)
  return { projectRootDirectory, parentDirectory }
}
