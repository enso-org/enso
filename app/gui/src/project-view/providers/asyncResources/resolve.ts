import type { OpenedProjectsStore } from '$/providers/openedProjects'
import { useProjectFiles } from '@/stores/projectFiles'
import { urlParse } from '@/util/url'
import {
  AssetType,
  EnsoPath,
  extractTypeFromId,
  NetworkError,
  ProjectId,
} from 'enso-common/src/services/Backend'
import { isLocalProjectId, LocalBackend } from 'enso-common/src/services/LocalBackend'
import { RemoteBackend } from 'enso-common/src/services/RemoteBackend'
import type { Opt } from 'enso-common/src/utilities/data/opt'
import { Err, Ok, rejectionToResult, type Result } from 'enso-common/src/utilities/data/result'
import { toValue } from 'vue'
import type { ResourceDefinition } from './AsyncResource'
import type { ResourceContext } from './context'
import { parseResourceUrl } from './parse'

/**
 * Create a resource resolver function that will use provided backends for accessing resource data.
 *
 * Part of 'asyncResources' store.
 * @internal
 */
export function useAsyncResourceResolver(
  backends: {
    localBackend: Opt<LocalBackend>
    remoteBackend: Opt<RemoteBackend>
  },
  openedProjects: OpenedProjectsStore,
) {
  function resolveResourceInContext(
    url: string,
    context: ResourceContext,
  ): Result<ResourceDefinition> {
    const parsedUrl = parseResourceUrl(url, context.basePathSegments)
    if (!parsedUrl.ok) return parsedUrl
    switch (parsedUrl.value.kind) {
      case 'ensoPath': {
        return Ok(resolveEnsoPathResource(parsedUrl.value.ensoPath))
      }
      case 'webUrl': {
        return Ok(resolveWebResource(parsedUrl.value.url))
      }
      case 'projectRelative': {
        {
          const project = toValue(context.project)
          if (project == null) return Err('Cannot resolve relative path outside of project')
          return Ok(
            resolveProjectResource(
              project,
              parsedUrl.value.relativePath,
              parsedUrl.value.uploading,
            ),
          )
        }
      }
    }
  }

  const catchNetworkError = rejectionToResult(NetworkError)

  function resolveEnsoPathResource(path: EnsoPath): ResourceDefinition {
    return {
      cacheKey: `ensoPath-${path}`,
      async fetch() {
        const cloudBackend = backends.remoteBackend
        if (cloudBackend == null)
          return Err('Cannot query enso path resource without cloud backend')
        const result = await catchNetworkError(cloudBackend.resolveEnsoPath(path))
        if (!result.ok) return result
        const asset = result.value
        const typedAsset = extractTypeFromId(asset.id)
        if (typedAsset.type != AssetType.file) return Err('Enso path does not point to a file')
        const details = await catchNetworkError(
          cloudBackend.getFileDetails(typedAsset.id, asset.title, true),
        )
        if (!details.ok) return details
        const url = details.value.url && urlParse(details.value.url)
        if (!url) return Err('Invalid asset URL provided')
        return Ok(url)
      },
    }
  }

  function resolveWebResource(url: URL): ResourceDefinition {
    return {
      cacheKey: `web-${url.href}`,
      async fetch() {
        return Ok(url)
      },
    }
  }

  // A never resolving promise that is used as a placeholder for "uploading" state when it is not
  // our client that is doing the upload.

  function resolveProjectResource(
    projectId: ProjectId,
    relativePath: string,
    uploading: boolean,
  ): ResourceDefinition {
    return {
      cacheKey: `projectRelative-${projectId}-${relativePath}`,
      // In case of resources during upload, resolve as if it is a remote client doing upload.
      // If this resource is uploaded by this client, the `uploading` field will be replaced with
      // real data promise inside `finishResourceUpload` function.
      uploading: uploading ? 'remote' : undefined,
      // Remote client will eventually remove the `?uploading` parameter from the resource URL, which
      // signals to all other clients that it is safe to fetch from remote server.
      onCacheHit(resource) {
        if (!uploading) resource.remoteUploadFinished()
      },
      async fetch(abort) {
        const openedProject = openedProjects.get(projectId)
        if (openedProject?.nextTask?.process === 'opening') {
          await openedProjects.waitForProcess(openedProject)
        }
        const initializedProject =
          openedProject?.state.status === 'initialized' ? openedProject.state : undefined
        if (initializedProject) {
          // Remote/local projects are treated the same when opened - contact LS for a file.
          const rootId = await initializedProject.store.projectRootId
          if (rootId == null) return Err('Could not identify project root')
          if (abort.aborted) return Err(abort)

          const projectFiles = useProjectFiles(initializedProject.store)
          return projectFiles.readFileBinary({ rootId, segments: relativePath.split('/') }, abort)
        } else {
          // project not opened
          if (isLocalProjectId(projectId)) {
            // unopened local project
            const localBackend = backends.localBackend
            if (!localBackend) return Err('Cannot query local resource without local backend')
            const response = await localBackend.resolveProjectAssetData(projectId, relativePath)
            if (abort.aborted) return Err(abort)
            return Ok(await response.blob())
          } else {
            // unopened remote project
            const cloudBackend = backends.remoteBackend
            if (cloudBackend == null)
              return Err('Cannot query cloud resource without cloud backend')
            try {
              const response = await cloudBackend.resolveProjectAssetData(
                projectId,
                relativePath,
                undefined,
                abort,
              )
              return Ok(await response.blob())
            } catch (e) {
              return Err(e)
            }
          }
        }
      },
    }
  }

  return resolveResourceInContext
}
